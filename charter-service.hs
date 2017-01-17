{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

-- the following are GHC extentions to Haskell2010
{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, OverloadedStrings
             , TypeOperators,
             TypeSynonymInstances #-}

-- base
import Control.Monad (when)
import Data.Maybe (fromJust, isNothing)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic)
import Prelude hiding (lookup)

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- aeson-pretty
import Data.Aeson.Encode.Pretty (encodePretty)

-- bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL8

-- containers
import Data.IntMap (IntMap, insert)
import qualified Data.IntMap as M

-- lens
import Control.Lens ((&), (?~), (.~))

-- mtl
import Control.Monad.Except (throwError)

-- servant
import Servant.API ((:>), (:<|>)(..), Capture, Get, JSON, Post, QueryParam,
                    ReqBody)
-- servant-server
import Servant.Server (Handler, Server, serve, errBody, err400)

-- servant-swagger
import Servant.Swagger (toSwagger)

-- stm
import Control.Concurrent.STM ( STM, TVar, atomically, modifyTVar, newTVar,
                                readTVar)

-- string-conversions
import Data.String.Conversions (convertString)

-- swagger2
import Data.Swagger (Swagger, ToSchema(declareNamedSchema)
                    , defaultSchemaOptions, description
                    , genericDeclareNamedSchema, info, title
                    , version)

-- transformers
import Control.Monad.IO.Class (liftIO)

-- wai
import Network.Wai (Application)

-- warp
import Network.Wai.Handler.Warp (run)

--------------------------------------------------------------------------------
-- constants

minutesPerHour :: Int
minutesPerHour = 60

--------------------------------------------------------------------------------
-- model

type TailNumber = String

data Plane = Plane { tailNum  :: TailNumber
                   , speedMph :: Double
                   , rangeMi  :: Double
                   } deriving (Generic, Show)

instance ToJSON Plane
instance FromJSON Plane


data FlightDuration = FlightDuration { flightDurationMin :: Double
                                     } deriving (Generic, Show)

instance ToJSON FlightDuration


data Pt = Pt { x :: Double
             , y :: Double
             } deriving Show

--------------------------------------------------------------------------------
-- thead-safe, non-persistent "database" built with an IntMap and STM
--
-- https://kseo.github.io/posts/2014-02-12-data-map-vs-data-intmap.html
-- https://www.schoolofhaskell.com/school/advanced-haskell/beautiful-concurrency/3-software-transactional-memory
-- https://wiki.haskell.org/Software_transactional_memory
-- https://en.wikipedia.org/wiki/Software_transactional_memory

-- a typesafe id, using a phatom type
type Id a = Int

type Table a = (Id a, IntMap a) -- fst is the next id to use in the table

data Entity a = Entity { iD     :: Id a
                       , entity :: a
                       } deriving (Generic, Show)


emptyTable :: Table a
emptyTable = (1, M.empty)

emptyTableSTM :: STM (TVar (Table a))
emptyTableSTM = newTVar emptyTable

emptyTableIO :: IO (TVar (Table a))
emptyTableIO = atomically emptyTableSTM

---

add :: a -> Table a -> Table a
add x (nextId, xs) = (nextId + 1, insert nextId x xs)

addSTM :: a -> TVar (Table a) -> STM Int
addSTM x xs = do
  modifyTVar xs (add x)
  iD <- fst <$> readTVar xs
  return $ iD - 1

addIO :: a -> TVar (Table a) -> IO Int
addIO x t = atomically $ addSTM x t

---

lookup :: Id a -> Table a -> Maybe a
lookup iD (_, xs) = M.lookup iD xs

lookupSTM :: Id a -> TVar (Table a) -> STM (Maybe a)
lookupSTM iD table = do
  xs <- readTVar table
  return $ lookup iD xs

lookupIO :: Id a -> TVar (Table a) -> IO (Maybe a)
lookupIO iD t = atomically $ lookupSTM iD t

---

assocs :: Table a -> [(Id a, a)]
assocs (_, xs) = M.assocs xs

assocsSTM :: TVar (Table a) -> STM [(Id a, a)]
assocsSTM t = assocs <$> readTVar t

assocsIO :: TVar (Table a) -> IO [(Id a, a)]
assocsIO = atomically . assocsSTM

--------------------------------------------------------------------------------
-- db

type PlaneEnt = Entity Plane
instance ToJSON PlaneEnt


--------------------------------------------------------------------------------
-- rest api specified as a type using Servant
--
-- http://haskell-servant.readthedocs.io/en/stable/

type PlaneAPI =
            -- get all of the planes
            "planes" :> Get '[JSON] [PlaneEnt]
            
            -- get the plane with the given id
       :<|> "planes" :> Capture "plane-id" Int :> Get '[JSON] (Maybe Plane)

            -- post a new plane into the system
       :<|> "planes" :> ReqBody '[JSON] Plane :> Post '[JSON] PlaneEnt

            -- compute the flight duration of a given plane between to
            -- points (x1,y2) and (x2, y2)
       :<|> "planes" :> "flightDuration" :> QueryParam "plane-id" Int
                                         :> QueryParam "x1" Double
                                         :> QueryParam "y1" Double
                                         :> QueryParam "x2" Double
                                         :> QueryParam "y2" Double
                                :> Get '[JSON] FlightDuration



--------------------------------------------------------------------------------
-- handlers

getPlanes :: TVar (Table Plane) -> Handler [PlaneEnt]
getPlanes t = do
  ps0 <- liftIO $ assocsIO t
  return $ map (\(iD, p) -> Entity iD p) ps0

getPlane :: TVar (Table Plane) -> Id Plane -> Handler (Maybe Plane)
getPlane t iD = liftIO $ lookupIO iD t

postPlane :: TVar (Table Plane) -> Plane -> Handler PlaneEnt
postPlane t p = do
  id <- liftIO $ addIO p t
  return $ Entity id p


computeFlightDuration :: TVar (Table Plane)
                      -> Maybe (Id Plane)
                      -> Maybe Double -> Maybe Double -- x1, y1
                      -> Maybe Double -> Maybe Double -- x2, y2
                      -> Handler FlightDuration

computeFlightDuration t mId mx1 my1 mx2 my2 = do

  assertGiven mId "plane-id"
  assertGiven mx1 "x1"
  assertGiven my1 "y1"
  assertGiven mx2 "x2"
  assertGiven my2 "y2"
  
  let
    iD = fromJust mId
    p1 = fromJust $ Pt <$> mx1 <*> my1
    p2 = fromJust $ Pt <$> mx2 <*> my2
  
  mPlane <- liftIO $ lookupIO iD t

  when (isNothing mPlane) $ throwError
    $ err400 {errBody = "plane " <> convertString (show iD) <> " not found\n"}

  let durHrs = flightDuration (fromJust mPlane) p1 p2 30  
  return $ FlightDuration  $ durHrs * fromIntegral minutesPerHour


assertGiven :: Maybe a -> ByteString -> Handler ()
assertGiven mx valStr =
  when (isNothing mx) $ throwError $ err400 {errBody = valStr <> " not given\n"}

--------------------------------------------------------------------------------
-- flight computation

dist :: Pt -> Pt -> Double
dist (Pt x1 y1) (Pt x2 y2) = sqrt $ dx*dx + dy*dy
  where
    dx = x2 - x1
    dy = y2 - y1


nTechStops :: Double -> Double -> Int
nTechStops dist range = n
  where
    r = dist / range
    fl = floor r
    ce = ceiling r

    n = if fl == ce
        then fl - 1
        else fl
    

flightDuration :: Plane
               -> Pt     -- ^ origin
               -> Pt     -- ^ destination
               -> Double -- ^ tech stop duration in hrs
               -> Double -- ^ (hrs)
               
flightDuration (Plane _ v r) p1 p2 techStopDur = techStopsDur + flightDur
  where
    d = dist p1 p2
    nts = nTechStops d r
    techStopsDur = fromIntegral nts * techStopDur
    flightDur = d / v



--------------------------------------------------------------------------------
-- server (built with servant + wai + warp)
--
-- http://www.yesodweb.com/book/web-application-interface
-- http://www.aosabook.org/en/posa/warp.html

charterServiceServer :: TVar (Table Plane) -> Server PlaneAPI

charterServiceServer t =      getPlanes t
                         :<|> getPlane t
                         :<|> postPlane t
                         :<|> computeFlightDuration t


server  :: TVar (Table Plane) -> Server API
server t = return charterServiceSwagger :<|> charterServiceServer t

planeAPI :: Proxy PlaneAPI
planeAPI = Proxy

api :: Proxy API
api = Proxy

app :: TVar (Table Plane) -> Application
app t = serve api $ server t



--------------------------------------------------------------------------------
-- swagger
-- http://swagger.io

-- | Swagger spec for charter-server

instance ToSchema FlightDuration where
    declareNamedSchema proxy =
      genericDeclareNamedSchema defaultSchemaOptions proxy

instance ToSchema Plane where
    declareNamedSchema proxy =
      genericDeclareNamedSchema defaultSchemaOptions proxy

instance ToSchema PlaneEnt where
    declareNamedSchema proxy =
      genericDeclareNamedSchema defaultSchemaOptions proxy

-- | API for serving @swagger.json@.
type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

-- | Combined API of a Todo service with Swagger documentation.
type API = SwaggerAPI :<|> PlaneAPI


charterServiceSwagger :: Swagger
charterServiceSwagger = toSwagger planeAPI
  & info.title .~ "CharterService API"
  & info.version .~ "1.0"
  & info.description ?~ "This is a toy api for jet charter operators"

-- | Output generated @swagger.json@ file for the @'CharterServiceAPI'@.
writeSwaggerJSON :: IO ()
writeSwaggerJSON = BL8.writeFile "charter-service.json"
                                 (encodePretty charterServiceSwagger)

--------------------------------------------------------------------------------
-- main

main :: IO ()
main = do
  writeSwaggerJSON -- should really be in a separate main
  let port = 8081
  planes <- emptyTableIO
  putStrLn $ "charter-server running on " ++ show port
  putStrLn $ "View the API docs with swagger-ui at http://<SERVER>:" ++ (show port) ++ "/swagger.json"
  run port $ app planes

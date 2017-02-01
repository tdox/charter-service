charter-service

A toy service for a jet charter company which satisfies the requirements of
this exercise (but using Haskell instead of Java)

------------------------

Write a REST API that performs some simple tasks related to a charter jet
business. This question requires a Java EE application server, specifically
Wildfly 8.x/9.x, TomEE 7.x, Glassfish 4.x, or Tomcat 8.x/9.x to run and test
the application. The deliverables are a zip file of your source code and a
deployable WAR file.

A charter jet business has several planes, each identified by a string and
having two important properties: it’s airspeed and range (the distance the
plane can fly on a tank of fuel).

The api should be able to do the following tasks:
* load planes into the system
* retrieve a plane from the system
* For a given plane, compute the time it takes to fly from one location to
  another (assuming that it takes 30 minutes for each refueling stop). For
  specifying locations, use a simple, flat 2 dimensional coordinate space.

There is no need to persist any data when the server stops.

------------------------

Instructions

These instructions assume you are using the terminal bash shell on Mac OS X.

Setup the build tools

 * If you don't already have it, install Homebrew (http://brew.sh)

 * Install "The Haskell Tool Stack"
   $ brew install haskell-stack
   (If not on a Mac, follow the "How to install" instructions here
   https://docs.haskellstack.org/en/stable/README/)

 * Install Haskell
   $ stack setup


Build the service
 $ stack build

Run the service
 $ stack exec charter-service

Make requests to the service
 In a separate shell:
 $ requests.bash

--------------------

API Documentation

REST API documetnation is automatically generated, served by the server and
viewable with the swagger-ui.

Instructions:

$ git clone https://github.com/swagger-api/swagger-ui.git

With your broswer, open swagger-ui/dist/index.html.
In the Explore field, enter "http://localhost:8081/swagger.json"

If you get a message like:

  "Can't read from server. It may not have the
  appropriate access-control-origin settings."
  
you can work around this issue in Chrome by installing the
Allow-Control-Allow-Origin plugin from here:

https://chrome.google.com/webstore/detail/allow-control-allow-origi/nlfbmbojpeacfghkpbjhddihlkkiljbi?hl=en

Alternatively, you may view the API docs by going to
http://editor.swagger.io/#/
File > Import File... > Choose File and select charter-service.json from this
directory.


------------------------

General comments on the code.

All of the code is in a single file, charter-service.hs.  In a real application,
the code would be split into several different directories and files as
indicated by the different sections in the single file.

If you are interested in learning more about the technologies used in the
application, see the web pages listed in the comments.


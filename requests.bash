#!/bin/bash

echo N339SM
curl -X POST -d '{"tailNum":"N399SM", "speedMph": 500, "rangeMi": 1000}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/planes
echo

echo
echo N101SM
curl -X POST -d '{"tailNum":"N101SM", "speedMph": 450, "rangeMi": 2000}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/planes
echo

echo
echo get 1
curl http://localhost:8081/planes/1
echo

echo
echo get 999
curl http://localhost:8081/planes/999
echo

echo
echo planes
curl http://localhost:8081/planes
echo


echo flightTime
curl 'http://localhost:8081/planes/flightDuration?plane-id=1&x1=0&y1=0&x2=500&y2=600'
echo

echo
echo negative tests
curl 'http://localhost:8081/planes/flightDuration?x1=0&y1=0&x2=500&y2=600'
curl 'http://localhost:8081/planes/flightDuration?plane-id=1&y1=0&x2=500&y2=600'
curl 'http://localhost:8081/planes/flightDuration?plane-id=1&x1=0&x2=500&y2=600'
curl 'http://localhost:8081/planes/flightDuration?plane-id=1&x1=0&y1=0&y2=600'
curl 'http://localhost:8081/planes/flightDuration?plane-id=1&x1=0&y1=0&x2=500'
curl 'http://localhost:8081/planes/flightDuration?plane-id=999&x1=0&y1=0&x2=500&y2=600'

echo ""
echo swagger.json
curl 'http://localhost:8081/swagger.json'
echo

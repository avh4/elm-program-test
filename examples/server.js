// This is a backend for the fictional HTTP services used in the Elm examples.

const express = require('express');

const port = process.env['PORT'] || 3000;

const app = express();

// Allow [CORS](https://en.wikipedia.org/wiki/Cross-origin_resource_sharing)
app.use(function(req, res, next) {
  res.header("Access-Control-Allow-Origin", "*");
  res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
  next();
});

// Print requsts to the console
app.use(function(req, res, next) {
  next();
  console.log(res.statusCode, req.method, req.url);
});

// Simulate a delay so we can see the loading states in the frontend
app.use(function(req, res, next) {
  setTimeout(next, 700);
});

app.use(express.json());


//
// /lighting_service/v1
//

const devices = [
  {
    id: "0feed",
    name: "Kitchen",
    dimmable: false,
    value: 0.0
  },
  {
    id: "aa901",
    name: "Foyer 1",
    dimmable: true,
    value: 0.0
  },
  {
    id: "aa902",
    name: "Foyer 2",
    dimmable: true,
    value: 0.8
  },
];

app.get('/lighting_service/v1/devices', (request, response) => {
  response.send(devices);
});

app.post('/lighting_service/v1/devices/:deviceId', (request, response) => {
  const deviceId = request.params.deviceId;

  for (var i = 0; i < devices.length; i++) {
    if (devices[i].id == deviceId) {
      devices[i].value = request.body.value;
      response.send(devices[i]);
      return;
    }
  }

  // We didn't find a matching deviceId
  response.send(404);
});


//
// Start the server
//

app.listen(port, (err) => {
  if (err) {
    return console.log('examples/server.js could not start', err);
  }

  console.log(`examples/server.js is listening on ${port}`);
});

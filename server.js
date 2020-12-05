const express = require("express");
const app = express();
const bodyParser = require("body-parser");
const restAPI = require("./api/rest-api.js")
const fs = require("fs");
const port = process.env.PORT || 3000;
const path = require('path');

app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: true }));
app.use(restAPI);
//app.use(express.static('www'));
app.use(express.static(path.resolve(__dirname, 'www/react/build')));

app.listen(port, (err)=> {
    if (err) return console.log(err);
    console.log("Starting on port " + port);
})

app.get('/', (req, res) => {
    res.sendFile(path.resolve(__dirname, 'www/react/build/index.html'));
})


app.get('/images/:id', (req, res) => {
        res.sendFile(path.resolve(__dirname, 'www/images/', req.params.id));
})

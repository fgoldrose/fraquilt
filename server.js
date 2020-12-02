const express = require("express");
const app = express();
const bodyParser = require("body-parser");
const restAPI = require("./api/rest-api.js")
const fs = require("fs");
const port = process.env.PORT || 3000;
const cors = require('cors');

app.use(cors());
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: true }));
app.use(restAPI);
app.use(express.static('www'));

app.listen(port, (err)=> {
    if (err) return console.log(err);
    console.log("Starting on port " + port);
})

/*
app.get('/images/:id', (req, res) => {
        if(fs.existsSync('./www./images/' + req.params.id)){
            res.sendFile('./www./images/' + req.params.id)
        }
        else{
            res.status(202);
            res.send('File being generated');
        }
    })
*/
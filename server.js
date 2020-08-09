const express = require("express");
const bodyParser = require("body-parser");
const app = express();
const fs = require("fs");
const {v4 : uuid} = require('uuid');
const frac = require('./frac');

app.listen(3000, ()=> {
    console.log("Starting on port 3000");

})
//app.use(express.static('images'));
var jsonParser = bodyParser.json()

app.get("/", (req, res) => {
    res.json(["Tony","Lisa","Michael","Ginger","Food"]);
});

app.post('/', jsonParser, function (req, res) {
    handleRequest(req);
    res.send('POST request to the homepage')
})

function handleRequest(req){
    var name = uuid();
    console.log(req.body);
    var options = req.body;
    options.name = name;
    fs.writeFile("images/" + name + ".json", JSON.stringify(req.body), (err) => {
        if (err) throw err;
        console.log('completed json file write');});
    frac.runFractal(options);
}

function makeImage(name, options){
    // for testing. will actually create the right image.
    var x = zeros([32, 32]);
    x.set(16, 16, 255);
    //console.log(x);
    var myFile = fs.createWriteStream("images/" + name + ".png");
    savePixels(x, "png").pipe(myFile);
}
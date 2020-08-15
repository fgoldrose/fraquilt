const express = require("express");
const router = express.Router();
const fs = require("fs");
const {v4 : uuid} = require('uuid');
const frac = require('./frac');
const {Worker, isMainThread, workerData} = require('worker_threads');
const Jimp = require('jimp');


router.post('/api', (req, res) => {
    const name = uuid();
    let options = req.body;
    options.name = name;

    /*
    fs.writeFile(`${__dirname}/../www/images/${name}.json`, JSON.stringify(req.body), (err) => {
        if (err) throw err;
        //console.log('completed json file write');
    });
    */

    let fractal = frac.runFractal(options);
    if(fractal == null){
        res.status(400);
        res.send("Request failed");
    }
    else{
        fractal.getBase64(Jimp.MIME_PNG, (err, img) => {
            res.status(200);
            res.json({'img' : img});});
    }
    /*
    const worker = new Worker(`${__dirname}/frac.js`,
    {
        workerData: options
    })
    */

    //res.json({'url' : `images/${name}.png`});
    
    
})

module.exports = router;
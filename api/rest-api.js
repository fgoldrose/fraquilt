const express = require("express");
const router = express.Router();
const fs = require("fs");
const {v4 : uuid} = require('uuid');
const frac = require('./frac');
const {Worker, isMainThread, workerData} = require('worker_threads');


router.post('/api', (req, res) => {
    const name = uuid();
    let options = req.body;
    options.name = name;

    fs.writeFile(`${__dirname}/../www/images/${name}.json`, JSON.stringify(req.body), (err) => {
        if (err) throw err;
        //console.log('completed json file write');
    });

    const worker = new Worker(`${__dirname}/frac.js`,
    {
        workerData: options
    })

    res.json({'url' : `images/${name}.png`});
    
})

module.exports = router;
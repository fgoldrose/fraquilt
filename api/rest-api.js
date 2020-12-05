const express = require("express");
const router = express.Router();
const fs = require("fs");
const {v4 : uuid} = require('uuid');
const frac = require('./frac');
const {Worker, isMainThread, workerData} = require('worker_threads');
const Jimp = require('jimp');
const aws = require('aws-sdk');
const S3_BUCKET = process.env.S3_BUCKET_NAME;
const GIFEncoder = require('gifencoder')
const { execFile } = require('child_process');
const path = require('path');
//const randomfuncs = require('../www/randomfuncs');
const port = process.env.PORT || 3000;
aws.config.region='us-east-2';

const local = false;

function writeS3(res, options, data){
    const s3 = new aws.S3();

    const s3Paramsjson = {
        Bucket: S3_BUCKET,
        Key: options.name + ".json",
        Body: JSON.stringify(options),
        ContentType: "text/json",
        ACL:'public-read'
    }

    const s3Paramsimg = {
        Bucket: S3_BUCKET,
        Key: options.name + ".png",
        Body: data,
        ContentType: "image/png",
        ACL:'public-read'
    }

    s3.upload(s3Paramsimg, (err, data) =>{
        if (err) throw err;
    });
    s3.upload(s3Paramsjson, (err, data) =>{
        if (err) throw err;
    });

    res.status(200);
        res.json({'url' : `https://fraquilt.s3.amazonaws.com/${options.name}.png`
                , 'image': "data:image/png;base64, " + data.toString('base64')});
        res.end();
}


router.get('/', (req, res) => {
    res.sendFile(path.resolve(`${__dirname}/../www/react/build/index.html`));
})

router.post('/api', (req, res) => {
    const name = uuid();
    let options = req.body;
    options.name = name;

    let fractal = frac.runFractal(options);

    if(fractal == null){
        res.status(400);
        res.send("Request failed");
    }
    else{
        let width = Math.pow(options.functions.length, options.iterations);
        let height = Math.pow(options.functions[0].length, options.iterations);
        new Jimp({data: fractal, width, height}, (err, image) => {
            if (err) throw err;

            if(local){
                fs.writeFile(`${__dirname}/../www/images/${options.name}.json`, JSON.stringify(req.body), (err) => {
                    if (err) throw err;
                });
                image.write(`${__dirname}/../www/images/${options.name}.png`);
                res.status(200);
                res.json({'url': `http://localhost:${port}/images/${options.name}.png`});
            }
            else{
                image.getBuffer(Jimp.MIME_PNG, (err, img) => {
                if (err) throw err;
                writeS3(res, options, img);
            })
            }
        });
    }
})



module.exports = router;

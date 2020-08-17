const express = require("express");
const router = express.Router();
const fs = require("fs");
const {v4 : uuid} = require('uuid');
const frac = require('./frac');
const {Worker, isMainThread, workerData} = require('worker_threads');
const Jimp = require('jimp');
const aws = require('aws-sdk');
const S3_BUCKET = process.env.S3_BUCKET_NAME;

aws.config.region='us-east-2';

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
        fractal.getBuffer(Jimp.MIME_PNG, (err, img) => {
            const s3 = new aws.S3();
            const s3Paramsjson = {
                Bucket: S3_BUCKET,
                Key: name + ".json",
                Body: JSON.stringify(options),
                ContentType: "application/json"
            }

            const s3Paramsimg = {
                Bucket: S3_BUCKET,
                Key: name + ".png",
                Body: img,
                ContentType: "image/png",
                ACL:'public-read'
            }
            s3.upload(s3Paramsjson, (err, data) =>{
                if (err) throw err;
            });
            s3.upload(s3Paramsimg, (err, data) =>{
                if (err) throw err;
            });

            res.status(200);
            res.json({'url' : `https://fraquilt.s3.us-east-2.amazonaws.com/${name}.png`});});
    }
})

module.exports = router;
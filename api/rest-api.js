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

const local = true;

function writeS3(options, data){
    const s3 = new aws.S3();
    const s3Paramsjson = {
        Bucket: S3_BUCKET,
        Key: options.name + ".json",
        Body: JSON.stringify(options),
        ContentType: "application/json",
        ACL:'public-read'
    }

    const s3Paramsimg = {
        Bucket: S3_BUCKET,
        Key: options.name + ".png",
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
    res.json({'url' : `https://fraquilt.s3.us-east-2.amazonaws.com/${options.name}.png`});
}

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
                res.json({'url': `http://localhost:${process.env.PORT}/images/${options.name}.png`});
            }
            else{
                image.getBuffer(Jimp.MIME_PNG, (err, img) => {
                if (err) throw err;
                writeS3(options, img);
            })
            }
        });
    }
})

module.exports = router;

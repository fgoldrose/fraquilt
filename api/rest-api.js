const express = require("express");
const router = express.Router();
const fs = require("fs");
const {v4 : uuid} = require('uuid');
const frac = require('./frac');
const fracb = require('./fracb');
const {Worker, isMainThread, workerData} = require('worker_threads');
const Jimp = require('jimp');
const aws = require('aws-sdk');
const S3_BUCKET = process.env.S3_BUCKET_NAME;
const GIFEncoder = require('gifencoder')
const { execFile } = require('child_process');

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
                res.json({'url': `${__dirname}/../www/images/${options.name}.png`});
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


router.post('/api/funcs', (req, res) => {
    const name = uuid();
    let options = req.body;
    options.name = name;

    let fractal = fracb.runFractal(options);
    console.log("got fractal")
    
    if(fractal == null){
        res.status(400);
        res.send("Request failed");
    }
    else{
        let width = Math.pow(options.functions.length, options.iterations);
        let height = Math.pow(options.functions[0].length, options.iterations);

        Jimp.read(`${__dirname}/../www/images/baseimage.png`, (err, image) => {
            if (err) throw err;
            image = image.resize(width, height);

            image.scan(0, 0, image.bitmap.width, image.bitmap.height, (x,y,idx) =>{
                let pix = Jimp.intToRGBA(image.getPixelColor(x, y));
                let col = fractal[y*width + x]([[pix.r, pix.g, pix.b]])[0];

                image.bitmap.data[idx] = col[0];
                image.bitmap.data[idx+1] = col[1];
                image.bitmap.data[idx+2] = col[2];
                image.bitmap.data[idx+3] = 255;

                if (x == image.bitmap.width - 1 && y == image.bitmap.height - 1) {
                    if(local){
                        image.write(`${__dirname}/../www/images/${options.name}.png`);
                        fs.writeFile(`${__dirname}/../www/images/${options.name}.json`, JSON.stringify(req.body), (err) => {
                            if (err) throw err;
                        });
                        res.status(200);
                        res.json({'url': `${__dirname}/../www/images/${options.name}.png`});
                    }
                    else{
                        image.getBuffer(Jimp.MIME_PNG, (err, img) => {
                        if (err) throw err;
                        writeS3(options, img);
                    })
                    }
                }
            })
        });
    }
})


router.post('/api/funcsgif', (req, res) => {
    const name = uuid();
    let options = req.body;
    options.name = name;

    let fractal = fracb.runFractal(options);
    console.log("got fractal")
    
    if(fractal == null){
        res.status(400);
        res.send("Request failed");
    }
    else{
        let width = Math.pow(options.functions.length, options.iterations);
        let height = Math.pow(options.functions[0].length, options.iterations);

        //Jimp.read(`${__dirname}/../www/images/baseimage.png`, (err, image) => {
        new Jimp(width, height, (err, image) => {
            if (err) throw err;
            image = image.resize(width, height);

            const encoder = new GIFEncoder(width, height);
            encoder.createReadStream().pipe(fs.createWriteStream(`${__dirname}/../www/images/${name}.gif`))
            encoder.start();
            encoder.setRepeat(0);
            encoder.setDelay(120);
            encoder.setQuality(10);

            let numframes = options.numframes || 10;

            for(let n = 0; n < numframes; n++){
                image.scan(0, 0, image.bitmap.width, image.bitmap.height, (x,y,idx) =>{
                    let col;
                    if(n==0){
                        col = fractal[y*width + x](options.colors)[0];
                    }
                    else{
                        let pix = Jimp.intToRGBA(image.getPixelColor(x, y));
                        col = fractal[y*width + x]([[pix.r, pix.g, pix.b]])[0];
                    }

                    image.bitmap.data[idx] = col[0];
                    image.bitmap.data[idx+1] = col[1];
                    image.bitmap.data[idx+2] = col[2];
                    image.bitmap.data[idx+3] = 255;

                    if (x == image.bitmap.width - 1 && y == image.bitmap.height - 1) {
                        console.log(n);
                        encoder.addFrame(image.bitmap.data);
                    }
                })

                if(n == numframes-1){
                    encoder.finish();
                    res.status(200);
                    res.json({'url': `${__dirname}/../www/images/${name}.gif`});
                }
            }
            
        });
    }
})

function changeOptionsRandom(options){
    let width = options.functions.length;
    let height = options.functions[0].length;
    let numcolors = options.colors.length;
    let c = Math.floor(Math.random() * numcolors);
    let comp = Math.floor(Math.random() * 3);

    if(Math.random() < 0){
        if (Math.random() < 0.5){
            options.colors[c][comp] = options.colors[c][comp] + 1
        }
        else{
            options.colors[c][comp] = options.colors[c][comp] - 1
        }
    }

    let w = Math.floor(Math.random() * width);
    let h = Math.floor(Math.random() * height);
    
    let addon;
    let rand = Math.random();
    let num = (Math.floor(Math.random() * 5) + 2)
    //num = 2
    if(Math.random() < .5){
        addon = "*" + num.toString() + "/" + (num - 1).toString()
    }
    else {
        addon = "/" + num + "*" + (num - 1).toString()
    }
/*
    if(rand < 0.05){
        addon = addon + "+1"
    }
    else if(rand > 0.95){
        addon = addon + "-1"
    }
*/

    /*
    else if (rand > .25){
        addon = "*" + num + "/10"//"*9/10"
    }
    else{
        addon = "/" + num + "*10"//"*10/9"
    }*/
    options.functions[w][h][c][comp] = options.functions[w][h][c][comp] + addon;
}

router.post('/api/gif', (req, res) => {
    const name = uuid();
    let options = req.body;
    options.name = name;
    let width = Math.pow(options.functions.length, options.iterations);
    let height = Math.pow(options.functions[0].length, options.iterations);
    
    const encoder = new GIFEncoder(width, height);
    encoder.createReadStream().pipe(fs.createWriteStream(`${__dirname}/../www/images/${name}.gif`))
    encoder.start();
    encoder.setRepeat(0);
    encoder.setDelay(120);
    encoder.setQuality(10);

    console.log(options);

    let numframes = options.numframes || 20;

    let fractaldata;

    for(let i = 0; i < numframes; i++){
        console.log(i)
        changeOptionsRandom(options);
        fractaldata = frac.runFractal(options);
        encoder.addFrame(fractaldata);
    }
    console.log(name);

    encoder.finish();
    res.status(200);
    res.json({'url': `${__dirname}/../www/images/${name}.gif`});
})

module.exports = router;

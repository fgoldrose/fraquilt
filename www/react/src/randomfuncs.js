
// Allow code to be used in browser and node
if(typeof exports == 'undefined'){
    var exports = {};
}


function hexToRGB(hex) {
    var bigint = parseInt(hex.slice(1), 16);
    var r = (bigint >> 16) & 255;
    var g = (bigint >> 8) & 255;
    var b = bigint & 255;

    return [r,g,b];
}

function randVal(numcols, likelynum, likelycontinue, numfrom, numto){
    let n = Math.random();
    let ret;
    if(n < likelynum){
        ret = (Math.floor(Math.random() * (numto-numfrom)) + numfrom).toString(10)
    }
    else{
        let c = Math.floor(Math.random() * 3)
        let i = Math.floor(Math.random() * numcols);
        if(c == 0){
            ret = "r" + i.toString(10)
        }
        else if(c == 1){
            ret = "g" + i.toString(10)
        }
        else {
            ret = "b" + i.toString(10)
        }
    }

    if(Math.random() < likelycontinue){
        ret += randOp(numcols);
    }
    return ret;
} 

function randOp(numcols){
    let o = Math.floor(Math.random() * 4);
    switch(o){
        case 0:
            return "+" + randVal(numcols, 0.5, 0, 1, 100);
        case 1:
            return "-" + randVal(numcols, 0.5, 0, 1, 100);
        case 2:
            return "*" + randVal(numcols, 1, 0, 2, 4);
        case 3:
            return "/" + randVal(numcols, 1, 0, 2, 4);
    }
}

export function randFuncs(numcols, width, height){
    let res = []
    for(let w=0; w < width; w++){
        let warr = []
        for(let h=0; h < height; h++){
            let harr = []
            for(let color=0; color < numcols; color++){
                let colarr = []
                for(let comp=0; comp < 3; comp++){
                    colarr.push(randVal(numcols, 0, 0.3, 255));
                }
                harr.push(colarr)
            }
            warr.push(harr)
        }
        res.push(warr)
    }
    return res
}


function randColors(numcolors){
    let cols = []
    for(let i=0; i < numcolors; i++){
        let r = Math.floor(Math.random() * 256);
        let g = Math.floor(Math.random() * 256);
        let b = Math.floor(Math.random() * 256);
        cols.push([r,g,b]);
    }
    return cols;
}

exports.randVal = randVal;
exports.randOp = randOp;
exports.randFuncs = randFuncs;
exports.randColors = randColors;

const Jimp = require('jimp');
const {workerData, isMainThread} = require('worker_threads');

const RED = 0;
const GREEN = 1;
const BLUE = 2;

function scaleColor(i){
    if (i < 0){
        return 0;
    }
    else if(i > 255){
        return 255;
    }
    else{
        return i;
    }
}

function setPixel(image, width, x, y, color){
    let index = (y * width + x) * 4
    image[index] = scaleColor(color[0]);
    image[index + 1] = scaleColor(color[1]);
    image[index + 2] = scaleColor(color[2]);
    image[index + 3] = 255;
}

function frac(image, n, x, y, colors, funcs, width, pixelwidth){
    if(n <= 0){
        // CREATE BASE IMAGE at x y
        setPixel(image, pixelwidth, x, y, colors[0])
        /*
        let r = scaleColor(colors[0][0]);
        let g = scaleColor(colors[0][1]);
        let b = scaleColor(colors[0][2]);
        image.setPixelColor(Jimp.rgbaToInt(r, g, b, 255), x, y);
        */
    }
    else{
        for(let row=0; row < funcs.length; row++){
            for(let col=0; col < width; col++){
                //console.log(n, colors, funcs[row][col](colors));

                frac(image, n-1, x + row * Math.pow(funcs.length ,(n-1)),
                 y + col * Math.pow(width, (n-1)), funcs[row][col](colors), funcs, width, pixelwidth);
            }
        }
    }
}

function parseOp(funcString, numcolors){
    if(!funcString || funcString == ""){
        return (cols, x) => {return x;};
    }
    let val;
    switch(funcString[0]){
        case '+':
            val = parseVal(funcString.slice(1), numcolors);
            return (cols, x) =>  x + val(cols);
        case '-':
            val = parseVal(funcString.slice(1), numcolors);
            return (cols, x) => x - val(cols);
        case '/':
            val = parseVal(funcString.slice(1),numcolors);
            return (cols, x) => {
                let v = val(cols);
                if(v == 0){
                    return x;
                }
                else{
                    return x / v;
                }
            };
        case '*':
            val = parseVal(funcString.slice(1),numcolors); 
            return (cols, x) => x * val(cols);
        case ' ':
            return parseOp(funcString.slice(1),numcolors);
        default:
            throw new Error('Unexpected string while parsing for an op');
    }
}

function parseVal(funcString, numcolors){
    if(!funcString || funcString == ""){
        throw new Error('Expected and did not find a value while parsing');
    }

    let num;
    let rest;
    if(funcString.length == 1){
        let num = parseInt(funcString, 10);
        if(isNaN(num)){
            throw new Error('Expected and did not find a number while parsing');
        }
        return cols => num;
    }
    switch(funcString[0]){
        case 'r':
            num = parseInt(funcString[1], 10);
            if(isNaN(num)){
                throw new Error('Expected a number after component while parsing');
            }
            if(num >= numcolors || num < 0){
                throw new Error('Component number not within number of colors')
            }
            rest = parseOp(funcString.slice(2),numcolors);
            return cols => rest(cols, cols[num][RED]);
        case 'g':
            num = parseInt(funcString[1], 10);
            if(isNaN(num)){
                throw new Error('Expected a number after component while parsing');
            }
            if(num >= numcolors || num < 0){
                throw new Error('Component number not within number of colors')
            }
            rest = parseOp(funcString.slice(2),numcolors);
            return cols => rest(cols, cols[num][GREEN]);
        case 'b':
            num = parseInt(funcString[1], 10);
            if(isNaN(num)){
                throw new Error('Expected a number after component while parsing');
            }
            if(num >= numcolors || num < 0){
                throw new Error('Component number not within number of colors')
            }
            rest = parseOp(funcString.slice(2),numcolors);
            return cols => rest(cols, cols[num][BLUE]);
        case ' ':
            return parseVal(funcString.slice(1), numcolors);
        default:
            // should be a number.       
            num = parseInt(funcString, 10);
            if(isNaN(num)){
                throw new Error('Expected and did not find a number while parsing');
            }
            let i = 1;
            while(funcString.length > i && !isNaN(parseInt(funcString[i], 10))){
                //num = num * 10 + parseInt(funcString[i], 10);
                i++;
            }
            rest = parseOp(funcString.slice(i), numcolors);
            return cols => rest(cols, num);
    }
}

function parseColorFunction(func, numcolors){
    //parse function to change one color
    if(func.length != 3){
        throw new Error('Color function array has length other than 3');
    }

    let rfunc = parseVal(func[RED], numcolors);
    let gfunc = parseVal(func[GREEN], numcolors);
    let bfunc = parseVal(func[BLUE], numcolors);

    return cols => [rfunc(cols), gfunc(cols), bfunc(cols)];
}

function parseFunction(funcs, numcolors){
    // Parse single colors->colors function
    
    let colorfuncs = [];
    for(let c=0; c < funcs.length; c++){
        colorfuncs.push(parseColorFunction(funcs[c], numcolors));
    }
    return cols => colorfuncs.map(x => x(cols));
}

function parseAllFunctions(funcs, numcolors){

    let fs = [];
    let width = funcs[0].length;

    for(let row=0; row < funcs.length; row++){
        let fsrow = [];
        if (funcs[row].length != width){
            throw new Error('Function row array width not consistent');
        }
        for(let col=0; col < funcs[row].length; col++){
            if(funcs[row][col].length != numcolors){
                throw new Error('Function array has length other than number of colors');
            }
            fsrow.push(parseFunction(funcs[row][col], numcolors));
        }
        fs.push(fsrow);
    }
    return fs;
}

function runFractal(options){
    try{
        if(options.colors.length < 1){
            throw new Error('Must have at least one color');
        }
        if(options.colors.length > 10){
            throw new Error('Too many colors (exceeds 10)');
        }
        options.colors.map(c => {
            if(c.length != 3){
                throw new Error('Color array has length other than 3');
            }
        })

        let funcs = parseAllFunctions(options.functions, options.colors.length);

        let pixelwidth = Math.pow(funcs.length, options.iterations);
        let pixelheight = Math.pow(funcs[0].length, options.iterations);
        if(pixelwidth > 4000 || pixelheight > 4000){
            throw new Error('Image too big');
        }

        //let fractal = new Jimp(width, height, '#FFFFFF');
        let fractal = Buffer.alloc(pixelwidth*pixelheight*4);
        frac(fractal, options.iterations, 0, 0, options.colors, funcs, funcs[0].length, pixelwidth);   
        //fractal.write(`${__dirname}/../www/images/${options.name}.png`);
        return fractal;
    }
    catch(err){
        console.log(err);
    }
}

if(!isMainThread){
    runFractal(workerData);
}

exports.runFractal = runFractal;
exports.parseAllFunctions = parseAllFunctions;
exports.parseColorFunction = parseColorFunction;
exports.parseFunction = parseFunction;
exports.parseVal = parseVal;
exports.parseOp = parseOp;




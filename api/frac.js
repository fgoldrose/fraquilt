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

function frac(image, n, x, y, colors, funcs){
    if(n == 0){
        // CREATE BASE IMAGE at x y
        var r = scaleColor(colors[0][0]);
        var g = scaleColor(colors[0][1]);
        var b = scaleColor(colors[0][2]);
        image.setPixelColor(Jimp.rgbaToInt(r, g, b, 255), x, y);
    }
    else{
        for(var row=0; row < funcs.length; row++){
            for(var col=0; col < funcs[row].length; col++){
                //console.log(n, colors, funcs[row][col](colors));

                frac(image, n-1, x + row * Math.pow(funcs.length ,(n-1)),
                 y + col * Math.pow(funcs[0].length, (n-1)), funcs[row][col](colors), funcs);
            }
        }
    }
}

function parseOp(funcString){
    if(funcString == ""){
        return (cols, x) => {return x;};
    }
    
    switch(funcString[0]){
        case '+':
            var val = parseVal(funcString.slice(1));
            return (cols, x) =>  x + val(cols);
        case '-':
            var val = parseVal(funcString.slice(1));
            return (cols, x) => x - val(cols);
        case '/':
            var val = parseVal(funcString.slice(1));
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
            var val = parseVal(funcString.slice(1)); 
            return (cols, x) => x * val(cols);
        case ' ':
            return parseOp(funcString.slice(1));
        default:
            throw new Error('Unexpected string while parsing for an op');
    }
}

function parseVal(funcString){
    if(funcString == ""){
        throw new Error('Expected and did not find a value while parsing');
    }
    if(funcString.length == 1){
        return cols => parseInt(funcString);
    }
    switch(funcString[0]){
        case 'r':
            var num = parseInt(funcString[1]);
            if(isNaN(num)){
                throw new Error('Expected and did not find a number while parsing');
            }
            var rest = parseOp(funcString.slice(2));
            return cols => rest(cols, cols[num][RED]);
        case 'g':
            var num = parseInt(funcString[1]);
            if(isNaN(num)){
                throw new Error('Expected and did not find a number while parsing');
            }
            var rest = parseOp(funcString.slice(2));
            return cols => rest(cols, cols[num][GREEN]);
        case 'b':
            var num = parseInt(funcString[1]);
            if(isNaN(num)){
                throw new Error('Expected and did not find a number while parsing');
            }
            var rest = parseOp(funcString.slice(2));
            return cols => rest(cols, cols[num][BLUE]);
        case ' ':
            return parseVal(funcString.slice(1));
        default:
            // should be a number.       
            num = parseInt(funcString[0]);
            if(isNaN(num)){
                throw new Error('Expected and did not find a number while parsing');
            }
            var i = 1;
            while(funcString.length > i && !isNaN(parseInt(funcString[i]))){
                num = num * 10 + parseInt(funcString[i]);
                i++;
            }
            var rest = parseOp(funcString.slice(i));
            return cols => rest(cols, num);
    }
}

function parseColorFunction(func){
    //parse function to change one color

    var rfunc = parseVal(func[RED]);
    var gfunc = parseVal(func[GREEN]);
    var bfunc = parseVal(func[BLUE]);

    return cols => [rfunc(cols), gfunc(cols), bfunc(cols)];
}

function parseFunction(funcs){
    // Parse single colors->colors function
    
    var colorfuncs = [];
    for(var c=0; c < funcs.length; c++){
        colorfuncs.push(parseColorFunction(funcs[c]));
    }
    return cols => colorfuncs.map(x => x(cols));
}

function parseAllFunctions(funcs){
    
    var fs = [];
    for(var row=0; row < funcs.length; row++){
        var fsrow = [];
        for(var col=0; col < funcs[row].length; col++){
            fsrow.push(parseFunction(funcs[row][col]));
        }
        fs.push(fsrow);
    }
    return fs;
}

function runFractal(options){
    var funcs = parseAllFunctions(options.functions);

    var width = Math.pow(funcs.length, options.iterations);
    var height = Math.pow(funcs[0].length, options.iterations);

    var fractal = new Jimp(width, height, '#FFFFFF');
    
    frac(fractal, options.iterations, 0, 0, options.colors, funcs);   
    fractal.write(`${__dirname}/../www/images/${options.name}.png`);
}

if(!isMainThread){
    runFractal(workerData);
}


//exports.runFractal = runFractal;




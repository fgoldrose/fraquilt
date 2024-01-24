import { Elm } from "./src/Main.elm";

type ColorAdjustments = {
    tl: number[],
    tr: number[],
    bl: number[],
    br: number[]
};
type ColorVariables = number[];
type Coords = { x: number, y: number };

const app = Elm.Main.init({
    node: document.getElementById("elm")!,
    flags: { randomSeed: new Date().getMilliseconds() }
});
app.ports.renderImage.subscribe(({ colorAdjustments, level, initialVariables }) => {
    generateImage(colorAdjustments, level, initialVariables);
})


function generateImage(colorAdjustments: ColorAdjustments, level: number, initialVariables: ColorVariables) {
    const canvas = <HTMLCanvasElement>document.getElementById("canvas");
    const ctx = canvas.getContext("2d")!;
    const pixelSize = pixelSizeForLevel(level);
    canvas.width = pixelSize;
    canvas.height = pixelSize;
    const imageData = ctx.createImageData(pixelSize, pixelSize);
    fraquilt(imageData, colorAdjustments, level, initialVariables, { x: 0, y: 0 })

    ctx.putImageData(imageData, 0, 0);
}

function fraquilt(imageData: ImageData, colorAdjustments: ColorAdjustments, level: number, colorVariables: ColorVariables, location: Coords) {
    if (level == 0) {
        setPixelColor(imageData, colorVariables, location);
    }
    else {
        const { tl, tr, bl, br } = colorAdjustments;
        fraquilt(imageData, colorAdjustments, level - 1,
            permute(tl, colorVariables), topLeftLocation(location, level)
        );
        fraquilt(imageData, colorAdjustments, level - 1,
            permute(tr, colorVariables), topRightLocation(location, level)
        );
        fraquilt(imageData, colorAdjustments, level - 1,
            permute(bl, colorVariables), bottomLeftLocation(location, level)
        );
        fraquilt(imageData, colorAdjustments, level - 1,
            permute(br, colorVariables), bottomRightLocation(location, level)
        );
    }
}

// Utils
function setPixelColor(imageData: ImageData, [r, g, b,]: number[], { x, y }: Coords) {
    const i = y * (imageData.width * 4) + x * 4;
    const buf = imageData.data;
    buf[i] = r;
    buf[i + 1] = g;
    buf[i + 2] = b;
    buf[i + 3] = 255;
}

function permute(adjustmentArray: number[], colorVariables: number[]) {
    const newColorVariables = [...colorVariables];
    for (let i = 0; i < colorVariables.length; i++) {
        newColorVariables[i] = colorVariables[adjustmentArray[i]];
    }
    return newColorVariables;
}

function pixelSizeForLevel(level: number) {
    return 2 ** level;
}

function topLeftLocation({ x, y }: Coords, level: number) {
    return { x, y };
}
function topRightLocation({ x, y }: Coords, level: number) {
    return { x: x + pixelSizeForLevel(level - 1), y };
}
function bottomLeftLocation({ x, y }: Coords, level: number) {
    return { x, y: y + pixelSizeForLevel(level - 1) }
}
function bottomRightLocation({ x, y }: Coords, level: number) {
    return { x: x + pixelSizeForLevel(level - 1), y: y + pixelSizeForLevel(level - 1) };
}

function randomInRange(min: number, max: number) {
    return Math.floor(Math.random() * (max - min)) + min;
}
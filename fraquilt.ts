import { Elm } from "./src/Main.elm";

type Permutation = number[];
type Permutations = {
    tl: Permutation,
    tr: Permutation,
    bl: Permutation,
    br: Permutation
};
type Color = { r: number, g: number, b: number };
type ColorVariables = Color[];
type Coords = { x: number, y: number };

const app = Elm.Main.init({
    flags: {
        randomSeed: new Date().getMilliseconds(),
        window: {
            width: window.innerWidth,
            height: window.innerHeight
        }
    }
});
app.ports.makeFullscreen.subscribe(() => {
    const canvas = <HTMLCanvasElement | null>document.getElementById("canvas");
    if (canvas) {
        canvas.requestFullscreen();
    }
})
app.ports.renderImage.subscribe(({ permutations, level, initialVariables }) => {
    const initialVarsColors = initialVariables.map(hexToRgb);
    generateImage(permutations, level, initialVarsColors);
})

function hexToRgb(hex: string): Color {
    const r = parseInt(hex.slice(1, 3), 16);
    const g = parseInt(hex.slice(3, 5), 16);
    const b = parseInt(hex.slice(5, 7), 16);
    return { r, g, b };
}

function generateImage(permutations: Permutations, level: number, initialVariables: ColorVariables, tries = 0) {
    const canvas = <HTMLCanvasElement | null>document.getElementById("canvas");
    if (!canvas) {
        // If the canvas isn't in the dom yet, try again in 100ms
        if (tries < 10) {
            setTimeout(() => generateImage(permutations, level, initialVariables, tries + 1), 100);
        }
    }
    else {
        const ctx = canvas.getContext("2d")!;
        const pixelSize = pixelSizeForLevel(level);
        canvas.width = pixelSize;
        canvas.height = pixelSize;
        const imageData = ctx.createImageData(pixelSize, pixelSize);
        fraquilt(imageData, permutations, level, initialVariables, { x: 0, y: 0 })

        ctx.putImageData(imageData, 0, 0);
    }
}

function fraquilt(imageData: ImageData, permutations: Permutations, level: number, colorVariables: ColorVariables, location: Coords) {
    if (level == 0) {
        setPixelColor(imageData, colorVariables, location);
    }
    else {
        const { tl, tr, bl, br } = permutations;
        fraquilt(imageData, permutations, level - 1,
            permute(tl, colorVariables), topLeftLocation(location, level)
        );
        fraquilt(imageData, permutations, level - 1,
            permute(tr, colorVariables), topRightLocation(location, level)
        );
        fraquilt(imageData, permutations, level - 1,
            permute(bl, colorVariables), bottomLeftLocation(location, level)
        );
        fraquilt(imageData, permutations, level - 1,
            permute(br, colorVariables), bottomRightLocation(location, level)
        );
    }
}

// Utils
function setPixelColor(imageData: ImageData, [color]: ColorVariables, { x, y }: Coords) {
    const i = y * (imageData.width * 4) + x * 4;
    const buf = imageData.data;
    buf[i] = color.r;
    buf[i + 1] = color.g;
    buf[i + 2] = color.b;
    buf[i + 3] = 255;
}

function permute(adjustmentArray: Permutation, colorVariables: ColorVariables) {
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

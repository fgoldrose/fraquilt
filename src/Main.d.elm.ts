

export interface App {
    readonly ports: Ports;
}

export type Ports = {
    readonly renderImage: {
        subscribe: (callback: (args: {
            level: number,
            initialVariables: number[],
            colorAdjustments: {
                tl: number[],
                tr: number[],
                bl: number[],
                br: number[]
            };
        }) => void) => void
    }
}

declare const Elm: {
    Main: {
        init(options: { node?: HTMLElement | null; flags: { randomSeed: number } }): App;
    };
};
export { Elm };
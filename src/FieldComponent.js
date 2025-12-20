export const offsetX = event => event.offsetX;
export const offsetY = event => event.offsetY;
export const getContext2DThatWillReadFrequently = canvas => () => canvas.getContext("2d", { willReadFrequently: true });
export const devicePixelRatio = window => window.devicePixelRatio;

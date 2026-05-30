export const offsetX = event => event.offsetX;
export const offsetY = event => event.offsetY;
export const getContext2DThatWillReadFrequently = canvas => () => canvas.getContext("2d", { willReadFrequently: true });
export const devicePixelRatio = window => window.devicePixelRatio;
export const wheelDeltaY = event => event.deltaY;
export const wheelOffsetX = event => event.offsetX;
export const wheelOffsetY = event => event.offsetY;
// Returns the active touch points relative to the top-left of the event target.
export const touchPositions = event => () => {
  const rect = event.target.getBoundingClientRect();
  const result = [];
  for (let i = 0; i < event.touches.length; i++) {
    const t = event.touches[i];
    result.push({ x: t.clientX - rect.left, y: t.clientY - rect.top });
  }
  return result;
};

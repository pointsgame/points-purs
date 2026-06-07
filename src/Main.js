const moveSoundAudio = new Audio('move.mp3');
export const playMoveSound = () => {
  moveSoundAudio.currentTime = 0;
  moveSoundAudio.play().catch(() => {});
};
export const setCookie = cookie => () => window.document.cookie = cookie;
export const saveFile = filename => content => () => {
  const blob = new Blob([content], { type: "application/x-smartgame" });
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  URL.revokeObjectURL(url);
};

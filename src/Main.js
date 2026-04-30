export const setCookie = cookie => () => window.document.cookie = cookie;
export const postMessage = window => message => () => window.postMessage(message);
export const eventData = event => event.data;
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

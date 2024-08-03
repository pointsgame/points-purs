export const setCookie = cookie => () => window.document.cookie = cookie;
export const postMessage = window => message => () => window.postMessage(message);
export const eventData = event => event.data;

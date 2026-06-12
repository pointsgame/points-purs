import init from './oppai_webworker.js';

// Queue messages that arrive while the wasm module is still loading; the wasm
// start function installs its own onmessage handler, so replay the queue
// through it once initialization is done.
const queue = [];
self.onmessage = event => queue.push(event.data);
init().then(() => {
  for (const data of queue) {
    self.onmessage(new MessageEvent('message', { data }));
  }
});

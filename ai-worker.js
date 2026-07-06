import init from './oppai_webworker.js';

// Messages start being delivered as soon as module evaluation hits its first
// top-level await, so a queue is needed to cover the wasm instantiation
// window. The wasm start function installs the real onmessage handler
// synchronously inside init(); once init() resolves, replay the buffered
// messages into it. Drain a snapshot (splice) rather than the live array so a
// handler that re-queues messages can never turn this loop into an infinite
// one.
const queue = [];
self.onmessage = event => queue.push(event.data);
init()
  .then(() => {
    for (const data of queue.splice(0)) {
      self.onmessage(new MessageEvent('message', { data }));
    }
  })
  .catch(error => console.error('Failed to initialize the OpPAI worker:', error));

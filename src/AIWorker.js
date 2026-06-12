export const _create = onMessage => () => {
  const worker = new Worker("ai-worker.js", { type: "module" });
  worker.onmessage = event => onMessage(event.data)();
  return worker;
};

export const _post = worker => message => () => worker.postMessage(message);

export const terminate = worker => () => worker.terminate();

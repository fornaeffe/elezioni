/** Browser test harness; imports the real client and its Vite-emitted worker. */
export { ReformFitClient } from '../../src/lib/politics/reform-2026/vote-share/fit-client';
export { ownedBuffers } from '../../src/lib/politics/reform-2026/vote-share/fit-resources';
export async function runConsumer(
  workerUrl: string,
  packet: unknown,
  transfer: ArrayBuffer[],
  simulations: number[],
) {
  const worker = new Worker(workerUrl, { type: 'module' });
  try {
    return await new Promise<unknown>((done, reject) => {
      worker.onmessage = (e) =>
        e.data.ok ? done(e.data.hashes) : reject(new Error(e.data.error));
      worker.onerror = (e) => reject(new Error(e.message));
      worker.postMessage({ packet, simulations }, transfer);
    });
  } finally {
    worker.terminate();
  }
}

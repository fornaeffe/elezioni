import { gunzipSync } from 'fflate';
self.onmessage = (event: MessageEvent<{ id: number; bytes: Uint8Array }>) => {
  try {
    const bytes = gunzipSync(event.data.bytes);
    self.postMessage({ id: event.data.id, bytes }, { transfer: [bytes.buffer] });
  } catch (error) { self.postMessage({ id: event.data.id, error: String(error) }); }
};

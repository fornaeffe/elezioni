import { expect, it } from 'vitest';
import { ResultServiceClient } from './result-service-client';

it('keeps requests pending through progress and isolates concurrent request IDs', async () => {
  class Transport extends EventTarget { postMessage() {} terminate() {} }
  const transport = new Transport(), client = new ResultServiceClient(transport as unknown as Worker), seen: unknown[] = [];
  const first = client.call('export', { resultId: 'a', owner: 'owner' }, p => seen.push(p));
  const second = client.call('import', { resultId: 'b', owner: 'owner' });
  const progress = { phase: 'exporting', completed: 12, total: 30 };
  transport.dispatchEvent(new MessageEvent('message', { data: { type: 'result-service-progress', requestId: 1, progress } }));
  transport.dispatchEvent(new MessageEvent('message', { data: { type: 'result-service', requestId: 2, ok: true, data: 'second' } }));
  expect(await second).toBe('second'); expect(seen).toEqual([progress]);
  transport.dispatchEvent(new MessageEvent('message', { data: { type: 'result-service', requestId: 1, ok: true, data: 'first' } }));
  expect(await first).toBe('first'); client.close();
});

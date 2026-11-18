import { expect, it } from 'vitest';
import { encodedExportStream } from './export-stream';

it('preserves exact UTF-8 while batching rows, including split surrogate pairs', async () => {
  const parts = [...Array(10000).fill('Città "Europa" \\ 0,false,null\n'), '\ud83d', '\ude00'];
  let pulls = 0;
  const output = await new Response(encodedExportStream((async function* () { yield* parts; })(), () => pulls++)).text();
  expect(output).toBe(parts.join('')); expect(pulls).toBeLessThan(10);
});
it('closes the source on cancellation and propagates late errors', async () => {
  let closed = false;
  async function* source() { try { for (;;) yield 'x'.repeat(65536); } finally { closed = true; } }
  const reader = encodedExportStream(source()).getReader(); await reader.read(); await reader.cancel();
  await expect.poll(() => closed).toBe(true);
  const broken = encodedExportStream((async function* () { yield 'prefix'; throw new Error('broken'); })());
  await expect(new Response(broken).text()).rejects.toThrow('broken');
});

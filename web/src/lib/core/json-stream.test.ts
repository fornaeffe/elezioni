import { expect, it } from 'vitest';
import { JsonStreamReader } from './json-stream';

function bytes(text: string, size = 1) {
  const input = new TextEncoder().encode(text); let at = 0;
  return new ReadableStream<Uint8Array<ArrayBuffer>>({ pull(c) { if (at === input.length) c.close(); else { c.enqueue(input.slice(at, at + size)); at = Math.min(input.length, at + size); } } });
}
it('parses escaped strings and UTF-8 across every byte boundary', async () => {
  const value = { name: 'Città "Europa" \\ 😀', rows: [[null, true, false, -1.25e-8], ['line\nend', '\u0000']] };
  const reader = new JsonStreamReader(bytes(JSON.stringify(value)));
  expect(await reader.value()).toEqual(value); await reader.finish(); await reader.close();
});
it('streams array records individually and rejects malformed JSON', async () => {
  const reader = new JsonStreamReader(bytes('[1,2,3]', 2)); const rows: unknown[] = [];
  await reader.array(async () => { rows.push(await reader.value()); });
  expect(rows).toEqual([1, 2, 3]); await reader.finish(); await reader.close();
  for (const input of ['[1,]', '{"x":1,"x":2}', '01', '"unterminated', '[1 2]', '{"a":1} trailing', '[1 "," 2]', '{"a":1 "," "b":2}', '["]"', '{"}"', '\u00a0{}']) {
    const malformed = new JsonStreamReader(bytes(input, 3));
    await expect((async () => { try { await malformed.value(); await malformed.finish(); } finally { await malformed.close(); } })()).rejects.toThrow();
  }
});

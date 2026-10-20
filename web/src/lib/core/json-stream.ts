/** Incremental JSON reader. Only the current token/selected value is resident;
 * table row arrays are consumed separately by the export importer. */
type Token = { kind: 'string' | 'number' | 'literal'; value: unknown } | { kind: 'punctuation'; value: string };
export class JsonStreamReader {
  private reader: ReadableStreamDefaultReader<string>;
  private chunk = '';
  private position = 0;
  private lookahead: Token | null | undefined;
  constructor(stream: ReadableStream<Uint8Array<ArrayBuffer>>) { this.reader = stream.pipeThrough(new TextDecoderStream()).getReader(); }
  async close() { await this.reader.cancel().catch(() => undefined); this.reader.releaseLock(); }
  private async character(): Promise<boolean> {
    while (this.position === this.chunk.length) {
      const next = await this.reader.read(); if (next.done) return false;
      this.chunk = next.value; this.position = 0;
    }
    return true;
  }
  private async token(): Promise<Token | null> {
    while (await this.character()) {
      if (!/[ \t\r\n]/.test(this.chunk[this.position])) break;
      this.position++;
    }
    if (!await this.character()) return null;
    const first = this.chunk[this.position++];
    if ('{}[],:'.includes(first)) return { kind: 'punctuation', value: first };
    const parts = [first]; let length = 1;
    if (first === '"') {
      let escaped = false;
      for (;;) {
        if (!await this.character()) throw new Error('Unterminated JSON string');
        const start = this.position;
        while (this.position < this.chunk.length) {
          const ch = this.chunk[this.position++];
          if (!escaped && ch === '"') {
            if (length + this.position - start > 16 * 1024 * 1024) throw new Error('JSON token exceeds the bounded record limit');
            parts.push(this.chunk.slice(start, this.position));
            return { kind: 'string', value: JSON.parse(parts.join('')) };
          }
          if (escaped) escaped = false; else if (ch === '\\') escaped = true;
        }
        const part = this.chunk.slice(start, this.position); length += part.length;
        if (length > 16 * 1024 * 1024) throw new Error('JSON token exceeds the bounded record limit');
        parts.push(part);
      }
    }
    for (;;) {
      if (!await this.character()) break;
      const start = this.position;
      while (this.position < this.chunk.length && !/[\s{}\[\],:]/.test(this.chunk[this.position])) this.position++;
      const part = this.chunk.slice(start, this.position); parts.push(part); length += part.length;
      if (length > 128) throw new Error('Invalid JSON scalar');
      if (this.position < this.chunk.length) break;
    }
    const text = parts.join('');
    if (['true', 'false', 'null'].includes(text)) return { kind: 'literal', value: JSON.parse(text) };
    if (!/^-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?$/.test(text) || !Number.isFinite(Number(text))) throw new Error('Invalid JSON number');
    return { kind: 'number', value: Number(text) };
  }
  async peek() { if (this.lookahead === undefined) this.lookahead = await this.token(); return this.lookahead; }
  async take() { const t = await this.peek(); this.lookahead = undefined; return t; }
  async expect(value: string) { const t = await this.take(); if (t?.kind !== 'punctuation' || t.value !== value) throw new Error(`Expected JSON ${value}`); }
  async object(visit: (key: string) => Promise<void>): Promise<void> {
    await this.expect('{'); const keys = new Set<string>();
    const first = await this.peek();
    if (first?.kind === 'punctuation' && first.value === '}') { await this.take(); return; }
    for (;;) {
      const key = await this.take();
      if (key?.kind !== 'string' || keys.has(key.value as string)) throw new Error('Invalid or duplicate JSON object key');
      keys.add(key.value as string); await this.expect(':'); await visit(key.value as string);
      const end = await this.take(); if (end?.kind === 'punctuation' && end.value === '}') return;
      if (end?.kind !== 'punctuation' || end.value !== ',') throw new Error('Expected JSON comma');
    }
  }
  async array(visit: (index: number) => Promise<void>): Promise<void> {
    await this.expect('[');
    const first = await this.peek();
    if (first?.kind === 'punctuation' && first.value === ']') { await this.take(); return; }
    for (let i = 0;; i++) {
      await visit(i); const end = await this.take();
      if (end?.kind === 'punctuation' && end.value === ']') return;
      if (end?.kind !== 'punctuation' || end.value !== ',') throw new Error('Expected JSON comma');
    }
  }
  async value(depth = 0): Promise<unknown> {
    if (depth > 128) throw new Error('JSON nesting limit exceeded');
    const t = await this.peek(); if (!t) throw new Error('Unexpected end of JSON');
    if (t.kind !== 'punctuation') { await this.take(); return t.value; }
    if (t.value === '{') {
      const result: Record<string, unknown> = Object.create(null);
      await this.object(async key => { result[key] = await this.value(depth + 1); }); return result;
    }
    if (t.value === '[') { const result: unknown[] = []; await this.array(async () => { result.push(await this.value(depth + 1)); }); return result; }
    throw new Error('Unexpected JSON punctuation');
  }
  async skip(depth = 0): Promise<void> {
    if (depth > 128) throw new Error('JSON nesting limit exceeded');
    const t = await this.peek();
    if (t?.value === '{' && t.kind === 'punctuation') await this.object(async () => this.skip(depth + 1));
    else if (t?.value === '[' && t.kind === 'punctuation') await this.array(async () => this.skip(depth + 1));
    else { if (!t || t.kind === 'punctuation') throw new Error('Expected JSON value'); await this.take(); }
  }
  async finish() { if (await this.take() !== null) throw new Error('Trailing JSON data'); }
}
export function jsonFileStream(file: File): ReadableStream<Uint8Array<ArrayBuffer>> {
  return file.name.endsWith('.gz') ? file.stream().pipeThrough(new DecompressionStream('gzip')) : file.stream();
}
export async function isReformFile(file: File): Promise<boolean> {
  const reader = new JsonStreamReader(jsonFileStream(file));
  const stop = Symbol('header'); let reform = false;
  try {
    await reader.object(async key => {
      if (key === 'kind') { reform = await reader.value() === 'politics-reform-scenario'; throw stop; }
      if (key === 'scenario') {
        const scenario = await reader.value() as Record<string, unknown>;
        reform = scenario?.kind === 'politics-reform-scenario'; throw stop;
      }
      await reader.skip();
    });
  } catch (error) { if (error !== stop) throw error; }
  finally { await reader.close(); }
  return reform;
}

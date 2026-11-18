import { afterEach, describe, expect, it, vi } from 'vitest';
import { mkdtemp, readFile, readdir, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { basename, join, relative } from 'node:path';
import { downloadSource } from './source-download';
import { acquireSource, sha256, type Source } from './sources';

const { agentOptions } = vi.hoisted(() => ({ agentOptions: vi.fn() }));
vi.mock('undici', async importOriginal => {
  const actual = await importOriginal<typeof import('undici')>();
  return { ...actual, Agent: class extends actual.Agent {
    constructor(options: ConstructorParameters<typeof actual.Agent>[0]) { super(options); agentOptions(options); }
  } };
});
const bytes = Buffer.from('reviewed original'), source: Source = { id: 'reviewed', path: 'original.bin',
  url: 'https://example.invalid/original', headers: { Accept: 'application/octet-stream' }, bytes: bytes.length, sha256: sha256(bytes) };
const networkError = (code: string) => new TypeError('fetch failed', { cause: Object.assign(new Error('connection failed'), { code }) });
const directories: string[] = [];
async function directory() { const d = await mkdtemp(join(tmpdir(), 'elezioni-source-download-')); directories.push(d); return d; }
function setup() {
  vi.spyOn(console, 'log').mockImplementation(() => {});
  const warnings = vi.spyOn(console, 'warn').mockImplementation(() => {});
  return { fetch: vi.spyOn(globalThis, 'fetch'), warnings };
}
function timers() { vi.useFakeTimers({ toFake: ['setTimeout', 'clearTimeout', 'Date'] }); }
afterEach(async () => {
  vi.useRealTimers(); vi.restoreAllMocks(); agentOptions.mockClear();
  for (const d of directories.splice(0)) {
    if (!basename(d).startsWith('elezioni-source-download-') || relative(tmpdir(), d).startsWith('..')) throw new Error('Unsafe cleanup');
    await rm(d, { recursive: true, force: true });
  }
});

describe('bounded source transport', () => {
  it('retries wrapped connection failures with the same request and an explicit connection timeout', async () => {
    timers(); const { fetch, warnings } = setup();
    fetch.mockRejectedValueOnce(networkError('UND_ERR_CONNECT_TIMEOUT')).mockRejectedValueOnce(networkError('ECONNRESET'))
      .mockResolvedValueOnce(new Response(bytes));
    const result = downloadSource(source), checked = expect(result).resolves.toEqual(bytes);
    await vi.advanceTimersByTimeAsync(1999); expect(fetch).toHaveBeenCalledTimes(1);
    await vi.advanceTimersByTimeAsync(1); expect(fetch).toHaveBeenCalledTimes(2);
    await vi.advanceTimersByTimeAsync(4999); expect(fetch).toHaveBeenCalledTimes(2);
    await vi.advanceTimersByTimeAsync(1); await checked;
    expect(fetch).toHaveBeenCalledTimes(3);
    expect(agentOptions).toHaveBeenCalledWith({ connect: { timeout: 30_000 } });
    for (const [url, init] of fetch.mock.calls) {
      expect(url).toBe(source.url); expect(init!.headers).toEqual(source.headers);
      expect(init!.signal).toBe(fetch.mock.calls[0][1]!.signal);
    }
    expect(warnings.mock.calls[0][0]).toContain('id=reviewed; host=example.invalid; attempt=1/3; elapsed=0.0s; code=UND_ERR_CONNECT_TIMEOUT; retry=true');
    expect(vi.getTimerCount()).toBe(0);
  });
  it('recovers from a wrapped ENOTFOUND without changing the request or deadline', async () => {
    timers(); const { fetch, warnings } = setup();
    fetch.mockRejectedValueOnce(networkError('ENOTFOUND')).mockResolvedValueOnce(new Response(bytes));
    const checked = expect(downloadSource(source)).resolves.toEqual(bytes);
    await vi.advanceTimersByTimeAsync(1999); expect(fetch).toHaveBeenCalledOnce();
    await vi.advanceTimersByTimeAsync(1); await checked;
    expect(fetch).toHaveBeenCalledTimes(2);
    expect(fetch.mock.calls[1][0]).toBe(source.url);
    expect(fetch.mock.calls[1][1]).toEqual(fetch.mock.calls[0][1]);
    expect(warnings.mock.calls[0][0]).toContain('code=ENOTFOUND; retry=true');
    expect(vi.getTimerCount()).toBe(0);
  });
  it.each(['ETIMEDOUT', 'ENOTFOUND'])('stops after three %s failures and preserves the underlying cause', async code => {
    timers(); const { fetch, warnings } = setup(), error = networkError(code); fetch.mockRejectedValue(error);
    const result = downloadSource(source), checked = expect(result).rejects.toMatchObject({ cause: error,
      message: expect.stringContaining(`attempts=3/3; elapsed=7.0s; code=${code}`) });
    await vi.advanceTimersByTimeAsync(1999); expect(fetch).toHaveBeenCalledTimes(1);
    await vi.advanceTimersByTimeAsync(1); expect(fetch).toHaveBeenCalledTimes(2);
    await vi.advanceTimersByTimeAsync(4999); expect(fetch).toHaveBeenCalledTimes(2);
    await vi.advanceTimersByTimeAsync(1); await checked;
    expect(fetch).toHaveBeenCalledTimes(3); expect(vi.getTimerCount()).toBe(0);
    expect(warnings.mock.calls[2][0]).toContain(`attempt=3/3; elapsed=7.0s; code=${code}; retry=false`);
  });
  it('retries a body interrupted after the response headers', async () => {
    timers(); const { fetch } = setup();
    const body = new ReadableStream({ start(controller) {
      controller.enqueue(bytes.subarray(0, 3)); controller.error(Object.assign(new Error('interrupted'), { code: 'ECONNRESET' }));
    } });
    fetch.mockResolvedValueOnce(new Response(body)).mockResolvedValueOnce(new Response(bytes));
    const checked = expect(downloadSource(source)).resolves.toEqual(bytes);
    await vi.advanceTimersByTimeAsync(2000); await checked; expect(fetch).toHaveBeenCalledTimes(2);
  });
  it.each([408, 429, 500, 502, 503, 504])('retries HTTP %i and releases its response body', async status => {
    timers(); const { fetch } = setup(), response = new Response('temporary failure', { status });
    const cancel = vi.spyOn(response.body!, 'cancel');
    fetch.mockResolvedValueOnce(response).mockResolvedValueOnce(new Response(bytes));
    const checked = expect(downloadSource(source)).resolves.toEqual(bytes);
    await vi.advanceTimersByTimeAsync(2000); await checked;
    expect(cancel).toHaveBeenCalledOnce(); expect(fetch).toHaveBeenCalledTimes(2);
  });
  it.each(['10', 'Tue, 17 Nov 2026 12:00:10 GMT'])('honors Retry-After %s', async retryAfter => {
    timers(); vi.setSystemTime(new Date('2026-11-17T12:00:00Z')); const { fetch } = setup();
    fetch.mockResolvedValueOnce(new Response('wait', { status: 429, headers: { 'Retry-After': retryAfter } }))
      .mockResolvedValueOnce(new Response(bytes));
    const checked = expect(downloadSource(source)).resolves.toEqual(bytes);
    await vi.advanceTimersByTimeAsync(9999); expect(fetch).toHaveBeenCalledTimes(1);
    await vi.advanceTimersByTimeAsync(1); await checked; expect(fetch).toHaveBeenCalledTimes(2);
  });
  it('shares one deadline across attempts and aborts a stalled request', async () => {
    timers(); const { fetch } = setup();
    fetch.mockRejectedValueOnce(networkError('UND_ERR_CONNECT_TIMEOUT')).mockImplementationOnce((_url, init) =>
      new Promise((_resolve, reject) => init!.signal!.addEventListener('abort', () => reject(init!.signal!.reason), { once: true })));
    const checked = expect(downloadSource(source)).rejects.toThrow('attempts=2/3; elapsed=180.0s; code=SOURCE_DEADLINE_EXCEEDED');
    await vi.advanceTimersByTimeAsync(180_000); await checked;
    expect(fetch).toHaveBeenCalledTimes(2); expect(vi.getTimerCount()).toBe(0);
  });
  it('ends at the deadline when Retry-After exceeds its remaining budget', async () => {
    timers(); const { fetch } = setup();
    fetch.mockResolvedValue(new Response('wait', { status: 429, headers: { 'Retry-After': '9999999999' } }));
    const checked = expect(downloadSource(source)).rejects.toThrow('elapsed=180.0s; code=SOURCE_DEADLINE_EXCEEDED');
    await vi.advanceTimersByTimeAsync(180_000); await checked;
    expect(fetch).toHaveBeenCalledOnce(); expect(vi.getTimerCount()).toBe(0);
  });
  it.each([403, 404])('does not retry permanent HTTP %i failures', async status => {
    const { fetch } = setup(); fetch.mockResolvedValue(new Response('unavailable', { status }));
    await expect(downloadSource(source)).rejects.toThrow(`code=HTTP_${status}`);
    expect(fetch).toHaveBeenCalledOnce();
  });
  it('does not retry TLS validation failures', async () => {
    const { fetch } = setup(); fetch.mockRejectedValue(networkError('CERT_HAS_EXPIRED'));
    await expect(downloadSource(source)).rejects.toThrow('code=CERT_HAS_EXPIRED');
    expect(fetch).toHaveBeenCalledOnce();
  });
});

describe('strict acquisition after transport', () => {
  it('rejects changed bytes after DNS recovery without a third attempt or a cached file', async () => {
    const d = await directory(), { fetch } = setup();
    timers();
    let started!: () => void;
    const firstFetch = new Promise<void>(resolve => { started = resolve; });
    fetch.mockImplementationOnce(() => { started(); return Promise.reject(networkError('ENOTFOUND')); })
      .mockResolvedValueOnce(new Response('changed upstream'));
    const checked = expect(acquireSource(d, source)).rejects.toThrow('differs from its pinned version');
    await firstFetch;
    await vi.advanceTimersByTimeAsync(2000); await checked;
    expect(fetch).toHaveBeenCalledTimes(2);
    expect(await readdir(d)).toEqual([]);
    expect(vi.getTimerCount()).toBe(0);
  });
  it('rejects changed downloaded bytes immediately without caching or retrying', async () => {
    const d = await directory(), { fetch } = setup(); fetch.mockResolvedValue(new Response('changed upstream'));
    await expect(acquireSource(d, source)).rejects.toThrow('differs from its pinned version');
    expect(fetch).toHaveBeenCalledOnce(); expect(await readdir(d)).toEqual([]);
  });
  it('rejects corrupt caches and imports without contacting the network', async () => {
    const d = await directory(), { fetch } = setup(); await writeFile(join(d, source.path), 'corrupt');
    await expect(acquireSource(d, source)).rejects.toThrow('differs from its pinned version');
    await expect(acquireSource(join(d, 'fresh'), source, join(d, source.path))).rejects.toThrow('differs from its pinned version');
    expect(fetch).not.toHaveBeenCalled(); expect(await readFile(join(d, source.path), 'utf8')).toBe('corrupt');
  });
});

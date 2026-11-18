import { build } from 'vite';
import { chromium } from '@playwright/test';
import { createServer } from 'node:http';
import { fileURLToPath } from 'node:url';
import { mkdir, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { execFile, execFileSync } from 'node:child_process';
import { promisify } from 'node:util';
import { readFileSync } from 'node:fs';

const label = process.env.ARCHIVE_BENCH_LABEL ?? 'current';
if (!/^[\w-]+$/.test(label)) throw new Error('Invalid benchmark label');
const rows = Number(process.env.ARCHIVE_BENCH_ROWS ?? 100000);
if (!Number.isSafeInteger(rows) || rows < 1) throw new Error('Invalid benchmark rows');
const codecWorkers = Number(process.env.ARCHIVE_BENCH_CODEC_WORKERS ?? 2);
if (![0, 2, 4].includes(codecWorkers)) throw new Error('Codec workers must be 0, 2 or 4');
const baselineRef = process.env.ARCHIVE_BENCH_BASELINE === '1' ? 'HEAD' : process.env.ARCHIVE_BENCH_BASELINE;
if (baselineRef && !/^[\w./-]+$/.test(baselineRef)) throw new Error('Invalid baseline revision');
const built = await build({ configFile: false, logLevel: 'error',
  plugins: [{ name: 'archive-benchmark-variant', load(id) {
    const normalized = id.replaceAll('\\', '/'), root = resolve('src').replaceAll('\\', '/') + '/';
    if (baselineRef && normalized.startsWith(root) && /\.(ts|json)$/.test(normalized)) {
      try { return execFileSync('git', ['show', `${baselineRef}:web/src/${normalized.slice(root.length)}`], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'ignore'] }); }
      catch { return null; } // Generated untracked assets use their current exact bytes.
    }
    if (normalized === root + 'lib/core/archive-chunk-decoder.ts')
      return readFileSync(id, 'utf8').replace('const archiveCodecWorkers = 2;', `const archiveCodecWorkers = ${codecWorkers};`);
    return null;
  } }],
  resolve: { alias: { $lib: fileURLToPath(new URL('../../src/lib', import.meta.url)) } },
  build: { write: false, minify: true, lib: { entry: fileURLToPath(new URL('../../tests/fixtures/archive-performance.worker.ts', import.meta.url)), formats: ['es'], fileName: 'archive-benchmark' } } });
const files = new Map<string, string | Uint8Array>(); let entry = '';
for (const output of Array.isArray(built) ? built : [built]) if ('output' in output) for (const chunk of output.output) {
  files.set('/' + chunk.fileName, chunk.type === 'chunk' ? chunk.code : chunk.source);
  if (chunk.type === 'chunk' && chunk.isEntry) entry = '/' + chunk.fileName;
}
const server = createServer((request, response) => {
  const content = files.get(request.url ?? '');
  response.setHeader('Content-Type', content ? 'text/javascript' : 'text/html');
  response.end(content ?? '<!doctype html><title>Isolated archive benchmark</title>');
});
await new Promise<void>(r => server.listen(0, '127.0.0.1', r));
const address = server.address(); if (!address || typeof address === 'string') throw new Error('Missing benchmark address');
const browser = await chromium.launch({ channel: 'chrome', headless: true });
try {
  const page = await browser.newPage(); await page.goto(`http://127.0.0.1:${address.port}`);
  page.on('console', message => console.log(message.text()));
  const root = await browser.newBrowserCDPSession(), execute = promisify(execFile), samples: { privateBytes: number; workingSetBytes: number }[] = [];
  let sampling: Promise<void> | undefined, sampleError: unknown;
  const sample = async () => {
    if (process.platform !== 'win32') return;
    const ids = (await root.send('SystemInfo.getProcessInfo')).processInfo.filter(p => p.type === 'renderer').map(p => p.id);
    if (!ids.length || !ids.every(Number.isSafeInteger)) throw new Error('Missing renderer process IDs');
    const output = await execute('pwsh.exe', ['-NoProfile', '-NonInteractive', '-Command',
      `$p=@(Get-Process -Id ${ids.join(',')} -ErrorAction Stop); @{privateBytes=($p|Measure-Object PrivateMemorySize64 -Sum).Sum;workingSetBytes=($p|Measure-Object WorkingSet64 -Sum).Sum}|ConvertTo-Json -Compress`], { windowsHide: true, timeout: 5000 });
    samples.push(JSON.parse(output.stdout));
  };
  await sample();
  const timer = setInterval(() => { if (!sampling) sampling = sample().catch(e => { sampleError = e; }).finally(() => { sampling = undefined; }); }, 1000);
  let result: Record<string, unknown>;
  try { result = await page.evaluate(({ entry, rows }) => new Promise<Record<string, unknown>>((resolve, reject) => {
    const worker = new Worker(entry, { type: 'module' });
    worker.onmessage = e => { if (e.data.stage) { console.log(JSON.stringify(e.data)); return; } if (e.data.type === 'result-service-progress') return; worker.terminate(); resolve(e.data); };
    worker.onerror = e => reject(new Error(e.message)); worker.postMessage({ rows });
  }), { entry, rows }); } finally { clearInterval(timer); await sampling; }
  if (sampleError) throw sampleError;
  if (!result.ok) throw new Error(String(result.error));
  const report = { browser: browser.version(), label, codecWorkers: baselineRef ? 0 : codecWorkers, memory: { peakPrivateBytes: Math.max(0, ...samples.map(s => s.privateBytes)), peakWorkingSetBytes: Math.max(0, ...samples.map(s => s.workingSetBytes)), samples: samples.length }, baselineRevision: baselineRef ? execFileSync('git', ['rev-parse', baselineRef], { encoding: 'utf8' }).trim() : null, ...result,
    qualification: 'Constructed M01 archive; real worker, IndexedDB, validation, gzip and OPFS; three trials; temporary isolated browser context. Sampled renderer-wide process memory includes workers, constructed input, verification buffers and GC; not a hard maximum. Not a national/M02 campaign.' };
  const directory = resolve('../data/derived/archive-performance'); await mkdir(directory, { recursive: true });
  await writeFile(resolve(directory, `${label}-${rows}.json`), JSON.stringify(report, null, 2) + '\n');
  console.log(JSON.stringify(report));
} finally { await browser.close(); await new Promise<void>((r, reject) => server.close(e => e ? reject(e) : r())); }

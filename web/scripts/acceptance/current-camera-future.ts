import { requireGenerationModel } from '../../src/lib/politics/reform-2026/vote-share/model-registry';
/** Browser-worker timing only; no app activation, exports or old-model reruns. */
import { createServer } from 'node:http';
import { readFile, writeFile, mkdir } from 'node:fs/promises';
import { resolve, relative, isAbsolute } from 'node:path';
import { createHash } from 'node:crypto';
import { build } from 'vite';
import { chromium } from '@playwright/test';
requireGenerationModel({ model: 'M02', modelVersion: '1.0.0' }); // Reproduce with commit 8dcb830.
const draws = Number(process.env.CAMERA_BROWSER_DRAWS ?? 1000), workerCount = Number(process.env.CAMERA_BROWSER_WORKERS ?? 4);
if (!Number.isSafeInteger(draws) || draws < 1 || !Number.isSafeInteger(workerCount) || workerCount < 1 || workerCount > draws) throw new Error('Invalid workload');
const output = resolve('../data/derived/current-camera-comparison'), staticRoot = resolve('static');
await mkdir(output, { recursive: true });
let workerCode = '';
const bundle = await build({ configFile: false, logLevel: 'error', build: { write: false, minify: false,
  lib: { entry: resolve('scripts/acceptance/current-camera-future.worker.ts'), formats: ['es'], fileName: 'camera-benchmark' } } });
for (const item of Array.isArray(bundle) ? bundle : [bundle]) if ('output' in item)
  for (const chunk of item.output) if (chunk.type === 'chunk' && chunk.isEntry) workerCode = chunk.code;
if (!workerCode) throw new Error('Missing worker bundle');
const server = createServer(async (request, response) => {
  try {
    const url = new URL(request.url!, 'http://localhost');
    if (url.pathname === '/') { response.writeHead(200, { 'Content-Type': 'text/html' }); response.end('<!doctype html><title>Camera benchmark</title>'); return; }
    if (url.pathname === '/camera-benchmark.js') { response.writeHead(200, { 'Content-Type': 'text/javascript' }); response.end(workerCode); return; }
    const path = resolve(staticRoot, '.' + decodeURIComponent(url.pathname)), rel = relative(staticRoot, path);
    if (isAbsolute(rel) || rel.startsWith('..')) throw new Error('Outside static directory');
    const bytes = await readFile(path);
    response.writeHead(200, { 'Content-Type': path.endsWith('.json') ? 'application/json' : 'application/octet-stream' }); response.end(bytes);
  } catch { response.writeHead(404); response.end('Missing benchmark asset'); }
});
await new Promise<void>(resolve => server.listen(0, '127.0.0.1', resolve));
const address = server.address();
if (!address || typeof address === 'string') throw new Error('Missing server address');
const browser = await chromium.launch({ headless: true });
type Run = { id: number; generationMs: number; scrutinyMs: number; error?: string; status?: string;
  seats?: number; distinctCandidates?: number; camera?: number; senato?: number; unfilled?: number; failures?: object[] };
type WorkerResult = { model: string; first: number; last: number; catalogId: string; loadMs: number; preparationMs: number; elapsedMs: number; runs: Run[] };
try {
  const page = await browser.newPage();
  await page.goto('http://127.0.0.1:' + address.port + '/');
  await page.exposeFunction('reportProgress', (value: unknown) => console.log(JSON.stringify(value)));
  const report = { schemaVersion: 1, browser: browser.version(), draws, workers: workerCount, started: new Date().toISOString(),
    qualification: 'Camera-only current default 24-list scenario, real generation/candidature/scrutiny in dedicated Chromium workers. Prepared fit loaded, not refitted. Historical research ran concurrently on this machine. Worker phase sums are accumulated worker wall time, not additive phases of campaign wall time. No browser UI/export acceptance.',
    workerCodeSha256: createHash('sha256').update(workerCode).digest('hex'), models: [] as object[] };
  for (const model of (process.env.CAMERA_BROWSER_MODELS ?? 'M01,M02').split(',')) {
    if (!['M01', 'M02'].includes(model)) throw new Error('Only current models permitted');
    const started = performance.now();
    const results = await page.evaluate(async ({ model, draws, workers }) => {
      const live: Worker[] = [];
      try {
        return await Promise.all(Array.from({ length: workers }, (_, i) => new Promise<WorkerResult>((resolve, reject) => {
          const worker = new Worker('/camera-benchmark.js', { type: 'module' }); live.push(worker);
          worker.onmessage = ({ data }) => {
            if (data.progress) void (window as unknown as { reportProgress: (value: unknown) => Promise<void> }).reportProgress(data);
            else if (data.fatal) reject(new Error(JSON.stringify(data)));
            else if (data.done) resolve(data);
          };
          worker.onerror = event => reject(new Error(event.message));
          worker.postMessage({ model, first: Math.floor(i * draws / workers) + 1, last: Math.floor((i + 1) * draws / workers) });
        })));
      } finally { live.forEach(w => w.terminate()); }
    }, { model, draws, workers: workerCount });
    const runs = results.flatMap(r => r.runs).sort((a, b) => a.id - b.id),
      failures = runs.filter(r => r.error), returned = runs.filter(r => !r.error),
      nonComputable = returned.filter(r => r.status === 'non-computable' || r.failures?.length),
      filled = returned.filter(r => r.status === 'complete' && r.seats === 392 && r.distinctCandidates === 392 && r.camera === 392 && r.senato === 0 && r.unfilled === 0);
    if (runs.length !== draws || runs.some((r, i) => r.id !== i + 1)) throw new Error('INCOMPLETE_ATTEMPT_LEDGER');
    const summary = { model, modelVersion: '1.0.0', draws, workers: workerCount, catalogId: results[0].catalogId,
      elapsedMs: performance.now() - started, loadMaxMs: Math.max(...results.map(r => r.loadMs)),
      preparationMaxMs: Math.max(...results.map(r => r.preparationMs)),
      generationWorkerMs: runs.reduce((n, r) => n + r.generationMs, 0), scrutinyWorkerMs: runs.reduce((n, r) => n + r.scrutinyMs, 0),
      exceptions: failures.map(r => ({ id: r.id, error: r.error })), nonComputable: nonComputable.map(r => ({ id: r.id, failures: r.failures })),
      returned: returned.length, filled392: filled.length, workersDetail: results.map(({ runs, ...r }) => r), runs };
    report.models.push(summary);
    await writeFile(resolve(output, 'future-' + draws + '-report.json'), JSON.stringify(report, null, 2) + '\n');
    console.log(JSON.stringify({ model, draws, minutes: summary.elapsedMs / 60000, filled: filled.length, exceptions: failures.length, nonComputable: nonComputable.length }));
    if (filled.length !== draws || summary.elapsedMs > 60 * 60 * 1000) process.exitCode = 1;
  }
} finally {
  await browser.close(); await new Promise<void>(resolve => server.close(() => resolve()));
}

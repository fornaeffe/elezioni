import { test, expect, type Browser, type Page } from '@playwright/test';
import { readFile, mkdir, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { spawn } from 'node:child_process';
import { cpus } from 'node:os';
import { build } from 'vite';
import { implementationKey } from '../../scripts/data-preparation/fast-share-release-io';
const base = process.env.BASE_PATH ?? '',
  directory = resolve('../data/derived/fv08-unified/resources'),
  bundles = new Map<string, string>();
let configuration: unknown;
test.beforeAll(async () => {
  await mkdir(directory, { recursive: true });
  configuration = {
    base,
    pointer: JSON.parse(
      await readFile(
        resolve('static/data/reform-2026/fast-share/candidate.json'),
        'utf8',
      ),
    ),
    implementations: Object.fromEntries(
      await Promise.all(
        Object.entries({
          generation: 'unified-generation',
          fitting: 'information',
          profiles: 'senate-adapter',
        }).map(async ([k, v]) => [
          k,
          await implementationKey(
            resolve('..'),
            'web/src/lib/politics/reform-2026/vote-share/' + v + '.ts',
          ),
        ]),
      ),
    ),
  };
  for (const [entry, name] of [
    ['fast-fit-harness.ts', 'fv08-fit.js'],
    ['unified-resource.worker.ts', 'fv08-resource.js'],
  ]) {
    const result = await build({
      configFile: false,
      logLevel: 'error',
      base: base + '/',
      worker: { format: 'es' },
      build: {
        write: false,
        minify: false,
        lib: {
          entry: resolve('tests/fixtures/' + entry),
          formats: ['es'],
          fileName: () => name,
        },
      },
    });
    for (const r of Array.isArray(result) ? result : [result])
      if ('output' in r)
        for (const out of r.output)
          bundles.set(
            base + '/' + out.fileName,
            out.type === 'chunk'
              ? out.code
              : typeof out.source === 'string'
                ? out.source
                : new TextDecoder().decode(out.source),
          );
  }
});
test.beforeEach(async ({ page }) => {
  await page.route('**/*', (r) => {
    const code = bundles.get(new URL(r.request().url()).pathname);
    return code === undefined
      ? r.continue()
      : r.fulfill({ contentType: 'application/javascript', body: code });
  });
  await page.goto(base + '/');
});
async function throttleWorker(browser: Browser) {
  // Chromium rejects CPU emulation on worker targets. Constrain the actual
  // private renderer processes instead, including the numerical worker thread.
  if (process.platform !== 'win32')
    throw new Error(
      'Quarter-CPU probe requires the Windows job-object harness on this acceptance machine',
    );
  const root = await browser.newBrowserCDPSession(),
    ids = (await root.send('SystemInfo.getProcessInfo')).processInfo
      .filter((p) => p.type === 'renderer')
      .map((p) => p.id);
  await root.detach();
  const child = spawn(
    'pwsh.exe',
    [
      '-NoProfile',
      '-File',
      resolve('tests/fixtures/limit-renderer-cpu.ps1'),
      '-ProcessIdsJson',
      JSON.stringify(ids),
      '-LogicalProcessors',
      String(cpus().length),
    ],
    { windowsHide: true },
  );
  let output = '',
    errors = '';
  child.stderr.on('data', (b) => {
    errors += b.toString();
  });
  const metadata = await new Promise<any>((done, reject) => {
    child.on('error', reject);
    child.on('exit', (code) => {
      if (!output.includes('\n'))
        reject(new Error('CPU limiter failed ' + code + ': ' + errors));
    });
    child.stdout.on('data', (b) => {
      output += b.toString();
      if (output.includes('\n')) {
        try {
          done(JSON.parse(output.split('\n')[0]));
        } catch (e) {
          reject(e);
        }
      }
    });
  });
  return {
    metadata,
    release: async () => {
      child.stdin.end();
      await new Promise<void>((done, reject) => {
        if (child.exitCode !== null)
          return child.exitCode === 0 ? done() : reject(new Error(errors));
        child.on('exit', (code) =>
          code === 0 ? done() : reject(new Error(errors)),
        );
      });
    },
  };
}
test('six full default refits retain deterministic states and measured latency', async ({
  page,
}) => {
  const runs = [];
  for (let i = 0; i < 6; i++) {
    const result = await page.evaluate(
      async ({ base, configuration }) => {
        const h = await import(/* @vite-ignore */ base + '/fv08-fit.js'),
          client = new h.ReformFitClient(configuration);
        try {
          const load = await client.load(),
            s = client.copyDraft();
          s.referenceDate = '2026-10-20';
          client.setDraft(s);
          const started = performance.now(),
            fit = await client.fit();
          return {
            milliseconds: performance.now() - started,
            load: load.audit,
            fit: fit.audit,
            scenario: client.copyDraft().data,
            phase: client.state.phase,
          };
        } finally {
          client.dispose();
        }
      },
      { base, configuration },
    );
    runs.push(result);
    await writeFile(
      resolve(directory, 'default-six-refits.json'),
      JSON.stringify({ base, configuration, runs }, null, 2) + '\n',
    );
    expect(result.load.nationalFits).toBe(0);
    expect(result.fit).toMatchObject({
      nationalFits: 1,
      profileFits: 1,
      pairedFits: 1,
    });
    expect(result.phase).toBe('ready');
    expect(result.milliseconds).toBeLessThan(30000);
  }
  expect(new Set(runs.map((r) => r.scenario.fittedStateId)).size).toBe(1);
});
test('maximum 128-component/512-poll worker and full municipal profile probe, including real worker 4x throttle', async ({
  page,
  browser,
}) => {
  const runs = [];
  for (const [index, rate] of [1, 1, 1, 1, 1, 1, 4].entries()) {
    const workerCreated = page.waitForEvent('worker');
    await page.evaluate((root) => {
      (window as any).resourceWorker = new Worker(root + '/fv08-resource.js', {
        type: 'module',
      });
    }, base);
    await workerCreated;
    const cdp = rate === 4 ? await throttleWorker(browser) : undefined;
    const result = await page.evaluate(async () => {
      const w = (window as any).resourceWorker;
      try {
        return await new Promise<any>((done, reject) => {
          w.onmessage = (e: MessageEvent) => done(e.data);
          w.onerror = (e: ErrorEvent) => reject(new Error(e.message));
          w.postMessage({ components: 128, polls: 512, municipalities: 7904 });
        });
      } finally {
        w.terminate();
      }
    }).finally(()=>cdp?.release());
    runs.push({
      index,
      rate,
      ...(cdp ? { throttling: cdp.metadata } : {}),
      ...result,
    });
    await writeFile(
      resolve(directory, 'maximum-worker.json'),
      JSON.stringify(
        {
          base,
          qualification:
            'Synthetic engineering dimensions, not a claim of identifiable political splits or statistical coverage.',
          runs,
        },
        null,
        2,
      ) + '\n',
    );
    expect(result.ok).toBe(true);
    expect(result.totalMs).toBeLessThan(rate === 4 ? 120000 : 30000);
    expect(result.workspaceBytes).toBeLessThanOrEqual(256 * 1024 ** 2);
    if(rate===4){const baseline=runs.slice(0,6).map(r=>r.totalMs).sort((a,b)=>a-b)[3];expect(result.totalMs/baseline).toBeGreaterThan(2.5);}
  }
});

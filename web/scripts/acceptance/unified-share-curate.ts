/** Retain compact, current-code FV-08 evidence only after every required gate. */
import { readFile, writeFile, mkdir } from 'node:fs/promises';
import { createReadStream } from 'node:fs';
import { createHash } from 'node:crypto';
import { resolve, dirname } from 'node:path';
import { loadCandidate } from '../../src/lib/politics/reform-2026/vote-share/release';
import { readBytes } from '../data-preparation/fast-share-release-io';
import { requireBrowserAcceptance } from './reports';
const root = resolve('..'),
  output = resolve(
    root,
    'test/fixtures/benchmarks/vote_share_unified_acceptance.json',
  ),
  directory = resolve(root, 'data/derived/fv08-unified'),
  json = async (path: string) => JSON.parse(await readFile(path, 'utf8'));
const pointer = await json('static/data/reform-2026/fast-share/candidate.json'),
  loaded = await loadCandidate(
    pointer,
    readBytes('static/data/reform-2026/fast-share'),
    { validateGeneration: false },
  );
const acceptance = await json(
  resolve(root, 'data/derived/r26-21-acceptance/report.json'),
);
if (acceptance.status !== 'passed')
  throw new Error('Complete root/project application acceptance is required');
const resource = await json(
    resolve(directory, 'resources/default-six-refits.json'),
  ),
  maximum = await json(resolve(directory, 'resources/maximum-worker.json'));
if (
  resource.configuration.pointer.catalogId !== pointer.catalogId ||
  resource.runs.length !== 6 ||
  resource.runs.some((r: any) => r.phase !== 'ready' || r.milliseconds > 30000)
)
  throw new Error('Current complete six-refit acceptance is required');
if (
  maximum.runs.length !== 7 ||
  maximum.runs.some(
    (r: any) => !r.ok || r.totalMs > (r.rate === 4 ? 120000 : 30000),
  )
)
  throw new Error('Maximum-dimension/throttled browser acceptance is required');
const platformPath = (path: string) =>
  process.platform === 'win32'
    ? path.replace(
        /^\/mnt\/([a-z])\//,
        (_, drive) => drive.toUpperCase() + ':/',
      )
    : path;
const isolated = await json(
    resolve(root, 'data/derived/fv05-acceptance/isolated-acceptance.json'),
  ),
  checkout = platformPath(isolated.checkout),
  cold = await json(resolve(checkout, 'cold.json')),
  warm = await json(resolve(checkout, 'current-source-warm.json'));
if (
  isolated.status !== 'passed' ||
  cold.environment.platform !== 'linux' ||
  cold.catalog !== pointer.catalogId ||
  warm.catalog !== pointer.catalogId ||
  !Object.values(warm.cache).every(Boolean)
)
  throw new Error(
    'Matching cold Linux / current-source offline warm preparation is required',
  );
const browserChecks: Record<string, unknown> = {};
for (const suite of ['resource','campaign'])
  browserChecks[suite] = requireBrowserAcceptance(
    await json(resolve(directory, suite + '-tests.json')),
  );
for (const base of ['root', 'nested'])
  for (const suite of ['release', 'ui'])
    browserChecks[suite + '-' + base] = requireBrowserAcceptance(
      await json(resolve(directory, suite + '-tests-' + base + '.json')),
    );
const campaigns = [];
for (const model of ['selected', 'legacy-poll-informed-v1']) {
  const path = resolve(
      directory,
      'browser/chromium-' + model + '-nested-1000.json.gz',
    ),
    receipt = await json(path + '.receipt.json');
  if (
    receipt.phase === 'computed' ||
    receipt.model !== model ||
    receipt.draws !== 1000 ||
    receipt.runs.length !== 1000 ||
    receipt.generationImplementationId !==
      loaded.catalog.generationImplementationId ||
    receipt.data.sourceBundleId !== loaded.catalog.sourceId ||
    receipt.pageErrors.length ||
    !receipt.memory.renderer?.supported ||
    receipt.elapsedMs >= 3600000 ||
    receipt.runs.some(
      (r: any, i: number) =>
        r.simulation !== i + 1 ||
        r.status !== 'complete' ||
        r.seats !== 588 ||
        r.distinctCandidates !== 588 ||
        r.camera !== 392 ||
        r.senato !== 196 ||
        r.unfilled,
    )
  )
    throw new Error('Incomplete or stale ' + model + ' browser campaign');
  const hash = createHash('sha256');
  for await (const chunk of createReadStream(path)) hash.update(chunk);
  campaigns.push({
    model,
    basePath: receipt.base,
    requested: 1000,
    complete: 1000,
    filledSeatsPerRun: 588,
    distinctCandidatesPerRun: 588,
    workers: receipt.workers,
    deviceHints: receipt.deviceHints,
    concurrentModelCampaigns: receipt.concurrentModelCampaigns,
    browser: receipt.browserVersion,
    elapsedMs: receipt.elapsedMs,
    benchmark: receipt.benchmark,
    archiveBytes: receipt.archiveBytes,
    archiveSha256: hash.digest('hex'),
    ledgerSha256: createHash('sha256')
      .update(JSON.stringify(receipt.runs))
      .digest('hex'),
    memory: {
      supported: receipt.memory.supported,
      peakBytes: receipt.memory.peakBytes,
      qualification: receipt.memory.qualification,
      renderer: {
        supported: receipt.memory.renderer.supported,
        samples: receipt.memory.renderer.samples,
        peakWorkingSetBytes: receipt.memory.renderer.peakWorkingSetBytes,
        peakPrivateBytes: receipt.memory.renderer.peakPrivateBytes,
        qualification: receipt.memory.renderer.qualification,
      },
    },
    bonusAwarded: receipt.runs.filter((r: any) => r.bonus).length,
    seatDistributions: receipt.seatDistributions,
    caps: receipt.caps,
    admission: receipt.admission,
    firstSubjectThresholds: receipt.thresholds,
    voteShares: receipt.voteShares,
  });
}
const result = {
  stage: 'FV-08',
  status: 'passed',
  createdAt: new Date().toISOString(),
  catalogId: pointer.catalogId,
  sourceId: loaded.catalog.sourceId,
  generationImplementationId: loaded.catalog.generationImplementationId,
  statisticalDefault: 'selected',
  alternative: 'legacy-poll-informed-v1',
  scope:
    'Current v5 reform workflow; conditional domestic simulations, not forecast calibration or public deployment.',
  applicationChecks: acceptance.checks,
  browserChecks,
  campaigns,
  refit: {
    runs: resource.runs.map((r: any) => ({
      milliseconds: r.milliseconds,
      load: r.load,
      fit: r.fit,
    })),
    maximum: maximum.runs.map((r: any) => ({
      index: r.index,
      rate: r.rate,
      nationalMs: r.nationalMs,
      profileMs: r.profileMs,
      totalMs: r.totalMs,
      workspaceBytes: r.workspaceBytes,
    })),
  },
  preparation: {
    platform: cold.environment.platform,
    coldMilliseconds: cold.elapsedMs,
    warmMilliseconds: warm.elapsedMs,
    identicalCatalogs: true,
  },
  decisions: {
    legacyNewListClassification:
      'A list is new when it has no mapped historical baseline in the current renewed chamber. Luca approved this per-chamber classification on 31 October and did not require another acceptance round.',
    memoryCeiling:
      '512 MiB aggregate ceiling waived; CPU selector, half-declared-memory recommendation bounded by browser-heap allowance, and independent device/browser memory warnings.',
    webkit:
      'Windows WebKit export capability deferred by Luca; no Safari export acceptance claimed.',
    policyAudit:
      'Post-FV-08 incomplete-destination, unfiled-support and physical-capacity audit remains open.',
  },
};
await mkdir(dirname(output), { recursive: true });
await writeFile(output, JSON.stringify(result, null, 2) + '\n');
console.log(output);

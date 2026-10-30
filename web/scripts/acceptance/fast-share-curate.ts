/** Retain a failed FV-08 screen for review. This never selects a default. */
import { readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { gzipSync } from 'fflate';
const root = resolve('..'),
  raw = resolve(root, 'data/derived/fv08-acceptance');
const read = async (name: string) =>
  JSON.parse(await readFile(resolve(raw, name), 'utf8'));
const ledger = await read('production/node-1000.json');
if (
  ledger.requested !== 1000 ||
  ledger.completed !== 1000 ||
  ledger.runs.length !== 1000 ||
  ledger.runs.some(
    (r: any, i: number) => r.index !== i || r.seed !== 'fv08-production-' + i,
  )
)
  throw new Error('Incomplete or changed frozen-seed production ledger');
const failed = ledger.runs.filter((r: any) => r.error),
  successful = ledger.runs.filter((r: any) => !r.error);
if (!failed.length)
  throw new Error(
    'This curator retains the failed FV-08 screen only; passing acceptance needs the complete frozen gates',
  );
if (failed.length !== ledger.technicalErrors)
  throw new Error('Failure ledger mismatch');
const share = failed.length / 1000,
  z = 1.959963984540054,
  center = (share + (z * z) / 2000) / (1 + (z * z) / 1000),
  radius =
    (z * Math.sqrt((share * (1 - share)) / 1000 + (z * z) / 4000000)) /
    (1 + (z * z) / 1000);
const resources = await read('node-resources/128-512.json'),
  replay = await read('replay/report.json');
const browsers = [];
for (const base of ['root', 'nested']) {
  const report = await read('browser/ui-' + base + '.json');
  const results: object[] = [];
  const visit = (suite: any) => {
    for (const spec of suite.specs ?? [])
      for (const test of spec.tests ?? [])
        results.push({
          title: spec.title,
          browser: test.projectName,
          status: test.status,
          attempts: test.results.map((r: any) => ({
            status: r.status,
            durationMs: r.duration,
            error: r.error?.message,
          })),
        });
    for (const child of suite.suites ?? []) visit(child);
  };
  for (const suite of report.suites) visit(suite);
  browsers.push({ base, stats: report.stats, results });
}
const hashes: Record<string, string> = {};
const names = new Set([
  ...execFileSync('git', ['diff', '--name-only', 'HEAD', '-z'], {
    cwd: root,
    encoding: 'utf8',
  }).split('\0'),
  ...execFileSync('git', ['ls-files', '--others', '--exclude-standard', '-z'], {
    cwd: root,
    encoding: 'utf8',
  }).split('\0'),
]);
for (const name of [...names].sort())
  if (
    (name.startsWith('web/') || name.startsWith('data/')) &&
    !name.endsWith('.md')
  )
    hashes[name] = createHash('sha256')
      .update(
        (await readFile(resolve(root, name), 'utf8')).replaceAll('\r\n', '\n'),
      )
      .digest('hex');
const packed = gzipSync(new TextEncoder().encode(JSON.stringify(ledger)), {
  level: 9,
  mtime: 0,
});
const artifact = 'vote_share_acceptance_runs.json.gz';
await writeFile(resolve(root, 'test/fixtures/benchmarks', artifact), packed);
const currentCapacity = await read('production/capacity.json');
const report = {
  schemaVersion: 1,
  stage: 'FV-08',
  recordedDate: '2026-10-30',
  status: 'blocked-no-default-activation',
  baseRevision: execFileSync('git', ['rev-parse', 'HEAD'], {
    cwd: root,
    encoding: 'utf8',
  }).trim(),
  sourceHashPolicy:
    'SHA-256 of LF-normalized current changed source files; earlier run catalogs are explicitly separate',
  sourceHashes: hashes,
  productionScreen: {
    scope: ledger.scope,
    catalogId: ledger.catalogId,
    requested: 1000,
    successful: successful.length,
    technicalErrors: failed.length,
    legalNonComputableAmongGenerated: ledger.nonComputable,
    overallUnsuccessful: failed.length + ledger.nonComputable,
    failureShare: share,
    failureShareWilson95: [center - radius, center + radius],
    failedIndices: failed.map((r: any) => r.index),
    errorCounts: failed.reduce(
      (a: any, r: any) => ((a[r.error] = (a[r.error] ?? 0) + 1), a),
      {},
    ),
    elapsedMs: ledger.elapsedMs,
    allSuccessfulRunsFilled588: successful.every(
      (r: any) => r.seats === 588 && r.filled === 588,
    ),
    maximumSuccessfulProjectionResidual: Math.max(
      ...successful.map((r: any) => r.projectionResidual),
    ),
    ledger: {
      path: artifact,
      sha256: createHash('sha256').update(packed).digest('hex'),
      compressedBytes: packed.byteLength,
    },
    qualification:
      'Earlier candidate catalog, before dense residual refinement and diagnostic additions. Not final current-engine or browser storage/export acceptance. No draw is rerolled or removed.',
  },
  currentCapacityRepeats: currentCapacity,
  nodeResourceProbe: {
    ...resources,
    qualification:
      'Synthetic national fit only; no empirical geographic refit, browser, worker throttle or aggregate-memory acceptance. Concurrent independent validation was running.',
  },
  replay: {
    ...replay,
    qualification:
      'Point comparisons only; simulation MCSE, regional/cluster sensitivity and full-country benchmark remain open.',
  },
  linux: {
    cold: await read('linux-cold.json'),
    warm: await read('linux-warm.json'),
    qualification:
      'Source-only Linux/WSL preparation, no research directory or R/Python dependency. Catalog identity must be compared with the current Windows release; earlier run catalogs remain separately identified.',
  },
  windowsReleases: {
    root: await read('release-root.json'),
    nested: await read('release-nested.json'),
  },
  browsers,
  sampledChromiumMemory: await Promise.all(
    ['root', 'nested'].map(async (base) => {
      const measured = await read('browser/chromium-' + base + '.json');
      return {
        base,
        browserVersion: measured.browserVersion,
        catalogId: measured.catalogId,
        ...measured.memory,
      };
    }),
  ),
  openGates: [
    'Restricted-filing forecast model decision and new full-default validation',
    'Predictive chain-block MCSE, regional/full-country/installed-generator historical comparisons',
    '1000 complete paired browser storage/plot/export/import simulations and matched seat/candidate comparison',
    'Complete default/max-envelope browser cold/warm, 4x worker throttle, cancellation and concurrent-memory envelope',
    'WebKit export capability validation on a supported OPFS-enabled target; Windows WebKit export failed',
  ],
};
const details = gzipSync(new TextEncoder().encode(JSON.stringify(report)), {
    level: 9,
    mtime: 0,
  }),
  detailsPath = 'vote_share_acceptance_details.json.gz';
await writeFile(
  resolve(root, 'test/fixtures/benchmarks', detailsPath),
  details,
);
const releaseSummary = (r: any) => ({
  catalog: r.catalog,
  environment: r.environment,
  elapsedMs: r.elapsedMs,
  cache: r.cache,
  byteBudget: r.byteBudget,
  verification: Object.fromEntries(
    Object.entries(r.verification).filter(
      ([key]) => !key.endsWith('Diagnostics'),
    ),
  ),
});
const summary = {
  ...report,
  details: {
    path: detailsPath,
    sha256: createHash('sha256').update(details).digest('hex'),
    compressedBytes: details.byteLength,
  },
  nodeResourceProbe: {
    ...resources,
    runs: resources.runs.map((r: any) => ({
      run: r.run,
      cold: r.cold,
      milliseconds: r.milliseconds,
      workspaceBytes: r.workspaceBytes,
      diagnosticsPassed: r.political?.passed && r.participation?.passed,
      error: r.error,
    })),
    qualification: report.nodeResourceProbe.qualification,
  },
  replay: {
    status: replay.status,
    requested: replay.requested,
    variantsCompleted: replay.cases.length,
    errors: replay.cases.filter((c: any) => c.error),
    draws: replay.draws,
    generationImplementationId: replay.generationImplementationId,
    maximumProjectionResidual: Math.max(
      ...replay.cases
        .filter((c: any) => !c.error)
        .map((c: any) => c.maximumResidual),
    ),
    qualification: report.replay.qualification,
  },
  linux: {
    cold: releaseSummary(report.linux.cold),
    warm: releaseSummary(report.linux.warm),
    qualification: report.linux.qualification,
  },
  windowsReleases: {
    root: releaseSummary(report.windowsReleases.root),
    nested: releaseSummary(report.windowsReleases.nested),
  },
  sampledChromiumMemory: report.sampledChromiumMemory.map(
    ({ rows, ...measurement }) => measurement,
  ),
};
await writeFile(
  resolve(root, 'test/fixtures/benchmarks/vote_share_acceptance.json'),
  JSON.stringify(summary, null, 2) + '\n',
);
console.log(
  JSON.stringify({
    status: report.status,
    requested: 1000,
    technicalErrors: failed.length,
    artifact,
  }),
);

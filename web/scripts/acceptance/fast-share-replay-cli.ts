import { readFile, mkdir, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { createHash } from 'node:crypto';
import { gzipSync } from 'fflate';
import { replay, type ReplaySource } from './fast-share-replay';
const limit = Number(process.argv[2] ?? 24);
const offset = Number(process.argv[3] ?? 0);
if (
  !Number.isSafeInteger(offset) ||
  offset < 0 ||
  offset > 23 ||
  (limit === 24 && offset !== 0)
)
  throw new Error(
    'A case offset is supported only for a diagnostic smoke run.',
  );
if (![1, 24].includes(limit))
  throw new Error('Use 1 (smoke) or 24 (complete frozen panel).');
const root = resolve('..'),
  directory = resolve(root, 'data/derived/fv08-acceptance/replay');
await mkdir(directory, { recursive: true });
const cases = [] as any[];
for (const phase of ['development', 'confirmation']) {
  const path = resolve(
    root,
    'research/vote-generation/fv01/evidence/data/derived/party-drift/fast-' +
      phase +
      '-final-20261027/cases.json',
  );
  const raw = await readFile(path, 'utf8'),
    archive = JSON.parse(raw);
  if (archive.draws !== 512)
    throw new Error('Frozen replay draw budget mismatch');
  for (const c of archive.cases.filter(
    (c: any) => c.variant === 'kind-common-conservative',
  ))
    cases.push({
      ...c,
      phase,
      comparatorHash: createHash('sha256').update(raw).digest('hex'),
    });
}
if (cases.length !== 24) throw new Error('Frozen 16/8 panel incomplete');
const report = {
  stage: 'FV-08',
  startedAt: new Date().toISOString(),
  generationImplementationId: await (
    await import('../data-preparation/fast-share-release-io')
  ).implementationKey(
    root,
    'web/src/lib/politics/reform-2026/vote-share/generation.ts',
  ),
  status:
    'point-comparison-only; 1000-replicate simulation MCSE required before acceptance',
  draws: 512,
  requested: limit,
  cases: [] as object[],
};
for (const c of cases.slice(offset, offset + limit)) {
  const name = c.inputDirectory.split(/[\\/]/).at(-1),
    sourceDirectory = resolve(root, 'data/derived/party-drift', name),
    manifest = JSON.parse(
      await readFile(resolve(sourceDirectory, 'manifest.json'), 'utf8'),
    ),
    raw = await readFile(resolve(sourceDirectory, 'profiles.json'));
  if (createHash('sha256').update(raw).digest('hex') !== manifest.profileHash)
    throw new Error('Pinned replay source changed: ' + name);
  const source = JSON.parse(raw.toString()) as ReplaySource,
    seed = 'fv08-replay:' + c.target + ':' + c.horizon;
  for (const availability of ['reference', 'future'] as const) {
    const start = performance.now();
    try {
      const result = replay(source, seed, availability),
        artifact = c.target + '-' + c.horizon + '-' + availability + '.json.gz';
      await writeFile(
        resolve(directory, artifact),
        gzipSync(new TextEncoder().encode(JSON.stringify(result.metrics))),
      );
      const baseline = {
        national: c.forecast.nationalScores.validShares,
        municipal: c.forecast.scores.municipalValidShares,
      };
      report.cases.push({
        phase: c.phase,
        target: c.target,
        horizon: c.horizon,
        availability,
        seed,
        sourceHash: manifest.profileHash,
        comparatorHash: c.comparatorHash,
        milliseconds: performance.now() - start,
        scores: result.scores,
        baseline,
        pointDifferences: Object.fromEntries(
          ['national', 'municipal'].map((scope) => [
            scope,
            Object.fromEntries(
              ['coverage90', 'meanMae', 'wis'].map((metric) => [
                metric,
                (result.scores as any)[scope][metric] -
                  baseline[scope as keyof typeof baseline][metric],
              ]),
            ),
          ]),
        ),
        maximumResidual: result.maximumResidual,
        maximumIterations: result.maximumIterations,
        artifact,
      });
    } catch (error) {
      report.cases.push({
        phase: c.phase,
        target: c.target,
        horizon: c.horizon,
        availability,
        error: String(error),
      });
      process.exitCode = 1;
    }
    await writeFile(
      resolve(
        directory,
        limit === 24 ? 'report.json' : 'report-smoke-' + offset + '.json',
      ),
      JSON.stringify(report, null, 2) + '\n',
    );
    console.log(JSON.stringify(report.cases.at(-1)));
  }
}

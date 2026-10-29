/** Reduced numerical acceptance and synthetic scaling only. No bundle publication. */
import { mkdir, writeFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { resolve } from 'node:path';
import { cpus, platform, release } from 'node:os';
import reference from '../../tests/fixtures/fast-share-reference.generated.json' with { type: 'json' };
import { runNumericalGate } from '../../tests/fixtures/fast-share-gates';
import { reducedShareInput } from '../../tests/fixtures/fast-share-input';
import { syntheticNational, syntheticProfiles } from '../../tests/fixtures/fast-share-scale';
import { buildPoliticalTarget, fitReference, fitProfiles, sampleGaussian, FAST_SHARE_MODEL, FAST_SHARE_NUMERICS } from '../../src/lib/politics/reform-2026/vote-share/index';

const root = fileURLToPath(new URL('../../../', import.meta.url)), output = resolve(root, 'data/derived/fv02-numerics');
const numerical = [];
for (const fixture of reference.quadrature) for (let i = 0; i < 3; i++) {
  const started = performance.now(), seed = 'fv02-numeric-' + i, result = runNumericalGate(fixture, seed);
  numerical.push({ fixture: fixture.id, seed, milliseconds: performance.now() - started, ...result.comparison });
  if (!result.comparison.passed) throw new Error('Reduced independent posterior gate failed: ' + fixture.id);
}
console.log(JSON.stringify({ stage: 'reduced-numerical-gates', passed: true, cases: numerical.length }));
const input = reducedShareInput(), countryStarted = performance.now(), country = fitReference(input.national, 'fv02-reference');
const countryMilliseconds = performance.now() - countryStarted;
const profileRuns = [];
const profileInput = syntheticProfiles(28, 128);
for (let i = 0; i < 6; i++) {
  const start = performance.now(), result = fitProfiles(profileInput);
  profileRuns.push({ run: i, milliseconds: performance.now() - start, workspaceBytes: result.audit.workspaceBytes });
}
const probes = [];
for (const components of [28, 64, 128]) for (const polls of [0, 128, 512]) {
  const started = performance.now(), source = syntheticNational(components, polls), fit = buildPoliticalTarget(source);
  const constructionMilliseconds = performance.now() - started, sampleStarted = performance.now();
  const pilot = sampleGaussian(fit.target, { seed: 'fv02-profile-pilot', draws: 64, burn: 100 });
  const pilotMilliseconds = performance.now() - sampleStarted;
  probes.push({ components, polls, contrasts: fit.audit.observations, constructionMilliseconds,
    workspaceBytes: fit.audit.workspaceBytes, covariance: fit.audit.iterations,
    pilot: { retainedDrawsPerChain: 64, burn: 100, milliseconds: pilotMilliseconds,
      acceptedFit: false, reason: 'Short computation pilot, not the frozen inference budget' },
    estimated2000DrawFitMilliseconds: constructionMilliseconds + pilotMilliseconds * (1000 + 2000 * 2) / (100 + 64 * 2) });
  console.log(JSON.stringify({ stage: 'synthetic-construction-and-pilot', components, polls,
    milliseconds: constructionMilliseconds, workspaceBytes: fit.audit.workspaceBytes, retainedValues: pilot.values.length }));
}
const maximumMeanError = Math.max(...numerical.flatMap(r => r.coordinates.map(c => c.meanError)));
const maximumEndpointError = Math.max(...numerical.flatMap(r => r.coordinates.flatMap(c => c.endpointErrors)));
const sortedWarm = profileRuns.slice(1).map(r => r.milliseconds).sort((a, b) => a - b);
const report = { schemaVersion: 1, model: FAST_SHARE_MODEL, numericalVersion: FAST_SHARE_NUMERICS,
  scope: 'FV-02 reduced numerical acceptance and synthetic extrapolation; no production scenario acceptance',
  environment: { node: process.version, platform: platform(), release: release(), cpu: cpus()[0]?.model },
  numerical, maximumMeanError, maximumEndpointError,
  reducedCountry: { milliseconds: countryMilliseconds, political: country.political.diagnostics,
    participation: country.participation.diagnostics, workspaceBytes: country.audit.workspaceBytes },
  profiles: { municipalities: 128, components: 28, runs: profileRuns,
    historicalEvents: profileInput.elections.length,
    historicalRows: profileInput.elections.reduce((n, e) => n + e.rows.length, 0),
    warmMedianMilliseconds: sortedWarm[2], warmMaximumMilliseconds: sortedWarm.at(-1),
    estimated8194UnitMilliseconds: sortedWarm[2] * 8194 / 128,
    estimated8194UnitWorkspaceBytes: Math.ceil(profileRuns[0].workspaceBytes * 8194 / 128),
    extrapolationQualification: 'Holds four-event synthetic history fixed; not a complete real-history fit or browser peak-memory measurement' },
  probes, memory: { end: process.memoryUsage(), processPeakRssKiB: process.resourceUsage().maxRSS,
    qualification: 'Node process RSS includes harness/fixtures; workspace counts are allocation estimates, not browser process peaks' },
  limitations: ['Synthetic dimensions do not validate the full real source catalog or paired Senate',
    'Full-country fitting, posterior retention and browser aggregate memory remain later acceptance gates',
    'Short sampler pilots are not accepted posterior fits; extrapolations assume linear sweep cost'] };
await mkdir(output, { recursive: true });
await writeFile(resolve(output, 'node-report.json'), JSON.stringify(report, null, 2) + '\n');
console.log(JSON.stringify({ passed: true, maximumMeanError, maximumEndpointError, report: resolve(output, 'node-report.json') }));

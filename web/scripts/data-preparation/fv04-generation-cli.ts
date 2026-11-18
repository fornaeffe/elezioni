/** Headless FV-04 acceptance, deliberately independent of installed defaults. */
import { mkdir, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { cpus, platform, release } from 'node:os';
import { betaTransition } from '../../src/lib/politics/reform-2026/vote-share/transition';
import { fitRandom } from '../../src/lib/politics/reform-2026/vote-share/random';
import {
  calibrateReferences,
  drawReference
} from '../../src/lib/politics/reform-2026/vote-share/reference-family';
import { forecastRandom } from '../../src/lib/politics/reform-2026/vote-share/forecast-random';
import { referenceMomentFixtures, tensorReferenceMoments } from '../../tests/fixtures/fast-reference-oracle';
import { fastGenerationFixture } from '../../tests/fixtures/fast-generation-input';
import { prepareFastGeneration } from '../../src/lib/politics/reform-2026/vote-share/generation';
import { runPoliticsReformScrutiny } from '../../src/lib/politics/reform-2026/scrutiny';
const assert = (value: boolean, reason: string) => {
  if (!value) throw new Error(reason);
};
const started = performance.now(),
  initial = [0.3, 0.25, 0.2, 0.1, 0.08, 0.07],
  budget = 120000,
  rng = fitRandom('fv04-transition', 'moment');
const sums = initial.map(() => 0),
  squares = initial.map(() => 0);
let extreme = 0;
for (let i = 0; i < budget; i++) {
  const p = betaTransition(
    initial,
    initial.map(() => 0.4),
    2,
    rng
  );
  if (Math.max(...p) > 0.8) extreme++;
  p.forEach((x, j) => {
    sums[j] += x;
    squares[j] += x * x;
  });
}
const martingale = initial.map((p, j) => {
  const mean = sums[j] / budget,
    mcse = Math.sqrt((squares[j] / budget - mean * mean) / budget),
    error = Math.abs(mean - p);
  assert(error < 0.002 && error <= 4 * mcse, 'Martingale mean gate ' + j);
  return { mean, expected: p, error, mcse };
});
assert(extreme / budget <= 0.005, 'Implausible concentration fixture gate');
console.log('Passed 120000-draw finite-step martingale and concentration gates.');
const referenceReports = [];
for (const fixture of referenceMomentFixtures) {
  const family = calibrateReferences(
    fixture.means.map((mean, i) => ({
      id: 'g' + i,
      members: ['p' + i],
      mean,
      sdPercentagePoints: fixture.sd[i]
    })),
    fixture.free
  );
  const low = tensorReferenceMoments(family.mu, family.sigma, 40),
    high = tensorReferenceMoments(family.mu, family.sigma, 56);
  fixture.means.forEach((m, i) => {
    assert(
      Math.abs(high.mean[i] - m) <= 1e-6 && Math.abs(high.sd[i] - fixture.sd[i] / 100) <= 1e-6,
      'Independent reference moments'
    );
    assert(
      Math.abs(low.mean[i] - high.mean[i]) < 1e-7 && Math.abs(low.sd[i] - high.sd[i]) < 1e-7,
      'Independent integration refinement'
    );
  });
  const seeds = [];
  for (let seed = 0; seed < 3; seed++) {
    const random = forecastRandom('fv04-reference-' + seed, 1, fixture.id),
      n = 50000,
      draws = fixture.means.map(() => new Float64Array(n)),
      total = fixture.means.map(() => 0);
    for (let i = 0; i < n; i++) {
      const value = drawReference(family, random);
      value.values.forEach((x, j) => {
        draws[j][i] = x;
        total[j] += x;
      });
    }
    const moments = draws.map((xs, j) => {
      const mean = total[j] / n;
      let second = 0,
        fourth = 0;
      for (const x of xs) {
        const d = x - mean;
        second += d * d;
        fourth += d ** 4;
      }
      const variance = second / n,
        sd = Math.sqrt(variance),
        meanMcse = sd / Math.sqrt(n),
        sdMcse = Math.sqrt(Math.max(0, fourth / n - variance * variance) / (4 * n * variance));
      assert(
        Math.abs(mean - fixture.means[j]) <= 4 * meanMcse,
        'Reference sampled mean ' + fixture.id + '/' + seed + '/' + j
      );
      assert(
        Math.abs(sd - fixture.sd[j] / 100) <= 4 * sdMcse,
        'Reference sampled SD ' + fixture.id + '/' + seed + '/' + j
      );
      return { mean, sd, meanMcse, sdMcse };
    });
    seeds.push({ seed: 'fv04-reference-' + seed, draws: n, moments });
  }
  referenceReports.push({ id: fixture.id, family, independent: high, seeds });
  console.log('Passed joint reference moments: ' + fixture.id);
}
const f = fastGenerationFixture(),
  times: number[] = [],
  dependence = [],
  firstListId = f.scenario.lists.camera[0].id;
for (const rho of [0.5, 0.85, 0.98]) {
  const context = structuredClone(f.context),
    p = context.models.chambers.profiles.find((p) => p.id === f.scenario.chamberProfileId)!;
  p.sharedVariance = rho;
  p.residualCorrelation = 0;
  const prepared = prepareFastGeneration(f.scenario, f.assets, context),
    draws = [],
    legal = [];
  let nonComputable = 0,
    bonusCamera = 0,
    bonusSenato = 0;
  for (let i = 1; i <= 32; i++) {
    const start = performance.now(),
      result = prepared.run(i);
    times.push(performance.now() - start);
    draws.push({ camera: result.audit.countryForecast.camera, senato: result.audit.countryForecast.senato });
    const scrutiny = runPoliticsReformScrutiny(result.input);
    if (scrutiny.stages.some((s) => s.status === 'non-computable')) nonComputable++;
    legal.push({
      simulationId: i,
      status: scrutiny.status,
      bonusStatus: scrutiny.bonus.status,
      awards: scrutiny.bonus.awards.map((a) => ({ chamber: a.chamber, subject: a.subject })),
      seatCounts:
        scrutiny.status === 'non-computable'
          ? null
          : ['camera', 'senato'].map((ch) => ({
              chamber: ch,
              filled: scrutiny.seats.filter((s) => s.entitlement.chamber === ch && s.status === 'filled')
                .length,
              firstListId,
              listSeats: scrutiny.seats.filter(
                (s) => s.entitlement.chamber === ch && s.entitlement.listId === firstListId
              ).length,
              unfilled: scrutiny.seats.filter((s) => s.entitlement.chamber === ch && s.status !== 'filled')
                .length
            }))
    });
    const c = result.audit.countryForecast.camera!,
      s = result.audit.countryForecast.senato!;
    if ((c[1] + c[2]) / (1 - c[0]) >= 0.42) bonusCamera++;
    if ((s[1] + s[2]) / (1 - s[0]) >= 0.42) bonusSenato++;
  }
  dependence.push({
    latentCorrelation: rho,
    requested: 32,
    nonComputable,
    firstListId,
    cameraFirstListOver42: bonusCamera,
    senatoFirstListOver42: bonusSenato,
    draws,
    legal
  });
}
times.sort((a, b) => a - b);
const cells = f.assets.geography.cells.length,
  report = {
    schemaVersion: 1,
    step: 'FV-04',
    accepted: true,
    environment: { node: process.version, platform: platform(), release: release(), cpu: cpus()[0].model },
    martingale: { draws: budget, horizon: 2, sigma: 0.4, moments: martingale, over80Rate: extreme / budget },
    references: referenceReports,
    reduced: {
      physicalCells: cells,
      components: f.scenario.components.length,
      generationMedianMs: times[Math.floor(times.length / 2)],
      generationMaxMs: times.at(-1),
      estimated7904MunicipalityMs: (times[Math.floor(times.length / 2)] * 15810) / cells,
      qualification:
        'Linear conservative planning estimate includes fixed candidate/legal assembly; not a measured full-country/default latency or validation of the maximum component envelope.'
    },
    dependence,
    qualification:
      '32 paired synthetic runs per dependence assumption show sensitivity and retain all legal failures. Production predictive/1000-run seat acceptance belongs to FV-08.',
    elapsedMs: performance.now() - started,
    peakRssKiB: process.resourceUsage().maxRSS
  };
const path = resolve('../data/derived/fv04-generation/numerical-acceptance.json');
await mkdir(resolve(path, '..'), { recursive: true });
await writeFile(path, JSON.stringify(report, null, 2) + '\n');
console.log(
  JSON.stringify(
    {
      accepted: true,
      report: path,
      reduced: report.reduced,
      elapsedMs: report.elapsedMs,
      peakRssKiB: report.peakRssKiB
    },
    null,
    2
  )
);

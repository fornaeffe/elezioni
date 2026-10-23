import { copyFile, mkdir, mkdtemp, readFile, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { basename, relative, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import { prepareSelectedPolls } from './polls';
import { preparePollCalibration, evaluateCalibration, fitCalibration, type CalibrationCase } from './poll-calibration';
import { readCalibrationResults } from './poll-calibration-sources';
import { createPollInformedScenario } from './poll-references';
import { generationFixture } from '../../src/lib/politics/reform-2026/generation.fixture';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const archive = await prepareSelectedPolls(root, resolve(root, 'data/sources/polls/history'));
const inputs = await readCalibrationResults(root);
const cases: CalibrationCase[] = inputs.manifest.elections.map(e => ({ ...e,
  truth: inputs.results.find(r => r.sourceId === e.resultSourceId)!.panel.map(p => p.share),
  baseline: inputs.results.find(r => r.sourceId === e.baselineSourceId)!.panel.map(p => p.share),
  questions: e.questionIds.map(id => archive.bundle.questions.find(q => q.id === id)!) }));

describe('historical poll calibration', () => {
  it('pairs 16 reviewed pre-election questions with six independent official targets', async () => {
    const r = await preparePollCalibration(root);
    expect(r.elections).toHaveLength(6); expect(r.elections.flatMap(e => e.sources)).toHaveLength(16);
    expect(r.folds.map(f => f.electionId)).toEqual(['european-2014', 'camera-2018', 'european-2019', 'camera-2022', 'european-2024']);
    expect(r.archiveBytes).toBeLessThan(1048576);
    expect(r.elections.every(e => e.sources.every(q => q.publicationDate <= e.cutoff && q.fieldworkEnd <= e.cutoff))).toBe(true);
    expect(r.gate.accepted).toBe(false); expect(r.gate.sufficientCameraHoldouts).toBe(false);
    expect(r.gate.runtimeCoverageValidated).toBe(false);
    expect(r.gate.candidateChecks.find(c => c.mode === 'uncertainty')!.metricsPass).toBe(true);
    expect(r.gate.candidateChecks.find(c => c.mode === 'combined')!.metricsPass).toBe(false);
    const score = (mode: string) => r.metrics.find(m => m.mode === mode)!;
    expect(score('poll-only').mae).toBeLessThan(score('history').mae);
    expect(score('uncertainty').intervalScore!).toBeLessThan(score('v1').intervalScore!);
    expect(score('combined').mae).toBeGreaterThan(score('v1').mae);
  });
  it('preserves result denominators, candidate-only attribution and legitimate pre-M5S absence', () => {
    const camera = inputs.results.find(r => r.sourceId === 'camera-20220925')!;
    expect(camera.total).toBe(28096790); expect(camera.candidateOnly).toBe(1027135);
    // Independently checked by summing VOTICANDIDATO for this single-list
    // candidature in the frozen domestic source, not by copying a live result.
    expect(camera.lists.find(l => l.name === 'MOVIMENTO 5 STELLE')!.votes).toBe(4336660);
    const first = inputs.results.find(r => r.sourceId === 'camera-20080413')!;
    expect(first.panel.find(p => p.id === 'm5s')!.votes).toBe(0);
    for (const r of inputs.results) expect(r.panel.reduce((n, p) => n + p.share, 0)).toBeCloseTo(1);
    expect(inputs.results.find(r => r.sourceId === 'europee-20240609')!.members).toHaveLength(2);
  });
  it('never lets the held-out or future election train its prediction or intervals', () => {
    const before = evaluateCalibration(cases), changed = structuredClone(cases);
    const c = changed.find(c => c.id === 'camera-2022')!;
    c.truth[0] += .03; c.truth[3] -= .03;
    const after = evaluateCalibration(changed);
    expect(after.folds.slice(0, 3)).toEqual(before.folds.slice(0, 3));
    const a = after.folds[3], b = before.folds[3];
    expect(a.model).toEqual(b.model);
    expect(a.scores.map(s => s.rows.map(r => r.share))).toEqual(b.scores.map(s => s.rows.map(r => r.share)));
    expect(a.scores[2].rows.map(r => 'lower' in r ? r.lower : null)).toEqual(b.scores[2].rows.map(r => 'lower' in r ? r.lower : null));
    expect(a.scores[2].mae).not.toBe(b.scores[2].mae);
    expect(after.folds[4].model).not.toEqual(before.folds[4].model);
    for (const f of before.folds) expect(f.trainingElectionIds).not.toContain(f.electionId);
  });
  it('pools by election and retains only one latest compatible wave per house', () => {
    const c = structuredClone(cases[0]), duplicated = structuredClone(c);
    const q = structuredClone(c.questions[0]); q.id += '-duplicate'; duplicated.questions.push(q);
    expect(fitCalibration([duplicated])).toEqual(fitCalibration([c]));
    const fit = fitCalibration(cases);
    expect(fit.houses.find(h => h.pollster === 'SWG')!.elections).toBe(6);
    expect(fit.houses.find(h => h.pollster === 'Tecnè')!.elections).toBe(5);
    expect(fit.extraSigma.every(s => Number.isFinite(s) && s >= 0)).toBe(true);
  });
  it('rejects post-cutoff evidence and invalid targets instead of leaking future information', () => {
    const bad = structuredClone(cases); bad[0].questions[0].source.publicationDate = '2013-02-25';
    expect(() => evaluateCalibration(bad)).toThrow('post-cutoff');
    const malformed = structuredClone(cases); malformed[0].truth[0] = NaN;
    expect(() => evaluateCalibration(malformed)).toThrow('Invalid calibration');
    expect(() => evaluateCalibration([cases[0], cases[0]])).toThrow('distinct');
  });
  it('runs offline from historical JSON alone, ignoring PDF downloads and rejecting invalid inputs', async () => {
    const isolated = await mkdtemp(resolve(tmpdir(), 'elezioni-poll-history-'));
    try {
      const directory = resolve(isolated, 'data/sources/polls/history');
      await mkdir(directory, { recursive: true });
      for (const name of ['archive.json', 'manifest.json', 'entities.json', 'results.json', 'results-pin.json', 'elections-manifest.json']) {
        await copyFile(resolve(root, 'data/sources/polls/history', name), resolve(directory, name));
      }
      expect(await preparePollCalibration(isolated)).toEqual(await preparePollCalibration(root));
      await writeFile(resolve(directory, 'Sondaggio551.pdf'), 'ignored optional download');
      expect(await preparePollCalibration(isolated)).toEqual(await preparePollCalibration(root));
      const archivePath = resolve(directory, 'archive.json'), original = await readFile(archivePath);
      await writeFile(archivePath, '{}');
      await expect(preparePollCalibration(isolated)).rejects.toThrow('Invalid poll object');
      await writeFile(archivePath, original);
      await writeFile(resolve(directory, 'results.json'), '{}');
      await expect(preparePollCalibration(isolated)).rejects.toThrow('pinned version');
    } finally {
      const inside = relative(resolve(tmpdir()), resolve(isolated));
      if (!basename(isolated).startsWith('elezioni-poll-history-') || inside.startsWith('..') || !inside) throw new Error('Unsafe poll history test cleanup');
      await rm(isolated, { recursive: true, force: true });
    }
  });
  it('embeds a rejected calibration receipt without changing scenario estimates or uncertainty', async () => {
    const r = await preparePollCalibration(root), { scenario, snapshot } = generationFixture();
    const current = (await prepareSelectedPolls(root)).bundle;
    const ordinary = createPollInformedScenario(scenario, snapshot, current);
    const receipt = { version: r.version, accepted: r.gate.accepted, reasons: r.gate.reasons };
    const audited = createPollInformedScenario(scenario, snapshot, current, scenario.projectionDate, null, receipt);
    expect(audited.scenario.pollReferences!.rows).toEqual(ordinary.scenario.pollReferences!.rows);
    expect(audited.report.calibration).toEqual(receipt);
    expect(audited.scenario.pollReferences!.assumptions.some(s => s.includes(r.version))).toBe(true);
    expect(() => createPollInformedScenario(scenario, snapshot, current, scenario.projectionDate, null, { ...receipt, accepted: true })).toThrow('full-generator');
  });
});

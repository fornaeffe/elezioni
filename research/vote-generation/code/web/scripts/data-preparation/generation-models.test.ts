import { describe, expect, it } from 'vitest';
import { ilr } from './poll-uncertainty';
import {
  cutoffFor, fitGenerationModel, makeModelGenerator, positiveShares, reconcileMargins, selectForecastInput,
  type GenerationModel, type ResearchEvent, type ResearchPoll
} from './generation-models';

function event(id: string, date: string, kind: ResearchEvent['kind'], drift = 0): ResearchEvent {
  const municipalities = Array.from({ length: 8 }, (_, i) => {
    const electors = 10000 + 1000 * i, validVotes = 5000 + 450 * i;
    const shares = [.15 + .01 * i + drift, .3 - .01 * i - drift / 2, .2 - drift / 2, .35];
    return { id: `m${i}`, regionId: `r${Math.floor(i / 2)}`, electors, validVotes,
      votes: shares.map(p => p * validVotes) };
  });
  return { id, date, kind, municipalities, national: {
    electors: municipalities.reduce((n, m) => n + m.electors, 0),
    validVotes: municipalities.reduce((n, m) => n + m.validVotes, 0),
    votes: [0, 1, 2, 3].map(j => municipalities.reduce((n, m) => n + m.votes[j], 0))
  } };
}

const events = [event('camera-2008', '2008-04-13', 'camera'), event('euro-2009', '2009-06-07', 'european', .01),
  event('camera-2013', '2013-02-24', 'camera', .02), event('euro-2014', '2014-05-25', 'european', -.01),
  event('camera-2018', '2018-03-04', 'camera', .03), event('euro-2019', '2019-05-26', 'european', .015),
  event('camera-2022', '2022-09-25', 'camera', -.02), event('euro-2024', '2024-06-08', 'european', .025)];
const poll = (id: string, publicationDate: string, fieldworkEnd = publicationDate): ResearchPoll => ({
  id, publicationDate, fieldworkEnd, kind: 'generic', shares: [.25, .25, .2, .3], sampleSize: 1000, sourceUrl: 'https://example.test/poll'
});

describe('full municipal generation research', () => {
  it('reconciles each realized geographic draw using sampled valid-vote weights', () => {
    const shares = Float64Array.from([.7, .1, .1, .1, .1, .5, .3, .1, .1, .1, .2, .6]);
    const weights = Float64Array.from([100, 900, 400]), target = [.15, .2, .25, .4];
    const margin = reconcileMargins(shares, weights, target);
    expect(margin.error).toBeLessThan(1e-8);
    for (let i = 0; i < weights.length; i++) expect([...shares.slice(4 * i, 4 * i + 4)].reduce((a, b) => a + b, 0)).toBeCloseTo(1, 12);
    for (let j = 0; j < 4; j++) {
      expect(weights.reduce((n, w, i) => n + w * shares[4 * i + j], 0) / 1400).toBeCloseTo(target[j], 7);
    }
  });

  it.each<GenerationModel>(['joint-gaussian', 'joint-student-t', 'transition-bootstrap'])('%s preserves national margins and valid rates after local shocks and replays', model => {
    const input = selectForecastInput(events[6], events, [], 365, 'mixed')!;
    const fit = fitGenerationModel(input, events, [], 'mixed');
    const generate = makeModelGenerator(input, fit, model, 'municipal-test');
    const first = generate(17), replay = generate(17), second = generate(18);
    expect(first).toEqual(replay); expect(second.output).not.toEqual(first.output);
    const electors = input.baseline.municipalities.reduce((n, m) => n + m.electors, 0);
    let valid = 0;
    const totals = [0, 0, 0, 0];
    input.baseline.municipalities.forEach((m, i) => {
      const rate = first.output[5 * i + 4], shares = [...first.output.slice(5 * i, 5 * i + 4)];
      expect(rate).toBeGreaterThan(0); expect(rate).toBeLessThan(1);
      expect(shares.every(p => Number.isFinite(p) && p > 0 && p < 1)).toBe(true);
      expect(shares.reduce((a, b) => a + b, 0)).toBeCloseTo(1, 12);
      valid += m.electors * rate;
      shares.forEach((p, j) => totals[j] += p * m.electors * rate);
    });
    expect(valid / electors).toBeCloseTo(first.targetValid, 9);
    totals.forEach((total, j) => expect(total / valid).toBeCloseTo(first.national[j], 7));
    expect(() => generate(-1)).toThrow('simulation');
  });

  it('filters both publication and fieldwork, and never trains on an outcome unavailable at cutoff', () => {
    const target = events[6], cutoff = cutoffFor(target.date, 365);
    const polls = [poll('eligible', cutoff), poll('published-late', '2022-01-01'), poll('fieldwork-late', cutoff, '2021-10-01'),
      { ...poll('wrong-target', cutoff), targetElectionId: 'euro-2024' }];
    const input = selectForecastInput(target, events, polls, 365, 'mixed')!;
    expect(input.cutoff).toBe('2021-09-25'); expect(input.pollIds).toEqual(['eligible']);
    expect(input.history.every(e => e.date < input.cutoff)).toBe(true);
    const fit = fitGenerationModel(input, events, polls, 'mixed');
    expect(fit.trainingIds).not.toContain(target.id); expect(fit.trainingIds).not.toContain('euro-2024');
    const sameInput = selectForecastInput(target, events, polls, 365, 'camera-only')!;
    const sameFit = fitGenerationModel(sameInput, events, polls, 'camera-only');
    expect(sameInput.history.every(e => e.kind === 'camera')).toBe(true);
    expect(sameFit.trainingIds.every(id => id.startsWith('camera-'))).toBe(true);
    const changed = structuredClone(events);
    for (const e of changed.filter(e => e.date >= input.cutoff)) {
      e.national.votes = [1, 2, 3, 1000]; e.municipalities[0].votes = [9, 8, 7, 6000];
    }
    const changedInput = selectForecastInput(target, changed, polls, 365, 'mixed')!;
    const changedFit = fitGenerationModel(changedInput, changed, polls, 'mixed');
    expect(changedInput).toEqual(input); expect(changedFit).toEqual(fit);
    expect(makeModelGenerator(changedInput, changedFit, 'joint-student-t', 'leakage')(2))
      .toEqual(makeModelGenerator(input, fit, 'joint-student-t', 'leakage')(2));
  });

  it('does not use a previous election that occurred after a long-horizon cutoff', () => {
    const target = events[3]; // European May 2014, 730-day origin is before Camera 2013.
    const input = selectForecastInput(target, events, [], 730, 'mixed')!;
    expect(input.baseline.id).toBe('euro-2009');
    expect(input.history.map(e => e.id)).toEqual(['camera-2008', 'euro-2009']);
    const fit = fitGenerationModel(input, events, [], 'mixed');
    expect(fit.trainingIds).not.toContain('camera-2013');
    expect(selectForecastInput(events[0], events, [], 730, 'mixed')).toBeNull();
  });

  it('uses a common Student-t scale for all national coordinates', () => {
    const input = selectForecastInput(events[6], events, [], 15, 'mixed')!;
    const fit = fitGenerationModel(input, events, [], 'mixed');
    const gaussian = makeModelGenerator(input, fit, 'joint-gaussian', 'tail-test')(13);
    const student = makeModelGenerator(input, fit, 'joint-student-t', 'tail-test')(13);
    const centre = ilr(input.point), g = ilr(gaussian.national).map((v, j) => v - centre[j]);
    const t = ilr(student.national).map((v, j) => v - centre[j]);
    const ratios = t.map((v, j) => v / g[j]);
    ratios.forEach(r => expect(r).toBeCloseTo(ratios[0], 10));
    const logit = (p: number) => Math.log(p / (1 - p));
    const validRatio = (logit(student.targetValid) - logit(input.validPoint)) / (logit(gaussian.targetValid) - logit(input.validPoint));
    expect(validRatio).toBeCloseTo(ratios[0], 10);
    expect(fit.df).toBe(fit.trainingIds.length + 3);
  });

  it('labels absent-party support through the explicit positive proxy and rejects invalid margins', () => {
    const shares = positiveShares([30, 0, 20, 50]);
    expect(shares[1]).toBeGreaterThan(0); expect(shares[1]).toBeLessThan(0.005);
    expect(shares.reduce((a, b) => a + b, 0)).toBeCloseTo(1, 14);
    expect(shares[0] / shares[2]).toBeCloseTo(1.5, 14);
    expect(() => positiveShares([0, 0, 0, 0])).toThrow('Invalid');
    expect(() => reconcileMargins(Float64Array.from([NaN, 0, 0, 1]), Float64Array.from([1]), [.25, .25, .25, .25])).toThrow('Invalid');
    expect(() => reconcileMargins(Float64Array.from([-.1, .1, .5, .5]), Float64Array.from([1]), [.25, .25, .25, .25])).toThrow('Invalid');
    expect(() => reconcileMargins(Float64Array.from([1, 0, 0, 0]), Float64Array.from([1]), [.25, .25, .25, .25])).toThrow('Infeasible');
    expect(() => reconcileMargins(Float64Array.from([.25, .25, .25, .25]), Float64Array.from([0]), [.25, .25, .25, .25])).toThrow('No valid');
    expect(() => selectForecastInput(events[0], events, [], -1, 'mixed')).toThrow('Invalid');
  });
});

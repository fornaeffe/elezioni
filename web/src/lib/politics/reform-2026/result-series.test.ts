import { describe, expect, it } from 'vitest';
import { coalitionScrutinyFixture, completeScrutinyFixture } from './scrutiny.fixture';
import { generationFixture } from './generation.fixture';
import { runPoliticsReformScrutiny } from './scrutiny';
import { finalAffiliations, reformRunSeries, unaffiliatedOwner } from './result-series';
import { addSample, emptySummary, summaryTable, quantileRanks, binFor } from '../../results/statistics';
import { seriesObservation } from './stored-statistics';
import type { StoredResult } from '../../core/result-store';
import { boxPlotItem, spineChart } from '../../results/chart-builders';

describe('reform result observations', () => {
  it('counts every final winner once by personal affiliation and keeps coalition bonus ownership', () => {
    const input = coalitionScrutinyFixture(), { scenario, snapshot } = generationFixture();
    if (input.renewal.kind !== 'joint') throw new Error();
    // An independent special winner still has a supporting coalition.
    input.renewal.camera.specialCandidatures.filter(p => p.affiliationListId === 'A').forEach(p => p.affiliationListId = null);
    const result = runPoliticsReformScrutiny(input);
    expect(result.status).toBe('complete');
    const affiliations = finalAffiliations(input, result), rows = reformRunSeries(input, result, scenario, snapshot, affiliations);
    for (const chamber of ['camera', 'senato']) {
      const lists = rows.filter(r => r.Chamber === chamber && r.Kind === 'list'), subjects = rows.filter(r => r.Chamber === chamber && r.Kind === 'subject');
      const count = chamber === 'camera' ? 392 : 196;
      expect(lists.reduce((n, r) => n + Number(r.Elected), 0)).toBe(count);
      expect(subjects.reduce((n, r) => n + Number(r.Elected), 0)).toBe(count);
      const bonus = lists.filter(r => r.Owner !== unaffiliatedOwner).reduce((n, r) => n + Number(r.Bonus), 0);
      expect(bonus).toBe(chamber === 'camera' ? 70 : 35);
      expect(subjects.find(r => r.Owner === 'coalition:G')!.Bonus).toBe(bonus);
      expect(lists.every(r => Number(r.Confirmed) === Number(r.Ordinary) + Number(r.Bonus) + Number(r.Special) + Number(r.TAA))).toBe(true);
    }
    expect(rows.find(r => r.Chamber === 'camera' && r.Kind === 'list' && r.Owner === unaffiliatedOwner)!.Special).toBeGreaterThan(0);
    const local = rows.find(r => r.Kind === 'college' && r.Owner === 'list:A' && r.Chamber === 'camera' && r.College !== 'TP')!;
    const c = result.chambers.find(c => c.chamber === 'camera')!;
    expect(local['Vote share %']).toBe(c.figures!.colleges.find(f => f.collegeId === local.College)!.figures.lists.find(l => l.listId === 'A')!.percentage.status === 'defined' ? 45 : NaN);
    expect(local.Elected).toBe(result.seats.filter(s => s.status === 'filled' && s.entitlement.chamber === 'camera' && s.electedAt.collegeId === local.College && affiliations.get(s.seatId) === 'A').length);
    expect(local.Entitled).toBe(c.cameraTerritorial!.colleges.status === 'complete' ? c.cameraTerritorial!.colleges.targets.find(t => t.collegeId === local.College && t.listId === 'A')!.ordinarySeats : NaN);
  });


  it('retains constituency bonus winners without colleges and separates final affiliation from coalition ownership', () => {
    const input = coalitionScrutinyFixture(), { scenario, snapshot } = generationFixture();
    const result = runPoliticsReformScrutiny(input), affiliations = finalAffiliations(input, result);
    const rows = reformRunSeries(input, result, scenario, snapshot, affiliations);
    for (const chamber of ['camera', 'senato'] as const) {
      const local = rows.filter(r => r.Chamber === chamber && r.Kind === 'constituency-list');
      const subjects = rows.filter(r => r.Chamber === chamber && r.Kind === 'constituency-subject');
      expect(new Set(local.map(r => r.Territory))).toEqual(new Set(['O']));
      const seats = result.seats.filter(s => s.entitlement.chamber === chamber && s.status === 'filled' &&
        s.electedAt.territoryId === 'O' && ['ordinary', 'bonus'].includes(s.category));
      expect(local.reduce((n, r) => n + Number(r.Elected), 0)).toBe(seats.length);
      expect(local.reduce((n, r) => n + Number(r.Bonus), 0)).toBe(chamber === 'camera' ? 70 : 35);
      expect(subjects.find(r => r.Owner === 'coalition:G')!.Bonus).toBe(chamber === 'camera' ? 70 : 35);
      expect(local.find(r => r.Owner === 'list:C')!.Bonus).toBeGreaterThan(0);
      const figures = result.chambers.find(c => c.chamber === chamber)!.figures!.territories.find(t => t.territoryId === 'O')!.figures;
      for (const row of local) {
        expect(row['Valid votes']).toBe(figures.validVotes);
        expect(row['Vote share %']).toBe(100 * Number(row.Votes) / figures.validVotes);
        expect(row.Confirmed).toBe(Number(row.Ordinary) + Number(row.Bonus));
        expect(row.Special).toBe(0); expect(row.TAA).toBe(0);
      }
      expect(new Set(local.map(row => seriesObservation({ id: 'scope-test', result: { benchmark: { simulations: 1 } } } as StoredResult, row).descriptor.id)).size).toBe(local.length);
    }
  });

  it('keeps chamber-specific unknown totals null while retaining known votes and confirmed lower bounds', () => {
    const input = completeScrutinyFixture(60), { scenario, snapshot } = generationFixture();
    if (input.renewal.kind !== 'joint') throw new Error();
    input.renewal.camera.ordinarySlates.forEach(s => s.preferences = { kind: 'missing', reason: 'No preferences' });
    const result = runPoliticsReformScrutiny(input), rows = reformRunSeries(input, result, scenario, snapshot);
    expect(rows.filter(r => r.Chamber === 'camera').every(r => r.Elected === null)).toBe(true);
    expect(rows.filter(r => r.Chamber === 'senato').every(r => r.Elected !== null)).toBe(true);
    expect(rows.find(r => r.Chamber === 'camera' && r.Kind === 'list' && r.Owner === 'list:A')).toMatchObject({ 'Vote share %': 60, Elected: null });
    expect(rows.filter(r => r.Kind === 'list' && r.Chamber === 'camera').reduce((n, r) => n + Number(r.Confirmed), 0)).toBeGreaterThan(0);
  });

  it('does not manufacture the non-renewed chamber or turn undefined percentages into zero', () => {
    const input = completeScrutinyFixture(30), { scenario, snapshot } = generationFixture();
    if (input.renewal.kind !== 'joint') throw new Error();
    input.renewal = { kind: 'single', chamber: input.renewal.senato };
    input.renewal.chamber.bonusSlates = []; input.bonusLinks = [];
    const result = runPoliticsReformScrutiny(input), rows = reformRunSeries(input, result, scenario, snapshot);
    expect(rows.every(r => r.Chamber === 'senato')).toBe(true);
    const independent = rows.find(r => r.Owner === unaffiliatedOwner)!;
    expect(independent['Vote share %']).toBeNull(); expect(independent.Confirmed).toBe(0);
  });
});

it('keeps requested denominators and precise chart/bin values', () => {
  const record = { id: 'test', result: { benchmark: { simulations: 3 } } } as StoredResult;
  const row = { Simulation: 1, Chamber: 'camera', Kind: 'list', Owner: 'list:A', College: null, Territory: null, Name: 'A', Color: '#123456', Order: 0,
    'Vote share %': 12.345678, Elected: 2, Confirmed: 2, Ordinary: 1, Bonus: 1, Special: 0, TAA: 0, Entitled: null };
  const { descriptor, sample } = seriesObservation(record, row), summary = emptySummary(descriptor);
  addSample(summary, sample); addSample(summary, { ...sample, simulation: 2, y: null, confirmed: 1 });
  const table = summaryTable({ requested: 3, saved: 2, legacy: false, series: [summary], colleges: [] });
  expect(table.rows[0]).toMatchObject({ Requested: 3, 'Mean elected': null, 'Mean vote share %': null, 'Confirmed elected minimum / requested': 1 });
  expect(sample.x).toBe(12.345678);
  expect(quantileRanks(4, .25)).toEqual({ lower: 0, upper: 1, fraction: .75 });
  expect(binFor(12.345678, [12.345678, 12.345679, 12.345680])).toBe(0);
  const distribution = { count: 2, min: 12.345678, max: 12.345678, mean: 12.345678, q1: 12.345678, median: 12.345678, q3: 12.345678, p05: 12.345678, p95: 12.345678, whiskerLow: 12.345678, whiskerHigh: 12.345678 };
  expect(boxPlotItem(descriptor, distribution, 3, 20)!.median).toBe(12.345678);
  const chart = spineChart({ descriptor, requested: 3, known: 1, missingX: 1, missingY: 1, distribution, yMax: 2,
    bins: [{ low: 12.345678, high: 12.345678, count: 2, outcomes: [{ seats: 2, count: 1 }, { seats: null, count: 1 }] }] }, 'Test');
  expect(chart.panels[0].bins[0].cells.map(c => [c.label, c.heightPercent])).toEqual([['2', 50], ['?', 50]]);
});

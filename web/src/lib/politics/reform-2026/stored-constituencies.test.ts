import { describe, expect, it } from 'vitest';
import type { StoredResult } from '../../core/result-store';
import type { ResultTable } from '../../core/types';
import { emptySummary, type SeriesDescriptor } from '../../results/statistics';
import { archivedConstituencyCatalogue, archivedConstituencyRows } from './stored-constituencies';
import type { CollegeSeatEndowment } from './stored-statistics';
import { seriesKey } from './result-series';

type Row = ResultTable['rows'][number];
function fixture() {
  const descriptor: SeriesDescriptor = { id: seriesKey('camera', 'constituency-list', 'list:A', null, 'north'), chamber: 'camera',
    kind: 'constituency-list', owner: 'list:A', territory: 'north', college: null, name: 'A · North', color: '#123456', order: 0 };
  const endowments: CollegeSeatEndowment[] = ['N1', 'N2'].map(college => ({ chamber: 'camera', territory: 'north', territoryName: 'North',
    college, collegeName: college, kind: 'Ordinario', seatsWithoutBonus: 6, seatsWithBonus: 5 }));
  const record = { scenario: { lists: { camera: [{ id: 'A', coalitionId: 'G' }, { id: 'B', coalitionId: 'G' }] },
    coverage: ['N1', 'N2'].map(collegeId => ({ chamber: 'camera', collegeId, listIds: ['A', 'B'] })) },
    result: { storage: { tables: [
      { name: 'Reform college series v1', columns: ['Votes', 'Valid votes', 'Entitled'] },
      { name: 'Reform statistical series v1', columns: ['Elected', 'Confirmed'] },
      { name: 'Seat entitlement and election locations', columns: ['Seat', 'Election territory', 'Election college', 'Election list', 'Subject', 'Category', 'Status'] },
      { name: 'Candidate outcomes (conditional)', columns: ['Seat', 'Affiliation list'] }
    ] } } } as unknown as StoredResult;
  const wrap = (rows: Row[]) => rows.map(r => ({ Simulation: 1, Chamber: 'camera', ...r }));
  const tables: Record<string, Row[]> = {
    'Reform college series v1': wrap(['N1', 'N2'].flatMap((College, i) => ['A', 'B'].map((list, j) => ({ College, Owner: `list:${list}`,
      Votes: i ? j ? 450 : 450 : j ? 90 : 10, 'Valid votes': i ? 900 : 100, Entitled: j ? 1 : i ? 3 : 2 })))),
    'Reform statistical series v1': wrap([{ Kind: 'list', Owner: 'list:A', Elected: 3, Confirmed: 3 },
      { Kind: 'list', Owner: 'list:B', Elected: 2, Confirmed: 2 }, { Kind: 'subject', Owner: 'coalition:G', Elected: 5, Confirmed: 5 }]),
    'Seat entitlement and election locations': wrap([
      { Seat: 'ordinary', Category: 'ordinary', 'Election territory': 'north', 'Election college': 'N1' },
      { Seat: 'bonus', Category: 'bonus', 'Election territory': 'north', 'Election college': null },
      { Seat: 'bonus-recovery', Category: 'bonus', 'Election territory': 'north', 'Election college': 'N2', 'Entitlement territory': 'south' },
      { Seat: 'ordinary-outward', Category: 'ordinary', 'Election territory': 'south', 'Election college': 'S1', 'Entitlement territory': 'north' },
      { Seat: 'special', Category: 'special-uninominal', 'Election territory': 'north', 'Election college': 'special' }
    ].map(r => ({ ...r, 'Entitlement territory': r['Entitlement territory'] ?? 'north', Subject: 'coalition:G', Status: 'filled' }))),
    'Candidate outcomes (conditional)': wrap(['ordinary', 'bonus', 'bonus-recovery', 'ordinary-outward', 'special'].map((Seat, i) =>
      ({ Seat, 'Affiliation list': [0, 1, 3].includes(i) ? 'A' : 'B' })))
  };
  const read = async function* (name: string) { yield* tables[name] ?? []; };
  const run = async (d = descriptor) => { const rows: Row[] = []; for await (const row of archivedConstituencyRows(record, d, endowments, read)) rows.push(row); return rows[0]; };
  return { descriptor, endowments, record, tables, run };
}

describe('archived constituency evidence', () => {
  it('weights raw votes and counts bonus seats at their final location, with personal list affiliation and legal coalition ownership', async () => {
    const f = fixture();
    expect(await f.run()).toMatchObject({ 'Vote share %': 46, Elected: 2, Ordinary: 1, Bonus: 1, Entitled: 5 });
    expect(await f.run({ ...f.descriptor, kind: 'constituency-subject', owner: 'coalition:G' })).toMatchObject({ 'Vote share %': 100, Elected: 3, Ordinary: 1, Bonus: 2, Entitled: 7 });
  });
  it('keeps missing votes, rows, coverage and incomplete counts unknown', async () => {
    const f = fixture();
    f.tables['Reform college series v1'].splice(2, 1);
    expect(await f.run()).toMatchObject({ 'Vote share %': null, Entitled: null, Elected: 2 });
    f.tables['Reform statistical series v1'][0].Elected = null;
    expect(await f.run()).toMatchObject({ Elected: null, Confirmed: 2 });
  });
  it('does not infer personal affiliation from coalition bonus ownership', async () => {
    const f = fixture(); f.tables['Candidate outcomes (conditional)'].splice(1, 1);
    expect(await f.run()).toMatchObject({ Elected: null });
    expect(await f.run({ ...f.descriptor, kind: 'constituency-subject', owner: 'coalition:G' })).toMatchObject({ Elected: 3 });
  });
  it('distinguishes non-contestation from missing coverage and rejects insufficient archive capabilities', async () => {
    const f = fixture();
    if (!('coverage' in f.record.scenario)) throw new Error();
    f.record.scenario.coverage[1].listIds = ['B'];
    f.tables['Reform college series v1'].splice(2, 1);
    expect(await f.run()).toMatchObject({ 'Vote share %': 1, Entitled: 2 });
    f.record.scenario.coverage.pop();
    expect(await f.run()).toMatchObject({ 'Vote share %': null, Entitled: null });
    const national = [emptySummary({ ...f.descriptor, kind: 'list', territory: null })];
    expect(archivedConstituencyCatalogue(f.record, national, f.endowments)).toHaveLength(1);
    f.record.result.storage!.tables[0].columns = ['Vote share %'];
    expect(archivedConstituencyCatalogue(f.record, national, f.endowments)).toEqual([]);
  });
});

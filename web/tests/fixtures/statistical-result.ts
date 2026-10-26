import { electionPositionColumns, positionColumns, positionTable } from '../../src/lib/politics/reform-2026/election-probability';
import type { ResultTable } from '../../src/lib/core/types';
import type { ReformGenerationScenario } from '../../src/lib/politics/reform-2026/generation-types';
import { reformSeriesColumns, reformSeriesTable, reformCollegeSeriesTable } from '../../src/lib/politics/reform-2026/result-series';

/** Small synthetic archive for browser presentation checks, not electoral data. */
export function statisticalResult(scenario: ReformGenerationScenario, count = 1201) {
  const list = scenario.lists.camera[0], subject = list.coalitionId ? `coalition:${list.coalitionId}` : `list:${list.id}`;
  const national: ResultTable = { name: reformSeriesTable, columns: reformSeriesColumns, rows: [] };
  const local: ResultTable = { name: reformCollegeSeriesTable, columns: reformSeriesColumns, rows: [] };
  for (let i = 1; i <= count; i++) {
    const x = (i % 101) / 10, y = i % 9, known = i !== 100;
    const base = { Simulation: i, Chamber: 'camera', Owner: `list:${list.id}`, College: null, Territory: null, Name: list.name, Color: '#336699', Order: 0,
      Votes: Math.round(x * 100), 'Valid votes': 10000, 'Vote share %': x, Elected: known ? y : null, Confirmed: y, Ordinary: y, Bonus: 0, Special: 0, TAA: 0, Entitled: null };
    national.rows.push({ ...base, Kind: 'list' }, { ...base, Kind: 'subject', Owner: subject });
    for (let j = 1; j <= 2; j++) local.rows.push({ ...base, Kind: 'college', College: `constructed-${j}`, Territory: 'constructed', Name: `${list.name} · Collegio di prova ${j}`,
      'Vote share %': 12.345678 + j * .000001, Elected: known ? i % j : null, Confirmed: i % j, Ordinary: i % j, Entitled: j });
  }
  const tables: ResultTable[] = [
    { name: 'Reform overview', columns: ['Requested'], rows: [{ Requested: count }] },
    { name: 'Reform runs', columns: ['Simulation', 'Status'], rows: Array.from({ length: count }, (_, i) => ({ Simulation: i + 1, Status: i === 99 ? 'partial' : 'complete' })) },
    { name: 'Reform vote shares', columns: ['Simulation', 'Chamber', 'List', 'Votes', 'Valid votes', 'Vote share %'], rows: national.rows.filter(r => r.Kind === 'list').map(r => ({ Simulation: r.Simulation, Chamber: r.Chamber, List: list.id, Votes: r.Votes, 'Valid votes': r['Valid votes'], 'Vote share %': r['Vote share %'] })) },
    { name: 'Seat entitlement and election locations', columns: ['Simulation', 'Chamber', 'Category', 'Subject', 'List', 'Status', 'Election list', 'Election college'], rows: [] },
    national, local
  ];
  return { schema_version: 2, exportedAt: '2026-10-22T00:00:00Z', scenario,
    result: { type: 'result', status: 'completed', tables: tables.map(t => ({ ...t, rows: t.rows.map(r => t.columns.map(c => r[c] ?? null)) })), warnings: [],
      benchmark: { startedAt: '2026-10-22T00:00:00Z', elapsedMs: 1, simulations: count, dataVersion: scenario.snapshotVersion, scrutinyAlgorithmId: scenario.algorithmId, seed: scenario.seed } } };
}

/** Distinct national series to detect stale plots and accidental multiple rendering. */
export function multipleSeriesResult(scenario: ReformGenerationScenario) {
  const archive = statisticalResult(scenario, 12);
  const national = archive.result.tables.find(t => t.name === reformSeriesTable)!;
  const source = national.rows.filter(row => row[national.columns.indexOf('Kind')] === 'list');
  national.rows = [];
  let index = 0;
  for (const chamber of ['camera', 'senato']) for (const kind of ['list', 'subject']) for (let entity = 1; entity <= 2; entity++) {
    const unknown = index++;
    for (const row of source) {
      const values = Object.fromEntries(national.columns.map((column, i) => [column, row[i]]));
      Object.assign(values, { Chamber: chamber, Kind: kind, Owner: `${kind}:constructed-${entity}`,
        Name: `${chamber} ${kind} ${entity}`, Order: entity,
        Elected: Number(values.Simulation) <= unknown ? null : entity });
      national.rows.push(national.columns.map(column => values[column]));
    }
  }
  return archive;
}

/** Synthetic evidence for position probabilities, bonus decisions and local selectors. */
export function refinementResult(scenario: ReformGenerationScenario) {
  const archive = multipleSeriesResult(scenario);
  const local = archive.result.tables.find(t => t.name === reformCollegeSeriesTable)!;
  local.rows = [];
  const positions: ResultTable = { name: positionTable, columns: electionPositionColumns, rows: [] };
  const eligibility: ResultTable = { name: 'Bonus eligibility', columns: ['Simulation', 'Chamber', 'Subject', 'Status'], rows: [] };
  for (const chamber of ['camera', 'senato']) for (let college = 1; college <= 3; college++) for (let entity = 1; entity <= 2; entity++) {
    if (college === 3 && entity === 1) continue;
    for (let simulation = 1; simulation <= 12; simulation++) {
      const row = { Simulation: simulation, Chamber: chamber, Kind: 'college', Owner: `list:constructed-${entity}`, College: `college-${college}`, Territory: 'test', Name: `${chamber} list ${entity} · Collegio ${college}`, Color: '#336699', Order: entity,
        Votes: 100, 'Valid votes': 1000, 'Vote share %': 10, Elected: entity, Confirmed: entity, Ordinary: entity, Bonus: 0, Special: 0, TAA: 0, Entitled: entity };
      local.rows.push(local.columns.map(c => row[c as keyof typeof row]));
      positions.rows.push({ Simulation: simulation, Chamber: chamber, List: `constructed-${entity}`, College: `college-${college}`, Name: `Collegio ${college}`,
        ...Object.fromEntries(positionColumns.map((column, i) => [column, i === 0 ? chamber === 'camera' ? Number(simulation <= 6) : 1 : i === 1 ? simulation <= 4 ? 1 : simulation <= 8 ? 0 : null : 'absent'])) });
    }
  }
  // Canonical series must remain in simulation order for disk-backed scans.
  local.rows.sort((a, b) => Number(a[0]) - Number(b[0]));
  const runs = archive.result.tables.find(t => t.name === 'Reform runs')!;
  runs.columns = ['Simulation', 'Status', 'Bonus'];
  runs.rows = Array.from({ length: 12 }, (_, i) => [i + 1, 'complete', i < 6 ? 'awarded' : i < 11 ? 'not-awarded' : 'non-computable']);
  for (let simulation = 1; simulation <= 6; simulation++) for (const chamber of ['camera', 'senato']) eligibility.rows.push({ Simulation: simulation, Chamber: chamber, Subject: `subject:constructed-${simulation <= 3 ? 1 : 2}`, Status: 'awarded' });
  for (const table of [positions, eligibility]) archive.result.tables.push({ ...table, rows: table.rows.map(row => table.columns.map(c => row[c] ?? null)) });
  return archive;
}

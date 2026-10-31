import { electionPositionColumns, positionColumns, positionTable } from '../../src/lib/politics/reform-2026/election-probability';
import type { ResultTable } from '../../src/lib/core/types';
import type { ReformGenerationScenario } from '../../src/lib/politics/reform-2026/generation-types';
import { reformSeriesColumns, reformSeriesTable, reformCollegeSeriesTable, reformConstituencySeriesTable } from '../../src/lib/politics/reform-2026/result-series';

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

/** Direct compact observations, including bonus winners without college rows. */
export function constituencyTable(count = 12) {
  const table: ResultTable = { name: reformConstituencySeriesTable, columns: reformSeriesColumns, rows: [] };
  for (let simulation = 1; simulation <= count; simulation++) for (const chamber of ['camera', 'senato']) for (const territory of ['north', 'south']) {
    for (const entity of [1, 2, 0]) {
      const ordinary = entity === 1 ? simulation % 3 : entity === 2 ? 2 : simulation % 3 + 2;
      const bonus = entity === 2 ? 0 : simulation <= 6 ? 2 : 0;
      table.rows.push({ Simulation: simulation, Chamber: chamber, Kind: entity ? 'constituency-list' : 'constituency-subject',
        Owner: entity ? `list:constructed-${entity}` : 'coalition:constructed', College: null, Territory: territory,
        Name: `${entity ? `${chamber} list ${entity}` : 'Coalizione test'} · ${territory === 'north' ? 'Nord' : 'Sud'}`,
        Color: '#336699', Order: entity, Votes: (territory === 'north' ? (simulation - 1) % 80 + 1 : 21 + (simulation - 1) % 80) * 10, 'Valid votes': 1000,
        'Vote share %': simulation === 7 ? null : territory === 'north' ? (simulation - 1) % 80 + 1 : 21 + (simulation - 1) % 80,
        Elected: simulation === 2 ? null : ordinary + bonus, Confirmed: ordinary + bonus, Ordinary: ordinary, Bonus: bonus,
        Special: 0, TAA: 0, Entitled: entity ? entity : 3 });
    }
  }
  return { ...table, rows: table.rows.map(row => table.columns.map(c => row[c] ?? null)) };
}

export function constituencyResult(scenario: ReformGenerationScenario) {
  const archive = refinementResult(scenario);
  archive.result.tables.push(constituencyTable());
  return archive;
}

/** Older archive with sufficient saved raw/seat evidence but no constituency table. */
export function archivedConstituencyResult(scenario: ReformGenerationScenario) {
  const archive = statisticalResult(scenario, 12), list = scenario.lists.camera[0];
  const colleges = scenario.coverage.filter(c => c.chamber === 'camera' && c.listIds.includes(list.id)).slice(0, 2);
  const national = archive.result.tables.find(t => t.name === reformSeriesTable)!;
  for (const row of national.rows) {
    row[national.columns.indexOf('Elected')] = 2;
    row[national.columns.indexOf('Confirmed')] = 2;
    row[national.columns.indexOf('Ordinary')] = 1;
    row[national.columns.indexOf('Bonus')] = 1;
  }
  const local = archive.result.tables.find(t => t.name === reformCollegeSeriesTable)!;
  local.rows = [];
  for (let simulation = 1; simulation <= 12; simulation++) for (const [i, college] of colleges.entries()) for (const id of college.listIds) {
    const row = { Simulation: simulation, Chamber: 'camera', Kind: 'college', Owner: `list:${id}`, College: college.collegeId, Territory: 'saved-territory',
      Name: `${id} · Collegio salvato ${i + 1}`, Color: '#336699', Order: 0, Votes: id === list.id ? i ? 450 : 10 : 0,
      'Valid votes': i ? 900 : 100, 'Vote share %': id === list.id ? i ? 50 : 10 : 0,
      Elected: i ? 0 : 1, Confirmed: i ? 0 : 1, Ordinary: i ? 0 : 1, Bonus: 0, Special: 0, TAA: 0, Entitled: 1 };
    local.rows.push(local.columns.map(c => row[c as keyof typeof row]));
  }
  const ledger = archive.result.tables.find(t => t.name === 'Seat entitlement and election locations')!;
  ledger.columns = ['Simulation', 'Chamber', 'Seat', 'Category', 'Subject', 'Status', 'Election territory', 'Election college', 'Election list'];
  ledger.rows = [];
  const subject = list.coalitionId ? `coalition:${list.coalitionId}` : `list:${list.id}`;
  const candidates: ResultTable = { name: 'Candidate outcomes (conditional)', columns: ['Simulation', 'Chamber', 'Seat', 'Affiliation list'], rows: [] };
  for (let simulation = 1; simulation <= 12; simulation++) for (const category of ['ordinary', 'bonus']) {
    ledger.rows.push([simulation, 'camera', category, category, subject, 'filled', 'saved-territory', category === 'bonus' ? null : colleges[0].collegeId, category === 'bonus' ? null : list.id]);
    candidates.rows.push({ Simulation: simulation, Chamber: 'camera', Seat: category, 'Affiliation list': list.id });
  }
  const endowments: ResultTable = { name: 'Seat endowments by college', columns: ['Chamber', 'Territory', 'Territory name', 'College', 'College name', 'Kind', 'Seats without bonus', 'Seats with bonus'],
    rows: colleges.map((c, i) => ({ Chamber: 'camera', Territory: 'saved-territory', 'Territory name': 'Circoscrizione salvata', College: c.collegeId, 'College name': `Collegio salvato ${i + 1}`, Kind: 'Ordinario', 'Seats without bonus': 6, 'Seats with bonus': 5 })) };
  for (const table of [candidates, endowments]) archive.result.tables.push({ ...table, rows: table.rows.map(row => table.columns.map(c => row[c] ?? null)) });
  return archive;
}

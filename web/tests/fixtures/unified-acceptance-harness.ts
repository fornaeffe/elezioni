/** Read-only verification of the actual browser-persisted ledger, bounded by a run. */
import { getStoredResult, storedRows, storedTablePage } from '../../src/lib/core/result-store';
export async function inspect(id: string, owner: string) {
  const record = await getStoredResult(id, owner),
    storage = record.result.storage!;
  const table = (name: string) => {
    const i = storage.tables.findIndex((t) => t.name === name);
    if (i < 0) throw new Error('Missing acceptance table ' + name);
    return i;
  };
  const columns = (name: string) => storage.tables[table(name)].columns;
  const runs: {
    simulation: number;
    status: string;
    bonus: boolean;
    seats: number;
    distinctCandidates: number;
    camera: number;
    senato: number;
    unfilled: number;
  }[] = [];
  for await (const row of storedRows(id, table('Reform runs'))) {
    const c = columns('Reform runs');
    runs.push({
      simulation: Number(row[c.indexOf('Simulation')]),
      status: String(row[c.indexOf('Status')]),
      bonus: row[c.indexOf('Bonus')] === 'awarded',
      seats: 0,
      distinctCandidates: 0,
      camera: 0,
      senato: 0,
      unfilled: 0,
    });
  }
  const byId = new Map(runs.map((r) => [r.simulation, r])),
    runIndex = new Map(runs.map((r, i) => [r.simulation, i])),
    seatSamples = new Map<string, number[]>(),
    seatsByList: Record<string, number> = {},
    c = columns('Seat entitlement and election locations');
  let currentSimulation = 0,
    people = new Set<string>();
  if ('lists' in record.scenario && 'camera' in record.scenario.lists)
    for (const chamber of ['camera', 'senato'] as const)
      for (const list of record.scenario.lists[chamber])
        seatSamples.set(
          JSON.stringify([chamber, list.id]),
          Array(runs.length).fill(0),
        );
  for await (const row of storedRows(
    id,
    table('Seat entitlement and election locations'),
  )) {
    const sim = Number(row[c.indexOf('Simulation')]),
      r = byId.get(sim);
    if (!r) throw new Error('Seat has no requested simulation');
    if (sim !== currentSimulation) {
      if (sim < currentSimulation)
        throw new Error('Ledger is not committed in global simulation order');
      if (currentSimulation)
        byId.get(currentSimulation)!.distinctCandidates = people.size;
      people = new Set();
      currentSimulation = sim;
    }
    if (row[c.indexOf('Status')] !== 'filled') r.unfilled++;
    r.seats++;
    const ch = row[c.indexOf('Chamber')];
    if (ch === 'camera') r.camera++;
    else if (ch === 'senato') r.senato++;
    people.add(String(row[c.indexOf('Candidate')]));
    const key = JSON.stringify([ch, row[c.indexOf('Election list')]]);
    seatsByList[key] = (seatsByList[key] ?? 0) + 1;
    const samples = seatSamples.get(key) ?? Array(runs.length).fill(0);
    samples[runIndex.get(sim)!]++;
    seatSamples.set(key, samples);
  }
  if (currentSimulation)
    byId.get(currentSimulation)!.distinctCandidates = people.size;
  const admission: Record<string, number> = {},
    thresholds: Record<string, number> = {};
  for await (const row of storedRows(id, table('Admission decisions'))) {
    const c = columns('Admission decisions'),
      k = String(row[c.indexOf('Decision')]);
    admission[k] = (admission[k] ?? 0) + 1;
  }
  for await (const row of storedRows(id, table('Bonus eligibility'))) {
    const c = columns('Bonus eligibility'),
      k = String(row[c.indexOf('At least 42%')]);
    thresholds[k] = (thresholds[k] ?? 0) + 1;
  }
  const caps: Record<string, number> = {},
    voteShares: Record<string, { sum: number; observedDraws: number }> = {};
  for await (const row of storedRows(id, table('Vote-based entitlements'))) {
    const c = columns('Vote-based entitlements'),
      check = JSON.parse(String(row[c.indexOf('Cap check')])),
      key = JSON.stringify([
        row[c.indexOf('Chamber')],
        check === null
          ? 'not-applicable'
          : check.exceedsCap
            ? 'applied'
            : 'not-needed',
      ]);
    caps[key] = (caps[key] ?? 0) + 1;
  }
  for await (const row of storedRows(id, table('Reform vote shares'))) {
    const c = columns('Reform vote shares'),
      key = JSON.stringify([row[c.indexOf('Chamber')], row[c.indexOf('List')]]),
      value = row[c.indexOf('Vote share %')];
    if (typeof value === 'number') {
      const p = voteShares[key] ?? { sum: 0, observedDraws: 0 };
      p.sum += value;
      p.observedDraws++;
      voteShares[key] = p;
    }
  }
  const seatDistributions = Object.fromEntries(
    [...seatSamples].map(([key, values]) => {
      const counts: Record<string, number> = {};
      for (const n of values) counts[n] = (counts[n] ?? 0) + 1;
      return [
        key,
        { counts, mean: values.reduce((a, b) => a + b, 0) / runs.length },
      ];
    }),
  );
  return {
    // Exercise bounded, indexed detail queries for the last committed run, on
    // both native and imported archives, without materializing the whole ledger.
    lastRunDetail: await storedTablePage(record, table('Seat entitlement and election locations'), 0, 10, runs.at(-1)!.simulation),
    model:
      'generation' in record.scenario
        ? record.scenario.generation?.model
        : undefined,
    modelVersion: 'generation' in record.scenario ? record.scenario.generation?.modelVersion : undefined,
    benchmark: record.result.benchmark,
    storage,
    runs,
    seatsByList,
    admission,
    thresholds,
    fitFilePresent: !!record.fitFile,
    generationImplementationId:
      'models' in record.scenario
        ? record.scenario.models.generationImplementationId
        : undefined,
    data: 'data' in record.scenario ? record.scenario.data : undefined,
    seatDistributions,
    caps,
    voteShares,
  };
}

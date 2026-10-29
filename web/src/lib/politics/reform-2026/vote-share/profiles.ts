import { clip, day, median, requireWorkspace, sum, years, type FitContext, type NationalElection } from './types';
import { completeSteps, type FitProgress } from './execution';

export interface MunicipalObservation {
  id: string; region: string; electors: number; nonValid: number;
  groups: string[][]; counts: number[];
}
export interface MunicipalElection { id: string; date: string; context: FitContext; rows: MunicipalObservation[] }
export interface ProfileSource {
  components: string[]; referenceDate: string; targetContext: FitContext;
  nationalElections: NationalElection[]; elections: MunicipalElection[];
  /** Conditional target geography only: no future vote counts or observed truth. */
  targets: { id: string; region: string; electors: number }[];
}
interface Feature { date: string; context: FitContext; value: number; exposure: number; noise: number }
interface CityEvent { event: MunicipalElection; index: number; row: MunicipalObservation }
interface Moment { n: number; mean: number; m2: number; noise: number }
export function fitProfiles(source: ProfileSource, maximumWorkspaceBytes?: number) {
  return completeSteps(fitProfilesSteps(source, maximumWorkspaceBytes));
}
export function* fitProfilesSteps(source: ProfileSource, maximumWorkspaceBytes?: number) {
  const ids = [...source.components].sort(), jCount = ids.length + 1, index = new Map(ids.map((id, i) => [id, i + 1]));
  if (!ids.length || ids.length > 128 || new Set(ids).size !== ids.length || !['camera', 'european'].includes(source.targetContext))
    throw new RangeError('Invalid profile basis/context');
  day(source.referenceDate);
  if (source.elections.length > 64) throw new RangeError('Unsupported profile historical envelope');
  requireWorkspace(source.elections.reduce((s, e) => s + e.rows.length * 64, 0) + source.targets.length * jCount * 18, maximumWorkspaceBytes);
  const events = [...source.elections].sort((a, b) => a.date.localeCompare(b.date) || a.id.localeCompare(b.id));
  if (new Set(events.map(e => e.id)).size !== events.length) throw new RangeError('Duplicate profile event');
  const national = new Map<string, NationalElection>();
  for (const n of source.nationalElections) {
    const members = n.groups.flat();
    if (national.has(n.id) || day(n.date) > day(source.referenceDate) || !['camera', 'european'].includes(n.context) ||
      !Number.isSafeInteger(n.electors) || n.electors <= 0 || n.counts.length !== n.groups.length + 1 ||
      n.counts.some(x => !Number.isSafeInteger(x) || x < 0) || sum(n.counts) !== n.electors ||
      n.counts[0] <= 0 || n.counts[0] >= n.electors || n.groups.some(g => !g.length) ||
      new Set(members).size !== members.length || members.some(id => !index.has(id))) throw new RangeError('Invalid national profile baseline');
    national.set(n.id, n);
  }
  const byCity = new Map<string, CityEvent[]>(), region = new Map(source.targets.map(t => [t.id, t.region]));
  const targetIds = new Set<string>();
  for (const t of source.targets) {
    if (!t.id || !t.region || targetIds.has(t.id) || !Number.isSafeInteger(t.electors) || t.electors <= 0) throw new RangeError('Invalid target geography/electorate');
    targetIds.add(t.id);
  }
  const baselines = new Map<string, { p0: number; groups: Map<string, number> }>();
  const key = (g: readonly string[]) => JSON.stringify([...g].sort());
  for (const [ei, e] of events.entries()) {
    const n = national.get(e.id);
    if (!n || n.context !== e.context || n.date !== e.date || day(e.date) > day(source.referenceDate)) throw new RangeError('Profile event missing compatible cutoff-eligible national baseline');
    const valid = n.electors - n.counts[0];
    if (!(valid > 0 && n.counts[0] > 0) || sum(n.counts) !== n.electors || n.groups.length + 1 !== n.counts.length) throw new RangeError('Invalid national profile baseline');
    baselines.set(e.id, { p0: n.counts[0] / n.electors, groups: new Map(n.groups.map((g, i) => [key(g), n.counts[i + 1] / valid])) });
    const seen = new Set<string>();
    for (const [ri, row] of e.rows.entries()) {
      if (ri % 256 === 0) yield { phase: 'profile-history', completed: ei * 1000 + ri / e.rows.length * 1000, total: events.length * 1000 } satisfies FitProgress;
      const members = row.groups.flat();
      if (!row.id || !row.region || seen.has(row.id) || !Number.isSafeInteger(row.electors) || row.electors <= 0 ||
        !Number.isSafeInteger(row.nonValid) || row.nonValid < 0 || row.nonValid > row.electors ||
        row.counts.length !== row.groups.length || row.counts.some(x => !Number.isSafeInteger(x) || x < 0) ||
        sum(row.counts) + row.nonValid !== row.electors || row.groups.some(g => !g.length) ||
        new Set(members).size !== members.length || members.some(id => !index.has(id))) throw new RangeError('Invalid original municipal grouped counts');
      seen.add(row.id);
      const entries = byCity.get(row.id) ?? []; entries.push({ event: e, index: ei, row }); byCity.set(row.id, entries);
      region.set(row.id, row.region);
    }
  }
  const cities = [...new Set([...byCity.keys(), ...targetIds])].sort(), cCount = cities.length;
  // Two country-sized feature summaries plus target outputs; histories stay sparse.
  const workspaceBytes = 8 * (2 * cCount * jCount + 2 * source.targets.length * jCount + events.length * jCount * 8) +
    source.targets.length * jCount + [...byCity.values()].reduce((s, xs) => s + xs.length * 64, 0);
  requireWorkspace(workspaceBytes, maximumWorkspaceBytes);
  const raw = new Float64Array(cCount * jCount).fill(NaN), information = new Float64Array(cCount * jCount);
  const moments = Array.from({ length: Math.max(0, events.length - 1) * jCount }, (): Moment => ({ n: 0, mean: 0, m2: 0, noise: 0 }));
  const regional = new Map<string, { numerator: Float64Array; denominator: Float64Array }>();
  let unmatchedGroups = 0;
  function features(city: string, countSkipped: boolean) {
    const separate: Feature[][] = Array.from({ length: jCount }, () => []), pooled: (Feature | undefined)[] = Array(jCount);
    let previous: { index: number; features: Map<number, Feature> } | undefined;
    for (const entry of byCity.get(city) ?? []) {
      const { row, event: e } = entry, n = baselines.get(e.id)!, valid = row.electors - row.nonValid;
      const local = (row.nonValid + 0.5) / (row.electors + 1);
      const nonValid = { date: e.date, context: e.context,
        value: Math.log(local / (1 - local)) - Math.log(Math.max(1e-10, n.p0) / Math.max(1e-10, 1 - n.p0)),
        exposure: row.electors, noise: 1 / (row.nonValid + 0.5) + 1 / (valid + 0.5) };
      separate[0].push(nonValid);
      const values = new Map<number, Feature>([[0, nonValid]]);
      row.groups.forEach((g, h) => {
        const baseline = n.groups.get(key(g));
        if (baseline === undefined) { if (countSkipped) unmatchedGroups++; return; }
        const feature = { date: e.date, context: e.context, exposure: valid, noise: 1 / (row.counts[h] + 0.5),
          value: Math.log((row.counts[h] + 0.5) / (valid + 0.5 * row.groups.length) / Math.max(1e-10, baseline)) };
        if (g.length === 1) { const j = index.get(g[0])!; separate[j].push(feature); values.set(j, feature); }
        else for (const id of g) pooled[index.get(id)!] = feature;
      });
      if (countSkipped && previous?.index === entry.index - 1) {
        for (const [j, after] of values) {
          const before = previous.features.get(j);
          if (!before) continue;
          const m = moments[(entry.index - 1) * jCount + j], value = after.value - before.value;
          m.n++; const delta = value - m.mean; m.mean += delta / m.n; m.m2 += delta * (value - m.mean); m.noise += before.noise + after.noise;
        }
      }
      previous = { index: entry.index, features: values };
    }
    return { separate, pooled };
  }
  for (const [c, city] of cities.entries()) {
    if (c % 128 === 0) yield { phase: 'profile-features', completed: c, total: cities.length } satisfies FitProgress;
    const { separate, pooled } = features(city, true);
    const r = region.get(city)!;
    if (!regional.has(r)) regional.set(r, { numerator: new Float64Array(jCount), denominator: new Float64Array(jCount) });
    const centre = regional.get(r)!;
    for (let j = 0; j < jCount; j++) {
      const entries = separate[j].length ? separate[j] : pooled[j] ? [pooled[j]!] : [];
      if (!entries.length) continue;
      let weighted = 0, total = 0, exposure = 0;
      for (const e of entries) {
        const w = 2 ** (-Math.max(0, years(source.referenceDate, e.date)) / 5) * (e.context === source.targetContext ? 2 : 1);
        weighted += w * e.value; total += w; exposure += e.exposure;
      }
      const pos = c * jCount + j, value = weighted / total, info = exposure / entries.length, weight = Math.sqrt(info + 1);
      raw[pos] = value; information[pos] = info; centre.numerator[j] += weight * value; centre.denominator[j] += weight;
    }
  }
  const rates: number[][] = Array.from({ length: jCount }, () => []), transitions = [];
  for (let t = 0; t < events.length - 1; t++) {
    const dt = years(events[t + 1].date, events[t].date); if (!(dt > 0)) continue;
    for (let j = 0; j < jCount; j++) {
      const m = moments[t * jCount + j]; if (m.n < 3) continue;
      const rate = Math.max(0, (m.m2 / (m.n - 1) - m.noise / m.n) / dt); rates[j].push(rate);
      transitions.push({ before: events[t].id, after: events[t + 1].id, component: j, municipalities: m.n, variancePerYear: rate });
    }
  }
  const all = rates.flat(), commonVariance = clip(all.length ? median(all) : 0.25 ** 2, 0.03 ** 2, 0.8 ** 2);
  const filterScales = Float64Array.from(rates, xs => Math.sqrt(clip((4 * commonVariance + sum(xs)) / (4 + xs.length), 0.03 ** 2, 0.8 ** 2)));
  const targets = [...source.targets].sort((a, b) => a.id.localeCompare(b.id)), cityIndex = new Map(cities.map((id, i) => [id, i]));
  const means = new Float64Array(targets.length * jCount), profileSd = new Float64Array(means.length), fallback = new Uint8Array(means.length);
  for (const [c, target] of targets.entries()) {
    if (c % 128 === 0) yield { phase: 'profile-filter', completed: c, total: targets.length } satisfies FitProgress;
    const original = cityIndex.get(target.id)!, { separate, pooled } = features(target.id, false), centre = regional.get(region.get(target.id)!);
    for (let j = 0; j < jCount; j++) {
      const pos = original * jCount + j, out = c * jCount + j;
      const gamma = centre?.denominator[j] ? centre.numerator[j] / centre.denominator[j] : 0;
      const weight = information[pos] / (information[pos] + 500);
      let mean = Number.isFinite(raw[pos]) ? weight * raw[pos] + (1 - weight) * gamma : gamma, variance = 1;
      const entries = separate[j].length ? separate[j] : pooled[j] ? [pooled[j]!] : [];
      let previous: string | undefined;
      for (const e of entries) {
        if (previous) variance += filterScales[j] ** 2 * years(e.date, previous);
        const noise = e.noise + (e.context === source.targetContext ? 0 : 0.15 ** 2), gain = variance / (variance + noise);
        mean += gain * (e.value - mean); variance *= 1 - gain; previous = e.date;
      }
      if (previous) variance += filterScales[j] ** 2 * Math.max(0, years(source.referenceDate, previous));
      fallback[out] = separate[j].length ? 0 : pooled[j] ? 1 : 2;
      means[out] = mean; profileSd[out] = Math.sqrt(variance + (fallback[out] ? 0.45 ** 2 : 0.10 ** 2));
    }
  }
  return { componentIds: ids, municipalityIds: targets.map(t => t.id), regions: targets.map(t => t.region),
    electors: Float64Array.from(targets, t => t.electors), means, profileSd, fallback, filterScales,
    futureScales: new Float64Array(jCount).fill(Math.sqrt(commonVariance)),
    audit: { commonLocalVariancePerYear: commonVariance, transitions, unmatchedGroups, historicalUnits: cities.length,
      pooledFallbackCells: fallback.filter(x => x === 1).length, missingHistoryCells: fallback.filter(x => x === 2).length, workspaceBytes } };
}

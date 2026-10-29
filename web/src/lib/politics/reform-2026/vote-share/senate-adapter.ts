import { clip, day, sum } from './types';
import type { MunicipalObservation } from './profiles';
import { SENATE_ADAPTER_VERSION, type GeographicProfile, type SenateAdapter } from './forecast-types';
export interface PairedHistory {
  date: string;
  availableDate: string;
  minimumAge: 18 | 25;
  sourceVersions: string[];
  rows: { municipality: string; camera: MunicipalObservation; senato: MunicipalObservation }[];
}
const key = (g: string[]) => JSON.stringify([...g].sort());
/** Descriptive 18+ contrasts, NOT a second Gaussian shock or a Senate=European
 * relabel. Only observable common groups enter. A pooled contrast is applied
 * to the whole group; it does not manufacture separate component counts. */
export function prepareSenateAdapter(
  camera: GeographicProfile,
  referenceDate: string,
  history: PairedHistory
): SenateAdapter {
  if (
    history.minimumAge !== 18 ||
    day(history.date) > day(referenceDate) ||
    day(history.availableDate) > day(referenceDate)
  )
    throw new Error('SENATE_PAIRED_BASELINE_UNAVAILABLE: require cutoff-eligible common 18+ history');
  const ids = camera.componentIds,
    known = new Set(ids),
    n = ids.length + 1;
  const rows = new Map<string, (typeof history.rows)[number]>(),
    aggregated = new Map<string, { members: string[]; c: number; s: number; cv: number; sv: number }>();
  let ce = 0,
    se = 0,
    cnv = 0,
    snv = 0;
  for (const r of history.rows) {
    if (rows.has(r.municipality)) throw new Error('Duplicate paired municipality');
    for (const x of [r.camera, r.senato]) {
      const members = x.groups.flat();
      if (
        !Number.isSafeInteger(x.electors) ||
        x.electors <= 0 ||
        !Number.isSafeInteger(x.nonValid) ||
        x.nonValid < 0 ||
        x.counts.length !== x.groups.length ||
        x.counts.some((v) => !Number.isSafeInteger(v) || v < 0) ||
        sum(x.counts) + x.nonValid !== x.electors ||
        new Set(members).size !== members.length ||
        members.some((id) => !known.has(id))
      )
        throw new Error('Invalid original paired grouped counts');
    }
    if (r.camera.electors !== r.senato.electors)
      throw new Error('PAIRED_ELECTORATE_MISMATCH: review the common 18+ geography before adapting');
    rows.set(r.municipality, r);
    ce += r.camera.electors;
    se += r.senato.electors;
    cnv += r.camera.nonValid;
    snv += r.senato.nonValid;
    const senate = new Map(r.senato.groups.map((g, i) => [key(g), r.senato.counts[i]]));
    r.camera.groups.forEach((g, i) => {
      const value = senate.get(key(g));
      if (value === undefined) return;
      const k = key(g),
        a = aggregated.get(k) ?? { members: [...g].sort(), c: 0, s: 0, cv: 0, sv: 0 };
      a.c += r.camera.counts[i];
      a.s += value;
      a.cv += sum(r.camera.counts);
      a.sv += sum(r.senato.counts);
      aggregated.set(k, a);
    });
  }
  if (!rows.size) throw new Error('Missing paired history');
  const selected = ids.map(
    (id) =>
      [...aggregated.values()]
        .filter((g) => g.members.includes(id) && g.c > 0 && g.s > 0 && g.cv > 0 && g.sv > 0)
        .sort(
          (a, b) => a.members.length - b.members.length || key(a.members).localeCompare(key(b.members))
        )[0]
  );
  const nationalLogRatios = Float64Array.from(selected, (a) =>
    a ? clip(Math.log(a.s / a.sv / (a.c / a.cv)), -1, 1) : 0
  );
  const contrasts: SenateAdapter['contrasts'] = selected.map((a, i) => ({
    component: ids[i],
    method: !a ? 'fallback' : a.members.length === 1 ? 'separate' : 'pooled',
    group: a?.members ?? [],
    camera: a ? a.c / a.cv : null,
    senato: a ? a.s / a.sv : null
  }));
  const logOdds = (p: number) => Math.log(p / (1 - p));
  const nonValidLogOdds =
    cnv > 0 && cnv < ce && snv > 0 && snv < se ? clip(logOdds(snv / se) - logOdds(cnv / ce), -1, 1) : 0;
  const profile = structuredClone(camera);
  camera.municipalityIds.forEach((id, c) => {
    const r = rows.get(id);
    if (!r) {
      for (let j = 0; j < n; j++) {
        profile.fallback[c * n + j] = 2;
        profile.profileSd[c * n + j] = Math.hypot(camera.profileSd[c * n + j], 0.45);
      }
      return;
    }
    const localC = new Map(r.camera.groups.map((g, i) => [key(g), r.camera.counts[i]])),
      localS = new Map(r.senato.groups.map((g, i) => [key(g), r.senato.counts[i]]));
    const cv = sum(r.camera.counts),
      sv = sum(r.senato.counts),
      weight = Math.min(cv, sv) / (Math.min(cv, sv) + 500);
    const c0 = (r.camera.nonValid + 0.5) / (r.camera.electors + 1),
      s0 = (r.senato.nonValid + 0.5) / (r.senato.electors + 1);
    profile.means[c * n] += weight * clip(logOdds(s0) - logOdds(c0) - nonValidLogOdds, -1, 1);
    for (let j = 0; j < ids.length; j++) {
      const a = selected[j];
      if (!a) {
        profile.fallback[c * n + j + 1] = 2;
        profile.profileSd[c * n + j + 1] = Math.hypot(camera.profileSd[c * n + j + 1], 0.45);
        continue;
      }
      const yc = localC.get(key(a.members)),
        ys = localS.get(key(a.members));
      if (yc === undefined || ys === undefined || !cv || !sv) {
        profile.fallback[c * n + j + 1] = 2;
        profile.profileSd[c * n + j + 1] = Math.hypot(camera.profileSd[c * n + j + 1], 0.45);
        continue;
      }
      const lc = (yc + 0.5) / (cv + 0.5 * r.camera.groups.length),
        ls = (ys + 0.5) / (sv + 0.5 * r.senato.groups.length);
      profile.means[c * n + j + 1] += weight * clip(Math.log(ls / lc) - nationalLogRatios[j], -1, 1);
      if (a.members.length > 1) profile.fallback[c * n + j + 1] = 1;
    }
  });
  return {
    version: SENATE_ADAPTER_VERSION,
    referenceDate,
    pairedDate: history.date,
    componentIds: [...ids],
    nationalLogRatios,
    nonValidLogOdds,
    profile,
    sourceVersions: [...history.sourceVersions],
    contrasts,
    assumptions: [
      'Senate reference is a deterministic logratio pushforward of Camera posterior draws using the paired common-18+ baseline; European posterior coordinates are never used as Senate.',
      'Compatible national contrasts are clipped to +/-1; local residual contrasts are clipped and shrunk by exposure/(exposure+500). Pooled contrasts are not inferred historical component splits.',
      'One paired election cannot estimate future forecast-error covariance. Dependence profiles are explicit Gaussian-copula assumptions, and marginal movement/profile variance budgets are not augmented.',
      'Senate filtering/future scales inherit Camera movement budgets. Missing paired reference-profile contrasts add declared 0.45 SD in quadrature to the Senate profile state, with labelled zero-gap fallbacks; coupling never adds another shock. This is a contrast adapter, not a direct Senate fit or empirical Senate calibration.',
      'Senate 2018 25+ observations are excluded from the central 18+ adapter.'
    ]
  };
}
export function adaptSenateReference(
  political: ArrayLike<number>,
  nonValid: number,
  adapter: SenateAdapter,
  gapMultiplier: number
) {
  if (
    political.length !== adapter.componentIds.length ||
    !Number.isFinite(gapMultiplier) ||
    gapMultiplier < 0
  )
    throw new Error('Invalid Senate adapter basis/sensitivity');
  const q = Float64Array.from(
      political,
      (p, j) => p * Math.exp(gapMultiplier * adapter.nationalLogRatios[j])
    ),
    mass = sum(q);
  if (!(mass > 0)) throw new Error('Empty Senate reference');
  for (let j = 0; j < q.length; j++) q[j] /= mass;
  const a =
    nonValid === 0 || nonValid === 1
      ? nonValid
      : 1 / (1 + Math.exp(-(Math.log(nonValid / (1 - nonValid)) + gapMultiplier * adapter.nonValidLogOdds)));
  return { political: q, nonValid: a };
}

import { readSheet } from 'read-excel-file/node';
import type { Gender } from '../../src/lib/politics/reform-2026/input';
import { mean, variance, sum } from '../../src/lib/politics/reform-2026/generation-math';
import type { PreferenceParameters } from '../../src/lib/politics/reform-2026/generation-types';
import { archiveMember, decode, delimited, normalizeName } from './formats';
import { ministryWorkbookRows } from './ministry-workbook';
import { sourceInteger } from './reform-history';
import { sha256 } from './sources';

export interface PreferenceObservation {
  election: string;
  region: string;
  province: string;
  municipality: string;
  list: string;
  ballots: number;
  candidates: { id: string; gender: Gender; marks: number }[];
}
const normalized = (s: string) => normalizeName(s).replace(/^reggionellemilia$/, 'reggioemilia');
const key = (...s: string[]) => JSON.stringify(s.map(normalized));
const personId = (surname: string, name: string, birth: string) => sha256(key(surname, name, birth)).slice(0, 24);
function recordedGender(value: string): Gender {
  if (!['M', 'F'].includes(value)) throw new Error(`Missing recorded source gender ${value}`);
  return value === 'M' ? 'male' : 'female';
}
const laterCsv = (b: Uint8Array) => delimited(new TextEncoder().encode(decode(b).text.split(/\r?\n/).slice(2).join('\n')), ';');

export async function prepareErPreferences(source: (id: string) => Promise<Buffer>) {
  const roster = (await readSheet(await source('er-roster-2024'))).slice(1);
  const people = new Map<string, { id: string; gender: Gender; slate: string }>();
  for (const r of roster) {
    const k = key(String(r[0]), `${r[2]} ${r[3]}`);
    if (people.has(k)) throw new Error(`Ambiguous ER roster person ${k}`);
    const birth: unknown = r[7];
    if (!(birth instanceof Date)) throw new Error('Invalid recorded roster birth date');
    people.set(k, { id: personId(String(r[2]), String(r[3]), birth.toISOString().slice(0, 10)),
      gender: recordedGender(String(r[8])), slate: key(String(r[0]), String(r[9])) });
  }
  const votes = laterCsv(await source('er-votes-published'));
  const preferences = laterCsv(await source('er-preferences-published'));
  const raw = await source('regional-2024');
  const rawPrefs = delimited(archiveMember(raw, 'Regionali_EmiliaRomagna_2024_Preferenze.csv'), ';');
  const rawVotes = delimited(archiveMember(raw, 'Regionali_EmiliaRomagna_2024_Scrutini.csv'), ';');
  // List numbers are not stable between the two releases. Establish each
  // source-list/roster-list link from exact names, then verify all filed slots.
  const sourceListSlots = new Map<string, string>();
  for (const r of rawPrefs) {
    const p = people.get(key(r.CIRCOSCRIZIONE, `${r.COGNOME} ${r.NOME}`));
    if (!p) continue;
    const k = key(r.CIRCOSCRIZIONE, r.LISTA);
    if (sourceListSlots.has(k) && sourceListSlots.get(k) !== p.slate) throw new Error('Conflicting ER list-to-roster identity');
    sourceListSlots.set(k, p.slate);
  }
  const rosterSlots = new Map(roster.map(r => [key(String(r[0]), String(r[9]), String(r[1])), people.get(key(String(r[0]), `${r[2]} ${r[3]}`))!]));
  const nameDifferences = new Set<string>();
  for (const r of rawPrefs) {
    const slate = sourceListSlots.get(key(r.CIRCOSCRIZIONE, r.LISTA));
    const p = slate ? rosterSlots.get(key(...JSON.parse(slate), r.NUM_CANDIDATO)) : undefined;
    if (!p) throw new Error(`Missing ER recorded roster slot ${r.CIRCOSCRIZIONE}:${r.LISTA}:${r.NUM_CANDIDATO}`);
    const k = key(r.CIRCOSCRIZIONE, `${r.COGNOME} ${r.NOME}`), old = people.get(k);
    if (old && old.id !== p.id) throw new Error(`ER roster identity conflict ${k}: source list ${r.LISTA}, slot ${r.NUM_CANDIDATO}`);
    if (!old) nameDifferences.add(k);
    people.set(k, p);
  }
  const observations = new Map<string, PreferenceObservation>();
  for (const r of votes) {
    const k = key(r.Provincia, r.cod_istat, r.Lista);
    const entry = observations.get(k) ?? { election: 'er-2024', region: '08', province: normalizeName(r.Provincia),
      municipality: r.cod_istat, list: normalizeName(r.Lista), ballots: 0, candidates: [] };
    entry.ballots += sourceInteger(r.Voti); observations.set(k, entry);
  }
  const rosterSlates = new Map<string, Set<string>>();
  for (const p of people.values()) rosterSlates.set(p.slate, new Set([...(rosterSlates.get(p.slate) ?? []), p.id]));
  const matchedSlates = new Map<string, string>();
  for (const r of preferences) {
    const k = key(r.Provincia, r.cod_istat, r.Lista), entry = observations.get(k);
    const p = people.get(key(r.Provincia, r.Candidato));
    if (!p || !entry) throw new Error(`Missing ER roster/list-vote join ${r.Provincia}:${r.Candidato}:${r.Lista}`);
    if (matchedSlates.has(key(r.Provincia, r.Lista)) && matchedSlates.get(key(r.Provincia, r.Lista)) !== p.slate) throw new Error('ER list-to-roster conflict');
    matchedSlates.set(key(r.Provincia, r.Lista), p.slate);
    entry.candidates.push({ id: p.id, gender: p.gender, marks: sourceInteger(r.Voti) });
  }
  for (const o of observations.values()) {
    const slate = matchedSlates.get(key(o.province, o.list));
    // A blank ministerial placeholder is not a positive-vote list.
    if (!slate || o.candidates.length !== rosterSlates.get(slate)?.size) throw new Error(`Incomplete ER observed slate ${o.province}:${o.municipality}:${o.list}`);
  }
  const publishedMarks = sum(preferences.map(r => sourceInteger(r.Voti)));
  return { observations: [...observations.values()], audit: {
    election: 'er-2024', rosterSlots: roster.length, people: new Set([...people.values()].map(p => p.id)).size,
    sourceNameDifferencesJoinedByFiledSlot: [...nameDifferences],
    rawMarks: sum(rawPrefs.map(r => sourceInteger(r.PREFERENZE))), publishedMarks,
    rawListVotes: sum(rawVotes.map(r => sourceInteger(r.VOTI_LISTA))), publishedListVotes: sum(votes.map(r => sourceInteger(r.Voti))),
    reconciliation: 'Use the later regional municipal preference and section-vote releases together. Preserve differences from the ministerial archive; their causes are not inferred.',
    marksDifference: publishedMarks - sum(rawPrefs.map(r => sourceInteger(r.PREFERENZE))) } };
}

/** National European municipal workbooks include recorded gender and birth
 * metadata. The source has no protected capolista. These are intensity and
 * dispersion observations, never future named-candidate strength estimates. */
export function prepareEuropeanPreferences(bytes: Uint8Array) {
  const votes = delimited(archiveMember(bytes, 'EUROPEE_SCRUTINI&LISTE_ITALIA_LivComune.csv'), ';');
  const voteMap = new Map<string, { ballots: number; region: string }>();
  for (const r of votes) {
    const k = key(r.DESCREGIONE, r.DESCPROVINCIA, r.DESCCOMUNE, r.DESCLISTA);
    if (voteMap.has(k)) throw new Error(`Duplicate European list vote ${k}`);
    voteMap.set(k, { ballots: sourceInteger(r.NUMVOTI), region: normalizeName(r.DESCREGIONE) });
  }
  const observations: PreferenceObservation[] = [], audit = [];
  for (let i = 1; i <= 5; i++) {
    const groups = new Map<string, PreferenceObservation>();
    let rows = 0;
    const sheet = ministryWorkbookRows(archiveMember(bytes, `Europee_Preferenze_Circ${i}_LivComune.xlsx`));
    const header = sheet.next().value;
    if (header?.join('|') !== `DATAELEZIONE|CIRCOSCRIZIONE|REGIONE|PROVINCIA|COMUNE|${i === 1 ? 'DESCRLISTA' : i === 2 ? 'DESCRLISTACONTR' : 'DESCRLISTACCP'}|COGNOME|NOME|DATANASCITA|LUOGONASCITA|SESSO|PREFERENZE`) throw new Error('European workbook schema changed');
    for (const r of sheet) {
      rows++;
      const k = key(r[2], r[3], r[4], r[5]), v = voteMap.get(k);
      if (!v) throw new Error(`Missing European ballot budget ${k}`);
      const o = groups.get(k) ?? { election: 'european-2024', region: v.region, province: normalizeName(r[3]),
        municipality: key(r[2], r[3], r[4]), list: normalizeName(r[5]), ballots: v.ballots, candidates: [] };
      o.candidates.push({ id: personId(r[6], r[7], r[8]), gender: recordedGender(r[10]), marks: sourceInteger(r[11]) });
      groups.set(k, o);
    }
    const rosters = new Map<string, Set<string>>();
    for (const o of groups.values()) rosters.set(o.list, new Set([...(rosters.get(o.list) ?? []), ...o.candidates.map(c => c.id)]));
    for (const o of groups.values()) if (o.candidates.length !== rosters.get(o.list)?.size) throw new Error(`Incomplete European roster ${i}/${o.municipality}/${o.list}`);
    observations.push(...groups.values());
    audit.push({ constituency: i, rows, observations: groups.size, marks: sum([...groups.values()].flatMap(g => g.candidates.map(c => c.marks))) });
  }
  if (observations.length !== voteMap.size) throw new Error('European preference/list-vote footprint differs');
  return { observations, audit };
}

function parameters(observations: readonly PreferenceObservation[]): PreferenceParameters {
  const elections = [...new Set(observations.map(o => o.election))];
  // Equal election weight prevents thousands of European sections/rows from
  // masquerading as independent temporal observations. Province/list pooling is
  // empirical shrinkage with a declared 10,000-ballot prior below.
  const intensities = elections.map(e => {
    const os = observations.filter(o => o.election === e);
    return sum(os.flatMap(o => o.candidates.map(c => c.marks))) / sum(os.map(o => o.ballots));
  });
  const pools = new Map<string, Map<string, number>>(), localValues = new Map<string, { value: number; weight: number; noise: number }[]>();
  for (const o of observations) {
    const k = JSON.stringify([o.election, o.province, o.list]);
    const pool = pools.get(k) ?? new Map<string, number>(), marks = sum(o.candidates.map(c => c.marks));
    for (const c of o.candidates) {
      pool.set(c.id, (pool.get(c.id) ?? 0) + c.marks);
      if (marks >= 10) {
        const id = JSON.stringify([k, c.id]), entries = localValues.get(id) ?? [];
        entries.push({ value: Math.log((c.marks + 0.5) / (marks + o.candidates.length * 0.5)), weight: Math.min(marks, 1000),
          noise: Math.max(0, 1 / (c.marks + 0.5) - 1 / (marks + o.candidates.length * 0.5)) });
        localValues.set(id, entries);
      }
    }
    pools.set(k, pool);
  }
  // Pooled roster log-strength dispersion and within-person local residuals
  // are distinct estimates. Jeffreys 1/2 smoothing retains observed zeros;
  // subtract a delta-method count-noise approximation, bounded at zero.
  const strengthSigma = Math.sqrt(mean([...pools.values()].map(p => variance([...p.values()].map(n => Math.log(n + 0.5))))));
  const localVariance = mean([...localValues.values()].filter(es => es.length > 1).map(es => {
    const w = sum(es.map(e => e.weight)), mu = sum(es.map(e => e.value * e.weight)) / w;
    return Math.max(0, sum(es.map(e => e.weight * ((e.value - mu) ** 2 - e.noise))) / w);
  }));
  return { intensity: mean(intensities), strengthSigma, localSigma: Math.sqrt(localVariance) };
}
export function fitPreferenceModel(input: readonly PreferenceObservation[]) {
  const exclusions: { election: string; municipality: string; list: string; reason: string }[] = [];
  const observations = input.filter(o => {
    const invalid = o.candidates.length < 2 || new Set(o.candidates.map(c => c.id)).size !== o.candidates.length ||
      o.candidates.some(c => c.marks > o.ballots) || sum(o.candidates.map(c => c.marks)) > o.ballots * (o.election === 'er-2024' ? 2 : 3);
    if (invalid) exclusions.push({ election: o.election, municipality: o.municipality, list: o.list, reason: 'Incomplete/duplicate roster or invalid source ballot budget' });
    return !invalid && o.ballots > 0;
  });
  const baseline = parameters(observations);
  if (Object.values(baseline).some(v => !Number.isFinite(v) || v < 0)) throw new Error('Non-finite preference fit');
  const pools = [];
  for (const election of [...new Set(observations.map(o => o.election))].sort()) for (const scope of [...new Set(observations.filter(o => o.election === election).map(o => o.region))].sort()) {
    const os = observations.filter(o => o.election === election && o.region === scope), fitted = parameters(os);
    const weight = sum(os.map(o => o.ballots)) / (10000 + sum(os.map(o => o.ballots)));
    pools.push({ election, scope, observations: os.length, parameters: {
      intensity: weight * fitted.intensity + (1 - weight) * baseline.intensity,
      strengthSigma: weight * fitted.strengthSigma + (1 - weight) * baseline.strengthSigma,
      localSigma: weight * fitted.localSigma + (1 - weight) * baseline.localSigma } });
  }
  const diagnostics = observations.map(o => {
    const marks = o.candidates.map(c => c.marks), total = sum(marks);
    return { election: o.election, region: o.region, province: o.province, intensity: total / o.ballots,
      leadingShare: total ? Math.max(...marks) / total : 0,
      zeroShare: marks.filter(m => m === 0).length / marks.length,
      tiedPairs: marks.reduce((s, x, i) => s + marks.slice(i + 1).filter(y => y === x).length, 0) };
  });
  const folds = [];
  for (const election of ['er-2024', 'european-2024']) {
    const scopes = [...new Set(observations.filter(o => o.election === election).map(o => election === 'er-2024' ? o.province : o.region))].sort();
    for (const scope of scopes) {
      const held = observations.filter(o => o.election === election && (election === 'er-2024' ? o.province : o.region) === scope);
      const ids = new Set(held.flatMap(o => o.candidates.map(c => c.id)));
      const train = observations.filter(o => !o.candidates.some(c => ids.has(c.id)));
      if (!train.length) throw new Error(`No independent candidate clusters for ${scope}`);
      const fit = parameters(train), actual = parameters(held);
      const leading = (os: readonly PreferenceObservation[]) => mean(os.filter(o => sum(o.candidates.map(c => c.marks)) > 0)
        .map(o => Math.max(...o.candidates.map(c => c.marks)) / sum(o.candidates.map(c => c.marks))));
      const zeros = (os: readonly PreferenceObservation[]) => mean(os.map(o => o.candidates.filter(c => c.marks === 0).length / o.candidates.length));
      const ties = (os: readonly PreferenceObservation[]) => mean(os.map(o => {
        const ns = o.candidates.map(c => c.marks), pairs = ns.length * (ns.length - 1) / 2;
        return ns.reduce((n, x, i) => n + ns.slice(i + 1).filter(y => y === x).length, 0) / pairs;
      }));
      folds.push({ election, scope, trainObservations: train.length, heldObservations: held.length, heldPeople: ids.size,
        intensityError: Math.abs(fit.intensity - actual.intensity), leadingShareError: Math.abs(leading(train) - leading(held)),
        zeroShareError: Math.abs(zeros(train) - zeros(held)), tiePairShareError: Math.abs(ties(train) - ties(held)),
        strengthSigmaError: Math.abs(fit.strengthSigma - actual.strengthSigma), localSigmaError: Math.abs(fit.localSigma - actual.localSigma) });
    }
  }
  const validation = { intensityMae: mean(folds.map(f => f.intensityError)), leadingShareMae: mean(folds.map(f => f.leadingShareError)), folds };
  const passed = validation.intensityMae <= 0.25 && validation.leadingShareMae <= 0.25;
  return { baseline, pools, audit: { observations: observations.length, exclusions, validation, passed,
    diagnostics: ['er-2024', 'european-2024'].map(e => ({ election: e, observations: diagnostics.filter(d => d.election === e).length,
      meanIntensity: mean(diagnostics.filter(d => d.election === e).map(d => d.intensity)),
      meanLeadingShare: mean(diagnostics.filter(d => d.election === e).map(d => d.leadingShare)),
      meanZeroShare: mean(diagnostics.filter(d => d.election === e).map(d => d.zeroShare)),
      meanTiedPairs: mean(diagnostics.filter(d => d.election === e).map(d => d.tiedPairs)) })) } };
}

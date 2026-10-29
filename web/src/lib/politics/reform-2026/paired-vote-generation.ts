import type { Chamber, ReferenceScope, ResolvedChamberReference } from './input';
import type { ChamberProfile, GenerationTrace, HistoricalMunicipality, ReformGenerationScenario, ReformGenerationSnapshot } from './generation-types';
import { apportion, generationNormal, generationRng, logistic, logit, sum } from './generation-math';

export interface ProjectedMunicipality {
  municipality: HistoricalMunicipality;
  camera: { shares: Map<string, number>; abstention: number; dates: Map<string, string>; measurement?: Map<string, number>; abstentionDate: string };
  senato: { shares: Map<string, number>; abstention: number; dates: Map<string, string>; measurement?: Map<string, number>; abstentionDate: string };
}
const scopeKey = (s: ReferenceScope) => JSON.stringify([s.kind, s.kind === 'national' ? null : s.code]);
const matches = (m: HistoricalMunicipality, s: ReferenceScope) => s.kind === 'national' ||
  (s.kind === 'municipality' ? m.code : s.kind === 'province' ? m.provinceId : m.regionId) === s.code;
const rank = (s: ReferenceScope) => ['national', 'region', 'province', 'municipality'].indexOf(s.kind);
export const renewedChamberIds = (s: Pick<ReformGenerationScenario, 'renewal'>): Chamber[] => s.renewal === 'joint' ? ['camera', 'senato'] : [s.renewal];

/** Unit variance in both chambers for every valid PSD profile. The shared and
 * residual pieces partition a marginal budget, rather than adding Senate noise.
 * Residuals may be positively or negatively correlated. Named component streams
 * remain stable across profiles for paired sensitivity. */
export function pairedStandardShock(seed: string, simulationId: number, profile: ChamberProfile, ...scope: string[]): { camera: number; senato: number } {
  const shared = generationNormal(seed, simulationId, 'votes-shared', ...scope);
  const c = generationNormal(seed, simulationId, 'votes-camera', ...scope);
  const s = generationNormal(seed, simulationId, 'votes-senato', ...scope);
  const a = Math.sqrt(profile.sharedVariance), b = Math.sqrt(1 - profile.sharedVariance), r = profile.residualCorrelation;
  return { camera: a * shared + b * c, senato: a * shared + b * (r * c + Math.sqrt(1 - r * r) * s) };
}

/** R26-04 common anchor, preserving its midpoint in log odds. Historical
 * chamber contrast is shrunk by 1/2; neither supplied chamber value is derived
 * from the other. Partial-share normalization is handled explicitly below. */
export function deriveCommonReferences(anchor: number, cameraBaseline: number, senateBaseline: number, gapMultiplier: number) {
  if (anchor === 0 || anchor === 1) return { camera: anchor, senato: anchor };
  const contrast = Math.max(-1, Math.min(1, logit(senateBaseline) - logit(cameraBaseline))) * 0.5 * gapMultiplier;
  return { camera: logistic(logit(anchor) - contrast / 2), senato: logistic(logit(anchor) + contrast / 2) };
}

function aggregate(units: readonly ProjectedMunicipality[], ch: Chamber, id: string): number {
  const valid = units.map(u => u.municipality[ch].electors * (1 - u[ch].abstention));
  return sum(units.map((u, i) => valid[i] * (u[ch].shares.get(id) ?? 0))) / (sum(valid) || 1);
}

/** IPF fixes only the supplied margins. Unspecified lists retain their local
 * relative proportions, rather than acquiring artificial national targets.
 * Municipal turnout and structural filing zeros remain fixed. */
function reconcileReferences(units: readonly ProjectedMunicipality[], ch: Chamber, targets: Map<string, number>, available: (u: ProjectedMunicipality, id: string) => boolean) {
  const ids = [...new Set(units.flatMap(u => [...u[ch].shares.keys()]))], targetIds = [...targets.keys()];
  const weights = units.map(u => u.municipality[ch].electors * (1 - u[ch].abstention));
  const total = sum(weights);
  if (!total) return;
  // At 100% supplied mass the free remainder is exactly zero. Remove it
  // directly: multiplicative fitting would otherwise approach zero only slowly.
  const fullMass = Math.abs(sum([...targets.values()]) - 1) < 1e-12;
  for (const u of units) for (const id of ids) {
    if (targets.get(id) === 0 || fullMass && !targets.has(id)) u[ch].shares.set(id, 0);
    else if (targets.has(id) && available(u, id) && !u[ch].shares.get(id)) u[ch].shares.set(id, 1e-12);
  }
  for (const id of targetIds) {
    const maximum = sum(units.map((u, i) => available(u, id) ? weights[i] : 0)) / total;
    const desired = targets.get(id)!;
    if (desired > maximum + 1e-12) throw new RangeError(`Share reference exceeds filed geographic support: ${ch}/${id} requests ${(desired * 100).toFixed(6)}%, but available valid votes allow at most ${(maximum * 100).toFixed(6)}%`);
    // A reference at its geographic maximum owns every valid vote there.
    // Remove competing mass exactly instead of approaching that boundary slowly.
    if (desired > 0 && Math.abs(desired - maximum) < 1e-12) {
      for (const u of units) if (available(u, id)) for (const other of ids) if (other !== id) u[ch].shares.set(other, 0);
    }
  }
  for (let iteration = 0; iteration < 1000; iteration++) {
    const totals = targetIds.map(id => sum(units.map((u, i) => weights[i] * (u[ch].shares.get(id) ?? 0))));
    targetIds.forEach((id, j) => {
      const desired = targets.get(id)! * total;
      if (desired && !totals[j]) throw new RangeError(`Share references leave no valid-vote capacity: ${ch}/${id}`);
      const factor = totals[j] ? desired / totals[j] : 0;
      for (const u of units) u[ch].shares.set(id, (u[ch].shares.get(id) ?? 0) * factor);
    });
    for (const u of units) {
      const mass = sum(ids.map(id => u[ch].shares.get(id) ?? 0));
      if (!mass && u.municipality[ch].electors * (1 - u[ch].abstention) > 0) throw new RangeError('Override removes all locally filed support');
      for (const id of ids) u[ch].shares.set(id, mass ? (u[ch].shares.get(id) ?? 0) / mass : 0);
    }
    const error = Math.max(...targetIds.map(id => Math.abs(aggregate(units, ch, id) - targets.get(id)!)));
    if (error < 1e-9) return;
  }
  throw new RangeError('Share references conflict with filed geographic support; reconciliation did not converge');
}

export function preparePairedVoteReferences(scenario: ReformGenerationScenario, snapshot: ReformGenerationSnapshot): { units: ProjectedMunicipality[]; trace: GenerationTrace } {
  const s = scenario, profile = snapshot.models.chambers.profiles.find(p => p.id === s.chamberProfileId)!;
  const active = renewedChamberIds(s), filing = new Map<string, Set<string>>();
  for (const c of s.coverage) for (const member of snapshot.memberships.filter(m => m.chamber === c.chamber && m.collegeId === c.collegeId)) {
    const k = `${c.chamber}:${member.municipalityCode}`;
    filing.set(k, new Set([...(filing.get(k) ?? []), ...c.listIds]));
  }
  const available = (u: ProjectedMunicipality, id: string, ch: Chamber) => !!filing.get(`${ch}:${u.municipality.code}`)?.has(id);
  const units: ProjectedMunicipality[] = snapshot.municipalities.map(m => {
    const project = (ch: Chamber) => {
      const votes = new Map(s.lists[ch].map(l => [l.id, 0]));
      for (const historical of m[ch].votes) {
        const mappings = s.correspondences.filter(c => c.chamber === ch && c.pastListId === historical.listId);
        const total = sum(mappings.map(c => c.factor));
        for (const c of mappings) if (filing.get(`${ch}:${m.code}`)?.has(c.futureListId)) votes.set(c.futureListId, votes.get(c.futureListId)! + historical.votes * c.factor / total);
      }
      const valid = sum([...votes.values()]);
      return { shares: new Map([...votes].map(([id, n]) => [id, valid ? n / valid : 0])), abstention: 1 - valid / m[ch].electors,
        dates: new Map(s.lists[ch].map(l => [l.id, snapshot.historicalDate])), abstentionDate: snapshot.historicalDate };
    };
    return { municipality: m, camera: project('camera'), senato: project('senato') };
  });
  const trace: GenerationTrace = { warnings: [], references: [], normalization: [], streamVersion: 'r26-generation-streams-v1', snapshotVersion: snapshot.version, modelVersion: snapshot.models.version };
  const historicalUnits = structuredClone(units);
  if (s.pollReferences) {
    trace.warnings.push({ code: 'POLL_REFERENCE_ASSUMPTIONS', message: `Quote informate dai sondaggi (${s.pollReferences.version}); incertezza e proxy non calibrati.` });
    for (const ch of active) {
      const ordinary = new Set(snapshot.seats.geography.colleges.filter(c => c.chamber === ch && c.kind === 'ordinary').map(c => c.id));
      const codes = new Set(snapshot.memberships.filter(m => m.chamber === ch && ordinary.has(m.collegeId)).map(m => m.municipalityCode));
      const group = units.filter(u => codes.has(u.municipality.code));
      const rows = s.pollReferences.rows.filter(r => r.chamber === ch);
      if (!rows.length) continue;
      const targets = new Map(rows.map(r => [r.listId, r.share]));
      for (const u of group) if (u[ch].abstention === 1 && sum(u.municipality[ch].votes.map(v => v.votes)) > 0) {
        u[ch].abstention = 1 - sum(u.municipality[ch].votes.map(v => v.votes)) / u.municipality[ch].electors;
        for (const r of rows) if (available(u, r.listId, ch)) u[ch].shares.set(r.listId, r.share);
        trace.warnings.push({ code: 'POLL_UNMAPPED_HISTORY_PROXY', message: `Affluenza storica e supporto stimato usati per ${ch}/${u.municipality.code}, senza corrispondenze disponibili.` });
      }

      // Seed genuinely absent support from a declared donor profile, then uniform
      // filed support. Existing local abstention and special ballots are retained.
      for (const r of rows) if (!aggregate(group, ch, r.listId) && r.share > 0) {
        for (const u of group) if (available(u, r.listId, ch)) {
          const donors = sum(u.municipality[ch].votes.filter(v => r.donorHistoricalListIds.includes(v.listId)).map(v => v.votes));
          u[ch].shares.set(r.listId, Math.max(1e-12, donors / Math.max(1, u.municipality[ch].electors)));
        }
        trace.warnings.push({ code: 'POLL_TERRITORIAL_PROXY', message: `Profilo territoriale stimato per ${ch}/${r.listId}.` });
      }
      // Keep historical mass of special/local-only lists within ordinary units.
      const ids = s.lists[ch].map(l => l.id);
      const otherMass = sum(ids.filter(id => !targets.has(id)).map(id => aggregate(group, ch, id)));
      for (const [id, p] of targets) targets.set(id, p * (1 - otherMass));
      for (const id of ids) if (!targets.has(id)) targets.set(id, aggregate(group, ch, id));
      reconcileReferences(group, ch, targets, (u, id) => available(u, id, ch));
      for (const u of group) {
        u[ch].measurement = new Map(rows.map(r => [r.listId, r.sigma]));
        for (const r of rows) u[ch].dates.set(r.listId, s.pollReferences.date);
      }
    }
  }

  const scopes = [...new Map([{ kind: 'national' } as ReferenceScope, ...s.shares.map(r => r.scope), ...s.abstention.map(r => r.scope)].map(sc => [scopeKey(sc), sc])).values()]
    .sort((a, b) => rank(a) - rank(b) || (scopeKey(a) < scopeKey(b) ? -1 : 1));
  for (const scope of scopes) {
    const group = units.filter(u => matches(u.municipality, scope));
    const rows = s.shares.filter(r => scopeKey(r.scope) === scopeKey(scope));
    const abstention = s.abstention.find(r => scopeKey(r.scope) === scopeKey(scope));
    const resolved = new Map<Chamber, Map<string, ResolvedChamberReference>>(active.map(ch => [ch, new Map()]));
    for (const row of rows) {
      const baseline = historicalUnits.filter(u => matches(u.municipality, scope));
      const common = row.common === null ? null : deriveCommonReferences(row.common, aggregate(baseline, 'camera', row.listId), aggregate(baseline, 'senato', row.listId), profile.gapMultiplier);
      for (const ch of active) {
        const value = row[ch] ?? common?.[ch];
        if (value !== undefined && value !== null) resolved.get(ch)!.set(row.listId, { share: value, referenceDate: s.projectionDate, origin: row[ch] === null ? 'model-derived' : 'supplied' });
      }
    }
    for (const ch of active) {
      const supplied = resolved.get(ch)!;
      const a = abstention?.[ch] ?? abstention?.common;
      if (a !== undefined && a !== null) {
        // A scoped turnout adjustment preserves relative municipal odds. Exact
        // endpoints are intentional and do not become missing values.
        const baseline = group.map(u => u[ch].abstention);
        let lower = -64, upper = 64;
        for (let i = 0; i < 80; i++) {
          const shift = (lower + upper) / 2;
          const actual = sum(group.map((u, j) => u.municipality[ch].electors * logistic(logit(baseline[j]) + shift))) / sum(group.map(u => u.municipality[ch].electors));
          if (actual < a) lower = shift; else upper = shift;
        }
        group.forEach((u, i) => { u[ch].abstention = a === 0 || a === 1 ? a : logistic(logit(baseline[i]) + (lower + upper) / 2); u[ch].abstentionDate = s.projectionDate; });
      }
      if (!supplied.size) continue;
      const eligible = s.lists[ch].map(l => l.id);
      const inputTotal = sum([...supplied.values()].map(r => r.share));
      if (inputTotal > 1 + 1e-12 || supplied.size === eligible.length && Math.abs(inputTotal - 1) > 1e-12) {
        if (!inputTotal) throw new RangeError('All overridden valid shares are zero');
        for (const [id, r] of supplied) supplied.set(id, { ...r, share: r.share / inputTotal });
        trace.warnings.push({ code: 'NORMALIZED_SHARES', message: `Le quote valide fornite per ${ch} sono state normalizzate al 100%.` });
        trace.normalization.push({ scope, chamber: ch, reason: 'supplied valid shares normalized', before: inputTotal, after: 1 });
      }
      const remaining = Math.max(0, 1 - sum([...supplied.values()].map(r => r.share)));
      const free = eligible.filter(id => !supplied.has(id)), mass = sum(free.map(id => aggregate(group, ch, id)));
      if (remaining > 1e-12 && free.length && !mass) throw new RangeError('Partial share overrides leave no historical support for the remainder');
      // Historical support is a prior for the remainder, not a set of hard
      // margins: special-territory lists cannot exchange votes with mainland lists.
      const targets = new Map([...supplied].map(([id, r]) => [id, r.share]));
      reconcileReferences(group, ch, targets, (u, id) => available(u, id, ch));
      for (const u of group) for (const id of supplied.keys()) { u[ch].dates.set(id, s.projectionDate); u[ch].measurement?.delete(id); }
    }
    const ids = new Set(scope.kind === 'national' ? active.flatMap(ch => s.lists[ch].map(l => l.id)) : rows.map(r => r.listId));
    for (const id of ids) {
      const ref = (ch: Chamber): ResolvedChamberReference | null => {
        if (!active.includes(ch) || !s.lists[ch].some(l => l.id === id)) return null;
        if (scope.kind !== 'national' && !resolved.get(ch)?.has(id)) return null;
        return resolved.get(ch)?.get(id) ?? { share: aggregate(group, ch, id), referenceDate: s.pollReferences?.rows.some(r => r.chamber === ch && r.listId === id) ? s.pollReferences.date : snapshot.historicalDate, origin: 'model-derived' };
      };
      const camera = ref('camera'), senato = ref('senato'), common = rows.find(r => r.listId === id)?.common;
      trace.references.push({ scope, listIds: { camera: camera ? id : null, senato: senato ? id : null },
        common: common === undefined || common === null ? null : { share: common, referenceDate: s.projectionDate }, camera, senato });
    }
  }
  return { units, trace };
}

export interface GeneratedMunicipalVotes { chamber: Chamber; municipalityCode: string; electors: number; abstention: number; votes: { listId: string; votes: number }[] }
export function generatePairedMunicipalVotes(s: ReformGenerationScenario, snapshot: ReformGenerationSnapshot, prepared: readonly ProjectedMunicipality[], simulationId: number): GeneratedMunicipalVotes[] {
  const profile = snapshot.models.chambers.profiles.find(p => p.id === s.chamberProfileId)!;
  const sigma = snapshot.models.chambers.sigmaPerSqrtDay;
  const shocks = new Map<string, ReturnType<typeof pairedStandardShock>>();
  const component = (...scope: string[]) => {
    const k = JSON.stringify(scope); let v = shocks.get(k);
    if (!v) { v = pairedStandardShock(s.seed, simulationId, profile, ...scope); shocks.set(k, v); } return v;
  };
  const shock = (id: string, date: string, m: HistoricalMunicipality, ch: Chamber, territorialDate = date) => {
    const days = (Date.parse(s.electionDate) - Date.parse(date)) / 86400000;
    if (days < 0) throw new RangeError('Vote reference date is after election date');
    if (territorialDate === date) return Math.sqrt(days) * (sigma.national * component('national', id)[ch] +
      sigma.region * component('region', m.regionId, id)[ch] + sigma.local * component('local', m.code, id)[ch]);
    const territorialDays = (Date.parse(s.electionDate) - Date.parse(territorialDate)) / 86400000;
    return Math.sqrt(days) * sigma.national * component('national', id)[ch] +
      Math.sqrt(territorialDays) * (sigma.region * component('region', m.regionId, id)[ch] + sigma.local * component('local', m.code, id)[ch]);
  };
  const output: GeneratedMunicipalVotes[] = [];
  for (const u of prepared) for (const ch of renewedChamberIds(s)) {
    const m = u.municipality, p = u[ch], ids = [...p.shares.keys()];
    // Exact zero support stays zero. The logit/elector-fraction normalization
    // matches existing mean-mode semantics; supplied references still drift.
    const probabilities = ids.map(id => {
      const electorShare = (p.shares.get(id) ?? 0) * (1 - p.abstention);
      return electorShare === 0 ? 0 : logistic(logit(electorShare) + shock(id, p.dates.get(id)!, m, ch, p.measurement?.has(id) ? snapshot.historicalDate : p.dates.get(id)!) + (p.measurement?.has(id) ? p.measurement.get(id)! * generationNormal(s.seed, simulationId, 'poll-measurement-v1', id) : 0));
    });
    probabilities.push(p.abstention === 0 ? 0 : p.abstention === 1 ? 1 : logistic(logit(p.abstention) + shock('abstention', p.abstentionDate, m, ch)));
    const counts = apportion(m[ch].electors, probabilities.map(p => Math.round(p * 1e12)), generationRng(s.seed, simulationId, 'vote-rounding', ch, m.code));
    output.push({ chamber: ch, municipalityCode: m.code, electors: m[ch].electors, abstention: counts.at(-1)!, votes: ids.map((listId, i) => ({ listId, votes: counts[i] })) });
  }
  return output;
}

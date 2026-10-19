import type { Scenario } from '../../core/types';
import { REFORM_ALGORITHM_ID, type Chamber, type Candidate } from './input';
import { REFORM_MODEL_IDS, REFORM_SCENARIO_VERSION, type ReformGenerationScenario, type ReformGenerationSnapshot } from './generation-types';
import { array, chamber, count, date, literal, nullable, object, ordinaryVotes, preferences, scalar, share, specialVotes, subject, text, union, type Schema, type ValidationIssue } from './shape';
import { validateSeatTableBundle } from './endowments';
import { sum } from './generation-math';

const nonnegative = scalar<number>(v => typeof v === 'number' && Number.isFinite(v) && v >= 0, 'a finite nonnegative number');
const scope = union(object({ kind: literal('national') }), object({ kind: literal('region', 'province', 'municipality'), code: text }));
const modelIds = object({ votes: literal(REFORM_MODEL_IDS.votes), preferences: literal(REFORM_MODEL_IDS.preferences), chambers: literal(REFORM_MODEL_IDS.chambers) });
const list = object({ id: text, name: text, coalitionId: nullable(text), protectedMinority: nullable(object({ regionId: text, legalReference: text })) });
const candidate: Schema<Candidate> = object({ id: text, name: text, birthDate: date, gender: literal('female', 'male'),
  availability: union(object({ kind: literal('available') }), object({ kind: literal('unavailable'), reason: text })),
  externalCandidacies: array(object({ chamber, kind: literal('domestic', 'foreign') })) });
type JsonReadonly<T> = T extends object ? { readonly [K in keyof T]: JsonReadonly<T[K]> } : T;
const scenarioShape: Schema<JsonReadonly<ReformGenerationScenario>> = object({
  schemaVersion: literal(REFORM_SCENARIO_VERSION), kind: literal('politics-reform-scenario'), algorithmId: literal(REFORM_ALGORITHM_ID),
  snapshotVersion: text, modelVersion: text, modelIds, preferenceProfileId: text, chamberProfileId: text, seed: text,
  electionDate: date, projectionDate: date, mode: literal('mean'), renewal: literal('joint', 'camera', 'senato'),
  lists: object({ camera: array(list), senato: array(list) }),
  coverage: array(object({ chamber, collegeId: text, listIds: array(text) })),
  correspondences: array(object({ chamber, pastListId: text, futureListId: text, factor: nonnegative })),
  shares: array(object({ scope, listId: text, common: nullable(share), camera: nullable(share), senato: nullable(share) })),
  abstention: array(object({ scope, common: nullable(share), camera: nullable(share), senato: nullable(share) })),
  candidateGeneration: object({ reuseProbability: share,
    maximumOrdinaryCandidacies: scalar<number>(v => Number.isInteger(v) && Number(v) >= 1 && Number(v) <= 5, '1 through 5') }),
  candidateTemplates: array(object({ chamber, role: literal('ordinary', 'bonus', 'special'), locationId: text, ownerId: text,
    position: scalar<number>(v => Number.isSafeInteger(v) && Number(v) > 0, 'a positive position'), affiliationListId: nullable(text), candidate })),
  suppliedPreferences: array(object({ chamber, collegeId: text, listId: text, preferences })),
  suppliedVotes: array(object({ chamber, collegeId: text, votes: union(ordinaryVotes, specialVotes) })),
  bonusLinks: array(object({ camera: subject, senato: subject, cameraDeclares: scalar<true>(v => v === true, 'true'), senatoDeclares: scalar<true>(v => v === true, 'true') })),
  conversion: nullable(object({ from: text, assumptions: array(text) }))
});

const parameters = object({ intensity: nonnegative, strengthSigma: nonnegative, localSigma: nonnegative });
const historical = object({ electors: count, votes: array(object({ listId: text, votes: count })) });
const snapshotShape: Schema<JsonReadonly<ReformGenerationSnapshot>> = object({
  schemaVersion: literal(1), kind: literal('politics-reform-generation'), version: text, historicalDate: date,
  sourceVersions: array(text), assumptions: array(text),
  seats: scalar<ReformGenerationSnapshot['seats']>(v => { try { return validateSeatTableBundle(v as ReformGenerationSnapshot['seats']).length === 0; } catch { return false; } }, 'a valid seat-table bundle'),
  models: object({ schemaVersion: literal(1), version: text, modelIds, sourceVersions: array(text), assumptions: array(text),
    preferences: object({ parameterVersion: text, baseline: parameters,
      pools: array(object({ election: text, scope: text, observations: count, parameters })),
      profiles: array(object({ id: text, intensityMultiplier: nonnegative, concentrationMultiplier: nonnegative, persistence: share, tripleWeight: nonnegative })) }),
    chambers: object({ parameterVersion: text,
      sigmaPerSqrtDay: object({ national: nonnegative, region: nonnegative, local: nonnegative }),
      profiles: array(object({ id: text, sharedVariance: share,
        residualCorrelation: scalar<number>(v => typeof v === 'number' && Number.isFinite(v) && Math.abs(v) <= 1, 'a finite correlation in [-1,1]'), gapMultiplier: nonnegative })) }) }),
  historicalLists: array(object({ id: text, name: text, kind: literal('list', 'special-ballot-group') })),
  historicalCoalitions: array(object({ chamber, listIds: array(text) })),
  municipalities: array(object({ code: text, regionId: text, provinceId: text, name: text, camera: historical, senato: historical })),
  memberships: array(object({ chamber, municipalityCode: text, collegeId: text, population: count }))
});

export function validateReformScenario(value: unknown, snapshot?: ReformGenerationSnapshot): string[] {
  const issues: ValidationIssue[] = [];
  if (!scenarioShape.check(value, '$', issues)) return issues.map(i => `${i.path}: ${i.message}`);
  const s = value as ReformGenerationScenario, errors: string[] = [];
  const unique = <T>(rows: readonly T[], key: (row: T) => string, label: string) => {
    if (new Set(rows.map(key)).size !== rows.length) errors.push(`Duplicate ${label}`);
  };
  const key = (v: unknown) => JSON.stringify(v);
  if (s.projectionDate > s.electionDate) errors.push('Projection date is after election date');
  for (const ch of ['camera', 'senato'] as const) unique(s.lists[ch], l => l.id, `${ch} list`);
  unique(s.coverage, r => key([r.chamber, r.collegeId]), 'coverage');
  for (const c of s.coverage) {
    unique(c.listIds, id => id, 'coverage list');
    if (!c.listIds.length || c.listIds.some(id => !s.lists[c.chamber].some(l => l.id === id))) errors.push(`Invalid coverage ${c.collegeId}`);
  }
  const scopeKey = (scope: ReformGenerationScenario['shares'][number]['scope']) => [scope.kind, scope.kind === 'national' ? null : scope.code];
  unique(s.shares, r => key([scopeKey(r.scope), r.listId]), 'share reference');
  unique(s.abstention, r => key(scopeKey(r.scope)), 'abstention reference');
  unique(s.candidateTemplates, r => key([r.chamber, r.role, r.locationId, r.ownerId, r.position]), 'candidate slot');
  unique(s.suppliedPreferences, r => key([r.chamber, r.collegeId, r.listId]), 'supplied preference scope');
  unique(s.suppliedVotes, r => key([r.chamber, r.collegeId]), 'supplied vote scope');
  unique(s.correspondences, r => key([r.chamber, r.pastListId, r.futureListId]), 'correspondence');
  for (const r of s.correspondences) if (!s.lists[r.chamber].some(l => l.id === r.futureListId)) errors.push('Correspondence names an absent future list');
  for (const ch of ['camera', 'senato'] as const) {
    for (const past of new Set(s.correspondences.filter(r => r.chamber === ch).map(r => r.pastListId))) {
      if (!(sum(s.correspondences.filter(r => r.chamber === ch && r.pastListId === past).map(r => r.factor)) > 0)) errors.push('Correspondence split has zero total factor');
    }
  }
  for (const r of s.shares) for (const ch of ['camera', 'senato'] as const) {
    if ((r[ch] !== null || (r.common !== null && (s.renewal === 'joint' || s.renewal === ch))) && !s.lists[ch].some(l => l.id === r.listId)) errors.push(`Share refers to absent ${ch} list ${r.listId}`);
  }
  if (snapshot) {
    if (s.snapshotVersion !== snapshot.version || s.modelVersion !== snapshot.models.version) errors.push('Scenario data/model version mismatch; explicit conversion is required');
    if (!snapshot.models.preferences.profiles.some(p => p.id === s.preferenceProfileId) || !snapshot.models.chambers.profiles.some(p => p.id === s.chamberProfileId)) errors.push('Unknown model profile');
    for (const r of s.correspondences) if (!snapshot.historicalLists.some(l => l.id === r.pastListId)) errors.push(`Unknown historical list ${r.pastListId}`);
    for (const c of snapshot.seats.geography.colleges) if (!s.coverage.some(r => r.chamber === c.chamber && r.collegeId === c.id)) errors.push(`Missing college coverage ${c.id}`);
    for (const c of s.coverage) if (!snapshot.seats.geography.colleges.some(r => r.chamber === c.chamber && r.id === c.collegeId)) errors.push(`Unknown college ${c.collegeId}`);
    for (const r of s.suppliedPreferences) if (!s.coverage.some(c => c.chamber === r.chamber && c.collegeId === r.collegeId && c.listIds.includes(r.listId)) || snapshot.seats.geography.colleges.find(c => c.id === r.collegeId)?.kind === 'special-uninominal') errors.push('Unmatched supplied preference scope');
    for (const r of s.suppliedVotes) {
      const c = snapshot.seats.geography.colleges.find(c => c.chamber === r.chamber && c.id === r.collegeId);
      if (!c || (c.kind === 'special-uninominal') !== ('candidateTotals' in r.votes)) errors.push('Unmatched/incompatible supplied vote scope');
      if (c?.kind === 'taa-proportional') errors.push('Supply Camera TAA special ballots; the proportional view is derived from those same ballots');
    }
    for (const r of [...s.shares, ...s.abstention]) {
      const sc = r.scope;
      if (sc.kind !== 'national' && !snapshot.municipalities.some(m => (sc.kind === 'municipality' ? m.code : sc.kind === 'province' ? m.provinceId : m.regionId) === sc.code)) errors.push(`Unknown reference scope ${key(sc)}`);
    }
    for (const t of s.candidateTemplates) {
      const ordinary = s.coverage.find(c => c.chamber === t.chamber && c.collegeId === t.locationId);
      if (t.role === 'ordinary' && (!ordinary?.listIds.includes(t.ownerId) || t.position > 7)) errors.push(`Unmatched ordinary candidate template ${t.locationId}`);
      if (t.role === 'ordinary' && t.affiliationListId !== null && t.affiliationListId !== t.ownerId) errors.push('Ordinary template affiliation conflicts with its list');
      if (t.role === 'special') {
        const members = s.lists[t.chamber].filter(l => (l.coalitionId ?? l.id) === t.ownerId && ordinary?.listIds.includes(l.id));
        if (t.position !== 1 || snapshot.seats.geography.colleges.find(c => c.id === t.locationId)?.kind !== 'special-uninominal' || !members.length || t.affiliationListId !== null && !members.some(l => l.id === t.affiliationListId)) errors.push('Invalid special candidate template');
      }
      if (t.role === 'bonus' && (s.renewal !== 'joint' || !snapshot.seats.chambers.find(c => c.chamber === t.chamber)?.territories.some(r => r.id === t.locationId && r.bonusSeats >= t.position))) errors.push('Invalid bonus candidate template');
    }
  }
  return [...new Set(errors)];
}
export function requireReformScenario(value: unknown, snapshot?: ReformGenerationSnapshot): ReformGenerationScenario {
  const errors = validateReformScenario(value, snapshot);
  if (errors.length) throw new RangeError(`Invalid reform scenario: ${errors.join('; ')}`);
  return structuredClone(value) as ReformGenerationScenario;
}
export function serializeReformScenario(s: ReformGenerationScenario): string { return JSON.stringify(requireReformScenario(s)); }
export function parseReformScenario(text: string, snapshot?: ReformGenerationSnapshot): ReformGenerationScenario {
  return requireReformScenario(JSON.parse(text), snapshot);
}

/** Defaults are generated from the accepted snapshot, never hand-edited.
 * Ordinary filing is a future scenario assumption. Special historical ballot
 * groups stay distinct: no invented coalition-to-list decomposition. */
export function createDefaultReformScenario(snapshot: ReformGenerationSnapshot, projectionDate: string, electionDate: string, seed = 'r26-default-v1'): ReformGenerationScenario {
  const s: ReformGenerationScenario = { schemaVersion: 1, kind: 'politics-reform-scenario', algorithmId: REFORM_ALGORITHM_ID,
    snapshotVersion: snapshot.version, modelVersion: snapshot.models.version, modelIds: REFORM_MODEL_IDS,
    preferenceProfileId: 'central-v1', chamberProfileId: 'central-v1', seed, electionDate, projectionDate, mode: 'mean', renewal: 'joint',
    lists: { camera: [], senato: [] }, coverage: [], correspondences: [], shares: [], abstention: [], candidateGeneration: { reuseProbability: 0.2, maximumOrdinaryCandidacies: 5 },
    candidateTemplates: [], suppliedPreferences: [], suppliedVotes: [], bonusLinks: [], conversion: null };
  for (const ch of ['camera', 'senato'] as const) {
    const totals = new Map<string, number>();
    for (const m of snapshot.municipalities) for (const r of m[ch].votes) totals.set(r.listId, (totals.get(r.listId) ?? 0) + r.votes);
    const ordinary = snapshot.historicalLists.filter(l => l.kind === 'list').sort((a, b) => (totals.get(b.id) ?? 0) - (totals.get(a.id) ?? 0)).slice(0, 8);
    const special = snapshot.historicalLists.filter(l => l.kind === 'special-ballot-group' && (totals.get(l.id) ?? 0) > 0);
    const specialOnly = snapshot.historicalLists.filter(l => l.kind === 'list' && !ordinary.some(o => o.id === l.id) &&
      snapshot.municipalities.some(m => m.regionId === '04' && m[ch].votes.some(v => v.listId === l.id && v.votes > 0)));
    s.lists[ch] = [...ordinary, ...specialOnly, ...special].map(l => ({ id: l.id, name: l.name, coalitionId: null, protectedMinority: null }));
    // Source candidate-link connected components supply the latest historical
    // coalition metadata. Retaining these links is an explicit future default,
    // not a claim that old candidates or coalitions will stand unchanged.
    for (const historical of snapshot.historicalCoalitions.filter(c => c.chamber === ch)) {
      const members = historical.listIds.filter(id => ordinary.some(l => l.id === id)).sort();
      if (members.length < 2) continue;
      const id = `historical-coalition:${members.join(':')}`;
      s.lists[ch] = s.lists[ch].map(l => members.includes(l.id) ? { ...l, coalitionId: id } : l);
    }
    for (const l of s.lists[ch]) s.correspondences.push({ chamber: ch, pastListId: l.id, futureListId: l.id, factor: 1 });
    for (const c of snapshot.seats.geography.colleges.filter(c => c.chamber === ch)) {
      const ms = new Set(snapshot.memberships.filter(u => u.chamber === ch && u.collegeId === c.id).map(u => u.municipalityCode));
      const ids = c.kind === 'special-uninominal' ? s.lists[ch].filter(l => snapshot.municipalities.some(m => ms.has(m.code) && m[ch].votes.some(r => r.listId === l.id && r.votes > 0))).map(l => l.id)
        : [...ordinary, ...(c.kind === 'taa-proportional' ? specialOnly : [])].map(l => l.id);
      s.coverage.push({ chamber: ch, collegeId: c.id, listIds: ids });
    }
  }
  const subjects = (ch: Chamber) => [...new Map(s.lists[ch].filter(l => snapshot.historicalLists.find(r => r.id === l.id)?.kind === 'list').map(l => {
    const subject = { kind: l.coalitionId ? 'coalition' as const : 'list' as const, id: l.coalitionId ?? l.id };
    return [JSON.stringify(subject), subject];
  })).values()];
  s.bonusLinks = subjects('camera').filter(a => subjects('senato').some(b => b.id === a.id && b.kind === a.kind))
    .map(subject => ({ camera: subject, senato: subject, cameraDeclares: true, senatoDeclares: true }));
  return requireReformScenario(s, snapshot);
}

/** Explicit, opt-in conversion. The original legacy scenario and parser remain
 * unchanged. Callers must supply a reform mapping for each list and must resolve
 * old candidate templates separately (they have no recorded gender). */
export function convertLegacyScenarioToReform(legacy: Scenario, snapshot: ReformGenerationSnapshot, options: {
  sourceAlgorithmId: 'politiche-r-parity-v1'; projectionDate: string; seed: string;
  listMap: Readonly<Record<string, string>>;
}): ReformGenerationScenario {
  if (options.sourceAlgorithmId !== 'politiche-r-parity-v1' || legacy.candidateTemplates.length) throw new RangeError('Legacy conversion needs old-law identity and separately resolved candidate templates with recorded gender');
  const result = createDefaultReformScenario(snapshot, options.projectionDate, legacy.electionDate, options.seed);
  const id = (name: string) => {
    const mapped = options.listMap[name]; if (!mapped) throw new RangeError(`Missing explicit reform mapping for ${name}`); return mapped;
  };
  for (const l of legacy.lists) {
    const mapped = id(l.name);
    for (const ch of ['camera', 'senato'] as const) {
      const target = result.lists[ch].find(r => r.id === mapped);
      if (!target) throw new RangeError(`Mapped reform list is absent in ${ch}: ${mapped}`);
      result.lists[ch] = result.lists[ch].map(r => r.id === mapped ? { ...r, coalitionId: l.coalition } : r);
    }
    if (l.shareOverride) result.shares.push({ scope: { kind: 'national' }, listId: mapped, common: l.startingShare / 100, camera: null, senato: null });
  }
  for (const r of legacy.localShareOverrides) result.shares.push({ scope: { kind: r.scope, code: r.locationCode }, listId: id(r.list), common: r.startingShare / 100, camera: null, senato: null });
  if (legacy.abstentionOverride) result.abstention.push({ scope: { kind: 'national' }, common: legacy.abstentionShare / 100, camera: null, senato: null });
  // Old history spans other elections/resolutions. Reusing those correspondences
  // would silently change semantics; require callers to reconcile them explicitly.
  if (legacy.listCorrespondences.some(r => r.source === 'manual')) throw new RangeError('Manual legacy historical correspondences require explicit source reconciliation');
  const active = new Set(legacy.lists.map(l => id(l.name)));
  for (const ch of ['camera', 'senato'] as const) result.lists[ch] = result.lists[ch].filter(l => active.has(l.id));
  result.coverage = result.coverage.map(c => ({ ...c, listIds: c.listIds.filter(i => active.has(i)) }));
  result.correspondences = result.correspondences.filter(c => active.has(c.futureListId));
  const subjects = (ch: Chamber) => [...new Map(result.lists[ch].map(l => [l.coalitionId ?? l.id,
    { kind: l.coalitionId ? 'coalition' as const : 'list' as const, id: l.coalitionId ?? l.id }])).values()];
  result.bonusLinks = subjects('camera').filter(s => subjects('senato').some(t => t.id === s.id && t.kind === s.kind))
    .map(s => ({ camera: s, senato: s, cameraDeclares: true, senatoDeclares: true }));
  result.conversion = { from: 'politiche-r-parity-v1/scenario-v7', assumptions: ['Explicit user mapping; reform final attributed totals and paired mean-mode interpretation.',
    'Legacy historical input and candidate generation parameters are not calibrated reform parameters; reform defaults are recorded in this scenario.'] };
  return requireReformScenario(result, snapshot);
}

/** Snapshot changes never silently relabel data or models. The current converter
 * accepts only the explicit same-version identity conversion. Future versions
 * must add a reviewed conversion here rather than widening a type assertion. */
export function validateReformSnapshot(value: unknown): string[] {
  try {
    const shapeIssues: ValidationIssue[] = [];
    if (!snapshotShape.check(value, '$', shapeIssues)) return shapeIssues.map(i => `${i.path}: ${i.message}`);
    const s = value as ReformGenerationSnapshot;
    const errors = validateSeatTableBundle(s.seats);
    for (const ch of ['camera', 'senato'] as const) {
      const lists = s.historicalCoalitions.filter(c => c.chamber === ch).flatMap(c => c.listIds);
      if (new Set(lists).size !== lists.length || lists.some(id => !s.historicalLists.some(l => l.id === id))) errors.push('Invalid historical coalition coverage');
    }
    for (const [label, ids] of [['historical list', s.historicalLists.map(l => l.id)],
      ['preference profile', s.models.preferences.profiles.map(p => p.id)], ['chamber profile', s.models.chambers.profiles.map(p => p.id)],
      ['membership', s.memberships.map(m => JSON.stringify([m.chamber, m.municipalityCode, m.collegeId]))]] as const) {
      if (!ids.length || new Set(ids).size !== ids.length) errors.push(`Missing/duplicate ${label}`);
    }
    if (!s.sourceVersions.length || !s.models.sourceVersions.length || !s.assumptions.length || !s.models.assumptions.length) errors.push('Missing model/source assumptions');
    if (s.municipalities.length !== s.seats.geography.municipalityCount || new Set(s.municipalities.map(m => m.code)).size !== s.municipalities.length) errors.push('Incomplete/duplicate historical municipalities');
    const municipalityIndex = new Map(s.municipalities.map(m => [m.code, m]));
    const membershipPopulation = new Map<string, number>(), municipalPopulation = new Map<string, number>();
    const collegeIndex = new Map(s.seats.geography.colleges.map(c => [c.id, c]));
    for (const u of s.memberships) {
      const c = collegeIndex.get(u.collegeId), m = municipalityIndex.get(u.municipalityCode);
      const t = c && s.seats.geography.territories.find(t => t.id === c.territoryId);
      if (!c || !m || c.chamber !== u.chamber || t?.regionId !== m.regionId) errors.push(`Invalid membership ${u.collegeId}/${u.municipalityCode}`);
      membershipPopulation.set(u.collegeId, (membershipPopulation.get(u.collegeId) ?? 0) + u.population);
      if (c?.kind !== 'taa-proportional') municipalPopulation.set(`${u.chamber}:${u.municipalityCode}`, (municipalPopulation.get(`${u.chamber}:${u.municipalityCode}`) ?? 0) + u.population);
    }
    for (const c of collegeIndex.values()) if (membershipPopulation.get(c.id) !== c.population) errors.push(`Incomplete membership population ${c.id}`);
    for (const m of s.municipalities) for (const ch of ['camera', 'senato'] as const) {
      if (!/^\d{6}$/.test(m.code) || !/^\d{3}$/.test(m.provinceId) || !s.seats.geography.regions.some(r => r.id === m.regionId) ||
          !municipalPopulation.get(`${ch}:${m.code}`) || municipalPopulation.get(`camera:${m.code}`) !== municipalPopulation.get(`senato:${m.code}`)) errors.push(`Invalid municipal identity/membership ${m.code}`);
      if (!Number.isSafeInteger(m[ch].electors) || m[ch].electors <= 0 || m[ch].votes.some(v => !Number.isSafeInteger(v.votes) || v.votes < 0 || !s.historicalLists.some(l => l.id === v.listId)) ||
          sum(m[ch].votes.map(v => v.votes)) > m[ch].electors || new Set(m[ch].votes.map(v => v.listId)).size !== m[ch].votes.length) errors.push(`Invalid ${ch} history ${m.code}`);
    }
    if (Object.values(s.models.preferences.baseline).some(n => !Number.isFinite(n) || n < 0) || s.models.preferences.baseline.intensity > 3) errors.push('Invalid preference parameters');
    for (const p of s.models.chambers.profiles) if (Math.abs(p.residualCorrelation) > 1 || p.sharedVariance < 0 || p.sharedVariance > 1 || !Number.isFinite(p.gapMultiplier)) errors.push('Invalid covariance profile');
    for (const p of s.models.preferences.profiles) if (p.persistence < 0 || p.persistence > 1 || [p.intensityMultiplier, p.concentrationMultiplier, p.tripleWeight].some(n => !Number.isFinite(n) || n < 0)) errors.push('Invalid preference profile');
    return errors;
  } catch { return ['Malformed reform snapshot']; }
}

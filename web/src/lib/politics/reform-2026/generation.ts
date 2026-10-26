import { REFORM_ALGORITHM_ID, type Chamber, type ChamberInput, type OrdinaryVotes, type Provenance, type ReformScrutinyInput, type SpecialVotes } from './input';
import type { GenerationTrace, ReformGenerationScenario, ReformGenerationSnapshot } from './generation-types';
import { REFORM_MODEL_IDS } from './generation-types';
import { GENERATION_STREAM_VERSION, apportion, generationRng, sum } from './generation-math';
import { generatePairedMunicipalVotes, preparePairedVoteReferences, renewedChamberIds, type GeneratedMunicipalVotes } from './paired-vote-generation';
import { generateReformCandidatures } from './reform-candidate-generation';
import { generateSyntheticPreferences, preferenceParametersForRegion } from './preference-generation';
import { requireReformScenario, validateReformSnapshot } from './generation-scenario';
import { validateReformScrutinyInput } from './validation';
import { prepareReferenceBonusPlan, type PreparedBonusPlan } from './reference-bonus-plan';

interface CollegeCounts { electors: number; votes: Map<string, number> }
/** Controlled integer reconstruction of historical municipal support to the
 * reform geography. Census weights are an estimate of geographic voting shares,
 * not observed submunicipal turnout. Preserve municipal votes and elector totals. */
export function aggregateReformCollegeVotes(s: ReformGenerationScenario, snapshot: ReformGenerationSnapshot, generated: readonly GeneratedMunicipalVotes[], simulationId: number): Map<string, CollegeCounts> {
  const result = new Map<string, CollegeCounts>();
  const colleges = new Map(snapshot.seats.geography.colleges.map(c => [c.id, c]));
  const filing = new Map(s.coverage.map(c => [c.collegeId, new Set(c.listIds)]));
  const memberships = new Map<string, typeof snapshot.memberships[number][]>();
  for (const m of snapshot.memberships) if (colleges.get(m.collegeId)?.kind !== 'taa-proportional') {
    const k = `${m.chamber}:${m.municipalityCode}`, entries = memberships.get(k) ?? [];
    entries.push(m); memberships.set(k, entries);
  }
  for (const row of generated) {
    const units = new Map<string, number>();
    for (const m of memberships.get(`${row.chamber}:${row.municipalityCode}`) ?? []) units.set(m.collegeId, (units.get(m.collegeId) ?? 0) + m.population);
    const ids = [...units.keys()].sort(), weights = ids.map(id => units.get(id)!);
    if (!ids.length) throw new RangeError(`Missing ${row.chamber} municipality membership ${row.municipalityCode}`);
    const remaining = apportion(row.electors, weights, generationRng(s.seed, simulationId, 'geography-electors', row.chamber, row.municipalityCode));
    ids.forEach((id, i) => { const r = result.get(id) ?? { electors: 0, votes: new Map() }; r.electors += remaining[i]; result.set(id, r); });
    for (const vote of row.votes) {
      const eligible = ids.map(id => !!filing.get(id)?.has(vote.listId));
      let pending = vote.votes;
      const allocated = ids.map(() => 0);
      while (pending > 0) {
        const support = weights.map((w, i) => eligible[i] && remaining[i] ? w : 0);
        if (!sum(support)) throw new RangeError(`Municipal votes conflict with college filing/capacity ${row.municipalityCode}/${vote.listId}`);
        const proposal = apportion(pending, support, generationRng(s.seed, simulationId, 'geography-votes', row.chamber, row.municipalityCode, vote.listId, pending));
        const accepted = proposal.map((n, i) => Math.min(n, remaining[i]));
        accepted.forEach((n, i) => { remaining[i] -= n; allocated[i] += n; }); pending -= sum(accepted);
      }
      ids.forEach((id, i) => { const r = result.get(id)!; r.votes.set(vote.listId, (r.votes.get(vote.listId) ?? 0) + allocated[i]); });
    }
  }
  // Camera TAA is one ballot set viewed both at special and proportional level.
  for (const c of snapshot.seats.geography.colleges.filter(c => c.kind === 'taa-proportional')) {
    const physical = snapshot.seats.geography.colleges.filter(p => p.chamber === c.chamber && p.territoryId === c.territoryId && p.kind === 'special-uninominal');
    const r: CollegeCounts = { electors: 0, votes: new Map() };
    for (const p of physical) { const source = result.get(p.id); if (!source) continue; r.electors += source.electors; for (const [id, n] of source.votes) r.votes.set(id, (r.votes.get(id) ?? 0) + n); }
    result.set(c.id, r);
  }
  return result;
}

export interface PreparedReformGeneration {
  scenario: ReformGenerationScenario;
  snapshot: ReformGenerationSnapshot;
  references: ReturnType<typeof preparePairedVoteReferences>;
  bonusPlan?: PreparedBonusPlan;
}
export function prepareReformGeneration(value: unknown, snapshot: ReformGenerationSnapshot): PreparedReformGeneration {
  const errors = validateReformSnapshot(snapshot);
  if (errors.length) throw new RangeError(`Invalid reform snapshot: ${errors.join('; ')}`);
  const scenario = requireReformScenario(value, snapshot), owned = structuredClone(snapshot);
  const references = preparePairedVoteReferences(scenario, owned);
  const bonusPlan = scenario.candidateGeneration.bonusPolicy ? prepareReferenceBonusPlan(scenario, owned, references.units) : undefined;
  if (bonusPlan) {
    references.trace.bonusSelection = { policy: bonusPlan.policy, referenceDate: bonusPlan.referenceDate,
      slates: bonusPlan.slates.map(({ chamber, territoryId, subject, allocations }) => ({ chamber, territoryId, subject, allocations })) };
    if (bonusPlan.slates.some(b => b.allocations.some(a => a.slots !== a.referenceSlots))) references.trace.warnings.push({
      code: 'BONUS_SELECTION_CONSTRAINTS', message: 'Composizione del premio adattata ai candidati forniti, ai prefissi disponibili o ai vincoli di genere; le quote di riferimento restano distinte dalle candidature.' });
  }
  return { scenario, snapshot: owned, references, ...(bonusPlan ? { bonusPlan } : {}) };
}
export function generateReformSimulation(prepared: PreparedReformGeneration, simulationId: number): { input: ReformScrutinyInput; trace: GenerationTrace } {
  if (!Number.isSafeInteger(simulationId) || simulationId < 1) throw new RangeError('Expected a positive simulation ID');
  const { scenario: s, snapshot } = prepared, trace = structuredClone(prepared.references.trace);
  const votes = generatePairedMunicipalVotes(s, snapshot, prepared.references.units, simulationId);
  const counts = aggregateReformCollegeVotes(s, snapshot, votes, simulationId);
  const candidatures = generateReformCandidatures(s, snapshot, simulationId, prepared.bonusPlan);
  const provenance = (modelId: string, parameterVersion: string, profileId: string): Provenance => ({ kind: 'generated', modelId, parameterVersion,
    profileId, sourceVersions: [...snapshot.sourceVersions, snapshot.version, snapshot.models.version, ...(s.pollReferences ? [s.pollReferences.archiveVersion, s.pollReferences.version] : [])], seed: s.seed,
    assumptions: [...snapshot.assumptions, ...snapshot.models.assumptions, ...(s.conversion?.assumptions ?? []), ...(s.pollReferences?.assumptions ?? [])] });
  const voteProvenance = provenance(REFORM_MODEL_IDS.votes, snapshot.models.chambers.parameterVersion, s.chamberProfileId);
  const preferenceProvenance = provenance(REFORM_MODEL_IDS.preferences, snapshot.models.preferences.parameterVersion, s.preferenceProfileId);
  const chambers = renewedChamberIds(s).map((ch): ChamberInput => {
    const geo = snapshot.seats.chambers.find(c => c.chamber === ch)!, candidates = candidatures.chambers.get(ch)!;
    const ordinaryVotes: ChamberInput['ordinaryVotes'][number][] = [], specialVotes: ChamberInput['specialVotes'][number][] = [];
    for (const college of geo.colleges) {
      const c = counts.get(college.id);
      if (!c) throw new RangeError(`Missing generated college ${college.id}`);
      const coverage = s.coverage.find(r => r.chamber === ch && r.collegeId === college.id)!;
      const listVotes = coverage.listIds.map(listId => ({ listId, votes: c.votes.get(listId) ?? 0 }));
      const supplied = s.suppliedVotes.find(r => r.chamber === ch && r.collegeId === college.id);
      const electorate = { minimumAge: 18 as const, electors: c.electors };
      if (college.kind === 'special-uninominal') {
        if (supplied && !('candidateTotals' in supplied.votes)) throw new RangeError('Special vote override requires candidate totals');
        const v: SpecialVotes = supplied ? structuredClone(supplied.votes) as SpecialVotes : { kind: 'attributed', listVotes, provenance: voteProvenance,
          candidateTotals: candidates.specialCandidatures.filter(c => c.collegeId === college.id).map(candidate => ({ candidateId: candidate.candidateId,
            votes: sum(listVotes.filter(l => candidate.linkedListIds.includes(l.listId)).map(l => l.votes)) })) };
        specialVotes.push({ collegeId: college.id, electorate, votes: v });
      } else {
        if (supplied && 'candidateTotals' in supplied.votes) throw new RangeError('Ordinary vote override cannot contain special candidate totals');
        ordinaryVotes.push({ collegeId: college.id, electorate, votes: supplied ? structuredClone(supplied.votes) as OrdinaryVotes : { kind: 'attributed', listVotes, provenance: voteProvenance } });
      }
    }
    // Resolve the overlapping TAA view from the actual special override too.
    for (const college of geo.colleges.filter(c => c.kind === 'taa-proportional')) {
      if (s.suppliedVotes.some(r => r.chamber === ch && r.collegeId === college.id)) continue;
      const r = ordinaryVotes.find(r => r.collegeId === college.id)!;
      const physical = specialVotes.filter(r => geo.colleges.find(c => c.id === r.collegeId)?.territoryId === college.territoryId);
      const listVotes = candidates.ordinarySlates.filter(s => s.collegeId === college.id).map(slate => ({ listId: slate.listId,
        votes: sum(physical.flatMap(p => (p.votes.kind === 'components' ? p.votes.directListVotes : p.votes.listVotes).filter(l => l.listId === slate.listId).map(l => l.votes))) }));
      ordinaryVotes[ordinaryVotes.indexOf(r)] = { ...r, votes: { kind: 'attributed', listVotes, provenance: voteProvenance } };
    }
    const people = new Map(candidatures.candidates.map(p => [p.id, p]));
    const ordinarySlates = candidates.ordinarySlates.map(slate => {
      const row = ordinaryVotes.find(r => r.collegeId === slate.collegeId)!;
      const budget = (row.votes.kind === 'components' ? row.votes.directListVotes : row.votes.listVotes).find(r => r.listId === slate.listId)?.votes;
      if (budget === undefined) throw new RangeError('Preference generation has no explicit list ballot budget');
      const supplied = s.suppliedPreferences.find(p => p.chamber === ch && p.collegeId === slate.collegeId && p.listId === slate.listId);
      const territory = geo.territories.find(t => t.id === geo.colleges.find(c => c.id === slate.collegeId)!.territoryId)!;
      const parameters = preferenceParametersForRegion(snapshot.models, territory.regionId);
      return { ...slate, preferences: generateSyntheticPreferences({ seed: s.seed, simulationId, chamber: ch,
        collegeId: slate.collegeId, listId: slate.listId, eligible: slate.candidateIds.slice(1).map(id => people.get(id)!), ballotBudget: budget,
        parameters, profile: snapshot.models.preferences.profiles.find(p => p.id === s.preferenceProfileId)!,
        provenance: preferenceProvenance, supplied: supplied?.preferences }) };
    });
    return { ...geo, ...candidates, ordinarySlates, ordinaryVotes, specialVotes };
  });
  const camera = chambers.find(c => c.chamber === 'camera'), senato = chambers.find(c => c.chamber === 'senato');
  const input: ReformScrutinyInput = { schemaVersion: 1, algorithmId: REFORM_ALGORITHM_ID, simulationId, electionDate: s.electionDate,
    legalVersion: 'legge-180-2026', geographyVersion: snapshot.seats.geography.version,
    randomness: { seed: s.seed, streamVersion: `${GENERATION_STREAM_VERSION}/legal-v1` }, candidates: candidatures.candidates,
    renewal: s.renewal === 'joint' ? { kind: 'joint', camera: camera!, senato: senato! } : { kind: 'single', chamber: chambers[0] },
    bonusLinks: s.renewal === 'joint' ? s.bonusLinks : [], chamberModel: { kind: 'generated', modelId: REFORM_MODEL_IDS.chambers,
      parameterVersion: snapshot.models.chambers.parameterVersion, covarianceProfileVersion: s.chamberProfileId,
      sourceVersions: [...snapshot.sourceVersions, snapshot.version, snapshot.models.version, ...(s.pollReferences ? [s.pollReferences.archiveVersion, s.pollReferences.version] : [])], seed: s.seed, mode: 'mean',
      assumptions: [...snapshot.models.assumptions, ...snapshot.assumptions, ...(s.pollReferences?.assumptions ?? []),
        ...(prepared.bonusPlan ? [`Bonus candidatures: ${prepared.bonusPlan.policy}, central support at ${prepared.bonusPlan.referenceDate}; constrained candidate allocations are not legal coalition-member entitlements.`] : [])], references: trace.references } };
  const checked = validateReformScrutinyInput(input);
  if (!checked.valid) throw new RangeError(`Generated reform input is invalid: ${checked.issues.map(i => `${i.path}: ${i.message}`).join('; ')}`);
  return { input: checked.input, trace };
}

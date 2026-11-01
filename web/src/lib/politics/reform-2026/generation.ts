import { assembleReformSimulation } from './assembly';
import { REFORM_ALGORITHM_ID, type Chamber, type ChamberInput, type OrdinaryVotes, type Provenance, type ReformScrutinyInput, type SpecialVotes } from './input';
import type { GenerationTrace, ReformGenerationScenario, ReformGenerationSnapshot, ReformAssemblyContext, ReformLegalSettings } from './generation-types';
import { REFORM_MODEL_IDS } from './generation-types';
import { GENERATION_STREAM_VERSION, apportion, generationRng, sum } from './generation-math';
import { generatePairedMunicipalVotes, preparePairedVoteReferences, renewedChamberIds, type GeneratedMunicipalVotes } from './paired-vote-generation';
import { generateReformCandidatures } from './reform-candidate-generation';
import { generateSyntheticPreferences, preferenceParametersForRegion } from './preference-generation';
import { requireReformScenario, validateReformSnapshot } from './generation-scenario';
import { validateReformScrutinyInput } from './validation';
import { prepareReferenceBonusPlan, type PreparedBonusPlan } from './reference-bonus-plan';

export interface CollegeCounts { electors: number; votes: Map<string, number> }
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
  return assembleReformSimulation(prepared, simulationId, counts, trace);
}
export { assembleReformSimulation } from './assembly';

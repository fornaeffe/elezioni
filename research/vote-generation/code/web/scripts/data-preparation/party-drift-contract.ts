/** Research-only membership contract. Source counts are never decomposed. */
import { createHash } from 'node:crypto';
import { componentResolution } from './party-drift-identifiability';

export const PARTY_DRIFT_SCHEMA = 'party-drift-research-v2' as const;
export interface PartyComponent {
  id: string; label: string; supportStart: string;
  origin: 'historical' | 'retrospective-split' | 'new';
}
export interface PartyBallot {
  id: string; date: string; kind: 'camera' | 'european';
  geography: string; available: string[];
  groups: { id: string; members: string[] }[];
}
export interface PartyCountObservation {
  sourceId: string; ballotId: string; municipalityId: string;
  electors: number; nonValid: number; counts: { groupId: string; count: number }[];
}
export interface PartyResearchContract {
  schema: typeof PARTY_DRIFT_SCHEMA; referenceDate: string; cutoff: string;
  components: PartyComponent[]; ballots: PartyBallot[]; counts: PartyCountObservation[];
  sourceVersions: string[];
  /** Explicit scenario information, including quantity/units in fingerprints.
   * This is not a fake survey or a claim that all posterior paths exist yet. */
  referenceInformation?: { id: string; date: string; availableOn: string; scope: string; members: string[];
    meanValidFraction: number; sdPercentagePoints: number }[];
  /** Explicit inputs to inference, not just a human-readable model name. */
  priorPolicy: unknown; fittingPolicy: unknown;
}
export function checkedDate(date: string) {
  const t = Date.parse(date);
  if (!/^\d{4}-\d{2}-\d{2}$/.test(date) || !Number.isFinite(t) || new Date(t).toISOString().slice(0, 10) !== date)
    throw new Error('Invalid model date: ' + date);
  return t / (365.25 * 86400000);
}
function unique(ids: string[], name: string) {
  if (ids.some(id => !id) || new Set(ids).size !== ids.length) throw new Error('Duplicate/empty ' + name);
}
export function validatePartyContract(contract: PartyResearchContract, options: { allowUnresolvedNegativeControl?: boolean } = {}) {
  if (contract.schema !== PARTY_DRIFT_SCHEMA) throw new Error('Unsupported party drift schema');
  checkedDate(contract.referenceDate); checkedDate(contract.cutoff);
  if (contract.referenceDate > contract.cutoff) throw new Error('Reference date after evidence cutoff');
  if (!contract.components.length) throw new Error('Empty political basis');
  unique(contract.components.map(c => c.id), 'component identity');
  unique(contract.ballots.map(b => b.id), 'ballot identity');
  unique(contract.counts.map(c => c.sourceId), 'source observation');
  const components = new Map(contract.components.map(c => [c.id, c]));
  for (const c of contract.components) {
    checkedDate(c.supportStart);
    if (c.id === '0') throw new Error('Non-valid is not a political component');
    if (c.origin === 'new') throw new Error('New-party reference constraints are not implemented in this research gate');
    // Future births require a separate initialization contract; never extrapolate them.
    if (c.supportStart > contract.referenceDate) throw new Error('Future-birth initialization is not implemented in this research gate');
  }
  const ballots = new Map(contract.ballots.map(b => [b.id, b]));
  for (const b of contract.ballots) {
    checkedDate(b.date); unique(b.available, 'available component');
    if (!b.available.length) throw new Error('Empty political choice set');
    unique(b.groups.map(g => g.id), 'group identity');
    const members = b.groups.flatMap(g => g.members);
    if (b.groups.some(g => !g.members.length)) throw new Error('Empty list membership');
    unique(members, 'membership: a component must belong to exactly one list');
    if (members.length !== b.available.length || members.some(id => !b.available.includes(id))) throw new Error('Memberships do not partition availability');
    for (const id of b.available) {
      const c = components.get(id);
      if (!c || c.supportStart > b.date) throw new Error('Unknown or pre-birth component: ' + id);
    }
  }
  const cells = new Set<string>();
  for (const o of contract.counts) {
    const b = ballots.get(o.ballotId);
    if (!b || b.date > contract.cutoff) throw new Error('Unknown or cutoff-unavailable count ballot');
    const cell = JSON.stringify([o.ballotId, o.municipalityId]);
    if (cells.has(cell)) throw new Error('Repeated municipality/ballot observation');
    cells.add(cell);
    unique(o.counts.map(c => c.groupId), 'observed group');
    if (o.counts.length !== b.groups.length || o.counts.some(c => !b.groups.some(g => g.id === c.groupId))) throw new Error('Incomplete original counts');
    if (![o.electors, o.nonValid, ...o.counts.map(c => c.count)].every(x => Number.isSafeInteger(x) && x >= 0) || o.electors === 0)
      throw new Error('Counts must be nonnegative safe integers with positive electors');
    if (o.nonValid + o.counts.reduce((s, c) => s + c.count, 0) !== o.electors) throw new Error('Elector conservation failure');
  }
  const references = contract.referenceInformation ?? [];
  unique(references.map(r => r.id), 'reference identity');
  for (const r of references) {
    checkedDate(r.date); checkedDate(r.availableOn); unique(r.members, 'reference membership');
    if (r.availableOn > contract.cutoff) throw new Error('Reference information unavailable at cutoff');
    if (!r.scope || !r.members.length || r.members.some(id => !components.has(id)) || !(r.meanValidFraction > 0 && r.meanValidFraction < 1) ||
      !Number.isFinite(r.sdPercentagePoints) || r.sdPercentagePoints < 0 || (r.sdPercentagePoints / 100) ** 2 >= r.meanValidFraction * (1 - r.meanValidFraction))
      throw new Error('Invalid reference information/units');
  }
  const information = contract.counts.map(o => ({ id: o.sourceId, groups: ballots.get(o.ballotId)!.groups.map(g => g.members) }));
  information.push(...references.map(r => ({ id: r.id, groups: [r.members] })));
  const resolution = componentResolution(contract.components.map(c => c.id), information);
  if (!resolution.resolved && !options.allowUnresolvedNegativeControl)
    throw new Error('Unresolved political components: combine them into an atomic identity or supply independent distinguishing information. ' + JSON.stringify(resolution.nullDirections));
}
/** Other is a complement of existing/relevant components, never outcome 0. */
export function otherMembership(existing: string[], named: string[][], otherShare: number) {
  unique(existing, 'existing component');
  const offered = named.flat(); unique(offered, 'poll answer membership');
  if (offered.some(id => !existing.includes(id))) throw new Error('Poll includes unknown/pre-birth component');
  if (!Number.isFinite(otherShare) || otherShare < 0 || otherShare > 1) throw new Error('Invalid Other share');
  const other = existing.filter(id => !offered.includes(id));
  if (otherShare > 0 && !other.length) throw new Error('Positive Other has empty political complement');
  return other;
}
function canonical(value: unknown): unknown {
  if (Array.isArray(value)) return value.map(canonical);
  if (value !== null && typeof value === 'object') return Object.fromEntries(Object.entries(value).sort(([a], [b]) => a.localeCompare(b)).map(([k, v]) => [k, canonical(v)]));
  return value;
}
export function partyFitFingerprint(contract: PartyResearchContract) {
  validatePartyContract(contract);
  // Labels do not define identity; evidence, dates, priors and effective memberships do.
  const semantic = { ...contract, components: contract.components.map(({ label: _label, ...c }) => c) };
  return createHash('sha256').update(JSON.stringify(canonical(semantic))).digest('hex');
}

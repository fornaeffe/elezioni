/** Pure form edits. All declarations, including inactive ones, pass the common grammar. */
import { parsePoliticsScenario, locationKey, type DomainContext } from '../scenario/validation';
import { withReferenceDefault } from '../scenario/references';
import { effectiveFilings, effectivePlacements, sameSubject } from '../scenario/effective';
import { activeGenerator } from '../scenario/registry';
import { REFORM, type PoliticsScenario, type Reference, type NonValidReference, type ReferenceScope,
  type Placement, type Candidate, type Subject } from '../scenario/schema';

export type Edit = (scenario: PoliticsScenario) => PoliticsScenario;
export type ApplyEdit = (edit: Edit) => boolean;
export type Mutable<T> = T extends object ? { -readonly [K in keyof T]: Mutable<T[K]> } : T;
export function editScenario(s: PoliticsScenario, change: (draft: Mutable<PoliticsScenario>) => void, context?: DomainContext) {
  const next = structuredClone(s) as Mutable<PoliticsScenario>;
  change(next);
  return parsePoliticsScenario(next, context);
}
export const scopeKey = (scope: ReferenceScope) => scope.kind === 'national' ? 'national' : `${scope.kind}:${scope.namespace}:${scope.code}`;
export type ReferenceTarget = Reference['subject'] | null;
const sameReferenceTarget = (a: NonNullable<ReferenceTarget>, b: NonNullable<ReferenceTarget>) => a.kind === b.kind && a.id === b.id;
export function referenceRows(s: PoliticsScenario, target: ReferenceTarget): readonly (Reference | NonValidReference)[] {
  return target ? s.references.filter(r => sameReferenceTarget(r.subject, target)) : s.nonValidReferences;
}
export function setReference(s: PoliticsScenario, target: ReferenceTarget, scope: ReferenceScope,
  channel: Reference['channel'], percent: string, sd?: string) {
  return editScenario(s, n => {
    const rows = target ? n.references : n.nonValidReferences;
    const sameAddress = (r: Reference | NonValidReference) => scopeKey(r.scope) === scopeKey(scope) && r.channel === channel &&
      (!target || 'subject' in r && sameReferenceTarget(r.subject, target));
    const model = activeGenerator(s).id;
    const matches = (r: Reference | NonValidReference) => sameAddress(r) && r.appliesTo.includes(model);
    const old = rows.find(matches);
    if (percent.trim() === '') {
      if (target) n.references = n.references.filter(r => !matches(r));
      else n.nonValidReferences = n.nonValidReferences.filter(r => !matches(r));
      return;
    }
    const row = withReferenceDefault({ id: old?.id ?? `reference:${target ? `${target.kind}:${target.id}` : 'non-valid'}:${scopeKey(scope)}:${channel}:${model}`, scope, channel,
      mean: Number(percent) / 100, appliesTo: old?.appliesTo ?? (rows.some(sameAddress) ? [model] : ['M01', 'M02']), uncertainty: sd === undefined ? old?.uncertainty :
        sd.trim() === '' ? null : { sdPercentagePoints: Number(sd), origin: 'explicit', policyVersion: null },
      ...(target ? { subject: target, denominator: 'valid-votes' as const } : { denominator: 'electors' as const }) });
    if (target) n.references = [...n.references.filter(r => !matches(r)), row as Reference] as Mutable<Reference[]>;
    else n.nonValidReferences = [...n.nonValidReferences.filter(r => !matches(r)), row as NonValidReference] as Mutable<NonValidReference[]>;
  });
}
export function removeReferences(s: PoliticsScenario, target: ReferenceTarget, scopes: readonly ReferenceScope[]) {
  const keys = new Set(scopes.map(scopeKey));
  return editScenario(s, n => {
    if (target) n.references = n.references.filter(r => !(sameReferenceTarget(r.subject, target) && keys.has(scopeKey(r.scope))));
    else n.nonValidReferences = n.nonValidReferences.filter(r => !keys.has(scopeKey(r.scope)));
  });
}
/** Candidate options explain incompatibilities before the user pins a slot. Adapter validation remains authoritative. */
export type CandidateContext = DomainContext & { readonly locationKinds?: Readonly<Record<string, string>> };
export function candidateReason(s: PoliticsScenario, candidate: Candidate, slot: Placement, context: CandidateContext): string | null {
  if (candidate.availability.kind === 'unavailable') return `Non disponibile: ${candidate.availability.reason}`;
  const age = Number(s.electionDate.slice(0, 4)) - Number(candidate.birthDate.slice(0, 4)) -
    (s.electionDate.slice(5) < candidate.birthDate.slice(5) ? 1 : 0);
  if (age < (slot.location.chamber === 'senato' ? 40 : 25)) return 'Età insufficiente per la camera';
  const law = s.selection.scrutiny.id;
  const pins = effectivePlacements(s, law, context.geographyRelations).filter(p => p.id !== slot.id && p.candidateId === candidate.id);
  if (pins.some(p => p.location.chamber !== slot.location.chamber) || s.lawSettings[law].externalCandidacies.some(p =>
    p.candidateId === candidate.id && (p.kind === 'foreign' || p.chamber !== slot.location.chamber))) return 'Candidatura incompatibile in altra camera o all’estero';
  if (pins.some(p => p.role === slot.role && locationKey(p.location) === locationKey(slot.location))) return 'Già presente in questa lista di candidati';
  const allPins = effectivePlacements(s, law, context.geographyRelations).filter(p => p.id !== slot.id);
  if ((slot.role === 'ordinary' || slot.role === 'plurinominal') && allPins.some(p => p.role === slot.role &&
    sameSubject(p.owner, slot.owner) && locationKey(p.location) === locationKey(slot.location) && Math.abs(p.position - slot.position) === 1 &&
    s.candidates.find(c => c.id === p.candidateId)?.gender === candidate.gender)) return 'I candidati adiacenti devono alternare il genere';
  if (slot.role === 'bonus') {
    if (pins.some(p => p.role === 'bonus' && (!sameSubject(p.owner, slot.owner) || locationKey(p.location) !== locationKey(slot.location)))) return 'Già in un altro listino';
    if (!pins.some(p => p.role === 'ordinary' && p.owner.kind === 'list' &&
      (slot.owner.kind === 'list' ? p.owner.id === slot.owner.id : s.subjects.lists.some(l => l.id === p.owner.id && l.coalitionId === slot.owner.id)) &&
      context.geographyRelations.some(r => locationKey(r.parent) === locationKey(slot.location) && locationKey(r.child) === locationKey(p.location)) &&
      effectiveFilings(s, law, p.owner.id).some(f => locationKey(f) === locationKey(p.location)))) return 'Richiede una candidatura ordinaria eleggibile nella circoscrizione';
  }
  if (slot.role === 'uninominal' && pins.some(p => p.role === 'uninominal')) return 'Già candidato in un collegio uninominale';
  if (slot.role === 'special' && pins.some(p => p.role === 'special')) return 'Già candidato in un collegio speciale';
  if ((slot.role === 'bonus' && pins.some(p => p.role === 'special' || context.locationKinds?.[locationKey(p.location)] === 'taa-proportional')) ||
    (slot.role === 'special' || context.locationKinds?.[locationKey(slot.location)] === 'taa-proportional') && pins.some(p => p.role === 'bonus')) return 'Candidature speciali e premio incompatibili';
  const ordinary = pins.filter(p => p.role === 'ordinary' || p.role === 'plurinominal');
  if ((slot.role === 'ordinary' || slot.role === 'plurinominal') && ordinary.length >=
    (law === REFORM ? s.lawSettings[REFORM].candidateGeneration.maximumOrdinaryCandidacies : 5)) return 'Numero massimo di candidature raggiunto';
  if (pins.some(p => p.owner.kind === 'list' && slot.owner.kind === 'list' && p.owner.id !== slot.owner.id)) return 'Candidato in un’altra lista';
  return null;
}
export const subjectName = (s: PoliticsScenario, subject: Subject) =>
  (subject.kind === 'coalition' ? s.subjects.coalitions : s.subjects.lists).find(x => x.id === subject.id)?.name ?? subject.id;

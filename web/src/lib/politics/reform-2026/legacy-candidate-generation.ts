import type { BonusSlate, Candidate, Chamber, ChamberInput, OrdinarySlate, SpecialCandidature } from './input';
import type { ReformGenerationScenario, ReformGenerationSnapshot } from './generation-types';
import { generationRng } from './generation-math';

// Original candidature policy, retained for exact replay of scenarios without
// candidateGeneration.bonusPolicy. New selection work belongs in the new policy.

export interface GeneratedCandidatures {
  candidates: Candidate[];
  chambers: Map<Chamber, Pick<ChamberInput, 'ordinarySlates' | 'bonusSlates' | 'specialCandidatures' | 'coalitions' | 'lists'>>;
}
const key = (...s: (string | number | boolean)[]) => JSON.stringify(s);
type PlannedSlate = { collegeId: string; territoryId: string; listId: string; prefix: number; startsFemale: boolean };

/** C18-bis/S9: seven alternated positions. C19: at most five ordinary
 * candidatures, one bonus slate, consistent badges and the required ordinary
 * prefix. Special candidates are generated separately, excluding incompatible
 * bonus uses. Filed affiliations describe persons, never proportional bonus
 * entitlements to coalition members. Final legal validation checks all uses. */
export function generateLegacyReformCandidatures(s: ReformGenerationScenario, snapshot: ReformGenerationSnapshot, simulationId: number): GeneratedCandidatures {
  const voteLists = s.lists;
  s = { ...s, coverage: s.coverage.map(c => ({ ...c, listIds: c.listIds.filter(id => !voteLists[c.chamber].some(l => l.id === id && l.kind === 'other')) })) };
  const people = new Map<string, Candidate>(), ordinaryUses = new Map<string, number>();
  const chambers: GeneratedCandidatures['chambers'] = new Map();
  function own(person: Candidate) {
    const old = people.get(person.id);
    const identity = (p: Candidate) => JSON.stringify([p.id, p.name, p.birthDate, p.gender, p.availability.kind,
      p.availability.kind === 'unavailable' ? p.availability.reason : null,
      p.externalCandidacies.map(c => [c.chamber, c.kind]).sort()]);
    if (old && identity(old) !== identity(person)) throw new RangeError(`Conflicting candidate identity ${person.id}`);
    people.set(person.id, structuredClone(person)); return person.id;
  }
  function synthetic(ch: Chamber, role: string, place: string, owner: string, position: number, female: boolean): Candidate {
    const id = `synthetic:${simulationId}:${ch}:${role}:${place}:${owner}:${position}`;
    return { id, name: `Candidato sintetico ${id}`, birthDate: `${Number(s.electionDate.slice(0, 4)) - 50}-01-01`, gender: female ? 'female' : 'male', availability: { kind: 'available' }, externalCandidacies: [] };
  }
  for (const ch of s.renewal === 'joint' ? ['camera', 'senato'] as const : [s.renewal]) {
    const geo = snapshot.seats.chambers.find(c => c.chamber === ch)!;
    const lists = s.lists[ch];
    const subjectFor = (id: string) => {
      const l = lists.find(l => l.id === id)!;
      return { kind: l.coalitionId ? 'coalition' as const : 'list' as const, id: l.coalitionId ?? id };
    };
    const coalitions = [...new Set(lists.map(l => l.coalitionId).filter((id): id is string => id !== null))].sort()
      .map(id => ({ id, listIds: lists.filter(l => l.coalitionId === id).map(l => l.id) }));
    const plans: PlannedSlate[] = [];
    for (const [i, c] of geo.colleges.filter(c => c.kind !== 'special-uninominal').entries()) {
      const coverage = s.coverage.find(r => r.chamber === ch && r.collegeId === c.id)!;
      coverage.listIds.slice().sort().forEach((id, j) => plans.push({ collegeId: c.id, territoryId: c.territoryId, listId: id, prefix: 0, startsFemale: (i + j) % 2 === 0 }));
    }
    const plannedBonus: { territoryId: string; subject: ReturnType<typeof subjectFor>; slots: { slate: PlannedSlate; index: number }[] }[] = [];
    const bonusGenderBalance = new Map<string, number>();
    if (s.renewal === 'joint') for (const territory of geo.territories.filter(t => t.bonusSeats > 0)) {
      const local = plans.filter(p => p.territoryId === territory.id);
      for (const subject of [...new Map(local.map(p => { const sub = subjectFor(p.listId); return [key(sub.kind, sub.id), sub]; })).values()]) {
        const slates = local.filter(p => JSON.stringify(subjectFor(p.listId)) === JSON.stringify(subject));
        const slots: { slate: PlannedSlate; index: number }[] = [];
        for (let level = 0; level < 7 && slots.length < territory.bonusSeats; level++) for (const slate of slates) {
          if (slots.length === territory.bonusSeats) break;
          slots.push({ slate, index: level }); slate.prefix++;
        }
        if (slots.length !== territory.bonusSeats) throw new RangeError(`Insufficient ordinary placements for bonus slate ${territory.id}/${subject.id}`);
        for (const slate of slates) if (slate.prefix % 2) {
          const k = key(subject.kind, subject.id), balance = bonusGenderBalance.get(k) ?? 0;
          slate.startsFemale = balance <= 0; bonusGenderBalance.set(k, balance + (slate.startsFemale ? 1 : -1));
        }
        plannedBonus.push({ territoryId: territory.id, subject, slots });
      }
    }
    const pinned = new Map<string, Candidate>();
    for (const t of s.candidateTemplates.filter(t => t.chamber === ch && t.role === 'ordinary')) pinned.set(key(t.locationId, t.ownerId, t.position - 1), t.candidate);
    for (const t of s.candidateTemplates.filter(t => t.chamber === ch && t.role === 'bonus')) {
      const bonus = plannedBonus.find(b => b.territoryId === t.locationId && b.subject.id === t.ownerId), slot = bonus?.slots[t.position - 1];
      if (!slot) throw new RangeError(`Unmatched bonus candidate template ${t.locationId}/${t.ownerId}`);
      if (t.affiliationListId !== null && t.affiliationListId !== slot.slate.listId) throw new RangeError('Bonus template affiliation conflicts with its filed ordinary prefix');
      const k = key(slot.slate.collegeId, slot.slate.listId, slot.index), old = pinned.get(k);
      if (old && old.id !== t.candidate.id) throw new RangeError('Conflicting ordinary and bonus template placement');
      pinned.set(k, t.candidate);
    }
    for (const p of plans) {
      const constraints = Array.from({ length: 7 }, (_, i) => ({ i, person: pinned.get(key(p.collegeId, p.listId, i)) })).filter(x => x.person !== undefined);
      const starts = constraints.map(({ i, person }) => (person!.gender === 'female') !== (i % 2 === 1));
      if (new Set(starts).size > 1) throw new RangeError(`Pinned genders do not alternate ${p.collegeId}/${p.listId}`);
      if (starts.length) p.startsFemale = starts[0];
    }
    const ordinarySlates: OrdinarySlate[] = [], reusable = new Map<string, string[]>();
    const bySlot = new Map<string, string>();
    for (const plan of plans) {
      const candidateIds: string[] = [];
      for (let i = 0; i < 7; i++) {
        const female = plan.startsFemale !== (i % 2 === 1), k = key(plan.collegeId, plan.listId, i), template = pinned.get(k);
        const poolKey = key(plan.listId, female), pool = (reusable.get(poolKey) ?? []).filter(id => !candidateIds.includes(id) && (ordinaryUses.get(id) ?? 0) < s.candidateGeneration.maximumOrdinaryCandidacies);
        const rng = generationRng(s.seed, simulationId, 'candidates', ch, k);
        let id: string;
        if (template) id = own(template);
        else if (i >= plan.prefix && pool.length && rng() < s.candidateGeneration.reuseProbability) id = pool[Math.floor(rng() * pool.length)];
        else {
          id = own(synthetic(ch, 'ordinary', plan.collegeId, plan.listId, i + 1, female));
          // Bonus candidates stay local. Other candidates may have compatible
          // multiple ordinary candidatures, and retain one shared person ID.
          if (i >= plan.prefix) reusable.set(poolKey, [...(reusable.get(poolKey) ?? []), id]);
        }
        ordinaryUses.set(id, (ordinaryUses.get(id) ?? 0) + 1); candidateIds.push(id); bySlot.set(k, id);
      }
      ordinarySlates.push({ collegeId: plan.collegeId, listId: plan.listId, candidateIds, preferences: { kind: 'missing', reason: 'Preferences are filled after vote generation' } });
    }
    const bonusSlates: BonusSlate[] = plannedBonus.map(b => ({ territoryId: b.territoryId, subject: b.subject,
      candidates: b.slots.map(slot => ({ candidateId: bySlot.get(key(slot.slate.collegeId, slot.slate.listId, slot.index))!, affiliationListId: slot.slate.listId })) }));
    const specialCandidatures: SpecialCandidature[] = [], specialCounter = new Map<string, number>();
    for (const college of geo.colleges.filter(c => c.kind === 'special-uninominal')) {
      const coverage = s.coverage.find(c => c.chamber === ch && c.collegeId === college.id)!;
      for (const sub of [...new Map(coverage.listIds.map(id => { const subject = subjectFor(id); return [key(subject.kind, subject.id), subject]; })).values()]) {
        const linkedListIds = coverage.listIds.filter(id => JSON.stringify(subjectFor(id)) === JSON.stringify(sub));
        const k = key(sub.kind, sub.id), index = specialCounter.get(k) ?? 0;
        const template = s.candidateTemplates.find(t => t.chamber === ch && t.role === 'special' && t.locationId === college.id && t.ownerId === sub.id);
        const candidateId = own(template?.candidate ?? synthetic(ch, 'special', college.id, sub.id, 1, index % 2 === 0));
        specialCounter.set(k, index + 1);
        specialCandidatures.push({ collegeId: college.id, candidateId, linkedListIds, affiliationListId: template ? template.affiliationListId : linkedListIds[0] });
      }
    }
    chambers.set(ch, { lists, coalitions, ordinarySlates, bonusSlates, specialCandidatures });
  }
  return { candidates: [...people.values()], chambers };
}

import type { Candidate, Chamber, OrdinarySlate, BonusSlate, SpecialCandidature } from './input';
import type { ReformGenerationScenario, ReformGenerationSnapshot } from './generation-types';
import { generateLegacyReformCandidatures, type GeneratedCandidatures } from './legacy-candidate-generation';
import { ordinaryPlans, suppliedOrdinaryUses, type PreparedBonusPlan } from './reference-bonus-plan';
import { bonusPlacementOptions, type BonusPerson } from './bonus-placement';
import { bonusKey } from './bonus-reference-support';
import { generationRng } from './generation-math';

export type { GeneratedCandidatures } from './legacy-candidate-generation';

export function generateReformCandidatures(s: ReformGenerationScenario, snapshot: ReformGenerationSnapshot,
  simulationId: number, prepared?: PreparedBonusPlan): GeneratedCandidatures {
  if (!s.candidateGeneration.bonusPolicy) return generateLegacyReformCandidatures(s, snapshot, simulationId);
  if (!prepared) throw new RangeError('Prepare reference bonus allocations before candidature generation');
  const people = new Map<string, Candidate>();
  const identity = (p: Candidate) => JSON.stringify([p.id, p.name, p.birthDate, p.gender, p.availability.kind,
    p.availability.kind === 'unavailable' ? p.availability.reason : null,
    p.externalCandidacies.map(c => [c.chamber, c.kind]).sort()]);
  const own = (person: Candidate): Candidate => {
    const old = people.get(person.id);
    if (old && identity(old) !== identity(person)) throw new RangeError(`Conflicting candidate identity ${person.id}`);
    people.set(person.id, structuredClone(person)); return person;
  };
  const synthetic = (ch: Chamber, role: string, place: string, owner: string, position: number, female: boolean): Candidate => own({
    id: `synthetic:${simulationId}:${ch}:${role}:${place}:${owner}:${position}`, name: `Candidato sintetico ${ch}/${place}/${owner}/${position}`,
    birthDate: `${Number(s.electionDate.slice(0, 4)) - 50}-01-01`, gender: female ? 'female' : 'male', availability: { kind: 'available' }, externalCandidacies: []
  });
  const chambers: GeneratedCandidatures['chambers'] = new Map();
  for (const ch of s.renewal === 'joint' ? ['camera', 'senato'] as const : [s.renewal]) {
    const geo = snapshot.seats.chambers.find(c => c.chamber === ch)!;
    const plans = ordinaryPlans(s, snapshot, ch);
    for (const t of s.candidateTemplates.filter(t => t.chamber === ch)) own(t.candidate);
    const uses = suppliedOrdinaryUses(plans), declarations = new Map<string, BonusPerson>();
    const bonuses = prepared.slates.filter(b => b.chamber === ch);
    for (const b of bonuses) for (const p of b.supplied) declarations.set(p.template.candidate.id, p);
    const bonusSlates: BonusSlate[] = [];
    for (const b of bonuses) {
      const selected = new Map<string, Candidate[]>();
      for (const allocation of b.allocations) {
        const rng = generationRng(s.seed, simulationId, 'bonus-college-order-v1', ch, b.territoryId, allocation.listId);
        const local = plans.filter(p => p.territoryId === b.territoryId && p.listId === allocation.listId).sort((a, b) => a.collegeId.localeCompare(b.collegeId));
        for (let i = local.length - 1; i > 0; i--) { const j = Math.floor(rng() * (i + 1)); [local[i], local[j]] = [local[j], local[i]]; }
        const option = bonusPlacementOptions(local, b.supplied.filter(p => p.listId === allocation.listId), declarations,
          suppliedOrdinaryUses(plans), s.candidateGeneration.maximumOrdinaryCandidacies, allocation.slots).get(allocation.female);
        if (!option) throw new RangeError(`Incompatible supplied bonus placements ${ch}/${b.territoryId}/${allocation.listId}`);
        for (const [i, p] of local.entries()) {
          if (option.prefixes[i] > 0) p.startsFemale = option.startsFemale[i];
          for (let position = 0; position < option.prefixes[i]; position++) if (!p.people[position]) {
            p.people[position] = option.assignments.get(bonusKey(p.collegeId, p.listId, position)) ??
              synthetic(ch, 'ordinary', p.collegeId, p.listId, position + 1, p.startsFemale !== (position % 2 === 1));
            const id = p.people[position]!.id; uses.set(id, (uses.get(id) ?? 0) + 1);
          }
        }
        const candidates: Candidate[] = [], seen = new Set<string>();
        for (let position = 0; position < 7; position++) for (const [i, p] of local.entries()) if (position < option.prefixes[i]) {
          const person = p.people[position]!, declaration = declarations.get(person.id);
          if (declaration && declaration.template.locationId !== b.territoryId || seen.has(person.id)) continue;
          seen.add(person.id); candidates.push(person);
        }
        if (candidates.length !== allocation.slots) throw new RangeError('Bonus planner lost a distinct candidate');
        selected.set(allocation.listId, candidates);
      }
      const suppliedIds = new Set(b.supplied.map(p => p.template.candidate.id));
      const slots: (BonusSlate['candidates'][number] | undefined)[] = Array(b.allocations.reduce((n, a) => n + a.slots, 0)).fill(undefined);
      for (const p of b.supplied) slots[p.template.position - 1] = { candidateId: p.template.candidate.id, affiliationListId: p.listId };
      const queues = b.allocations.map(a => ({ listId: a.listId, people: selected.get(a.listId)!.filter(p => !suppliedIds.has(p.id)) }));
      const unpinned: BonusSlate['candidates'][number][] = [];
      while (queues.some(q => q.people.length)) for (const q of queues) {
        const person = q.people.shift(); if (person) unpinned.push({ candidateId: person.id, affiliationListId: q.listId });
      }
      for (let i = 0; i < slots.length; i++) if (!slots[i]) slots[i] = unpinned.shift()!;
      bonusSlates.push({ territoryId: b.territoryId, subject: b.subject, candidates: slots as BonusSlate['candidates'] });
    }
    const bonusIds = new Set(bonusSlates.flatMap(b => b.candidates.map(p => p.candidateId)));
    // Reserve all supplied uses and bonus witnesses before reuse. Bonus people
    // remain eligible for compatible extra ordinary candidacies (C19(2),(4)).
    const pools = new Map<string, Candidate[]>();
    for (const [i, p] of plans.entries()) {
      p.startsFemale ??= i % 2 === 0;
      for (const person of p.people) if (person) {
        own(person);
        const k = bonusKey(p.listId, person.gender), pool = pools.get(k) ?? [];
        if (!pool.some(c => c.id === person.id)) pool.push(person); pools.set(k, pool);
      }
    }
    if ([...uses.values()].some(n => n > s.candidateGeneration.maximumOrdinaryCandidacies)) throw new RangeError('Supplied ordinary candidacies exceed the configured maximum');
    const ordinarySlates: OrdinarySlate[] = [];
    for (const p of plans) {
      for (let position = 0; position < 7; position++) if (!p.people[position]) {
        const female = p.startsFemale !== (position % 2 === 1), gender = female ? 'female' : 'male';
        const k = bonusKey(p.listId, gender), rng = generationRng(s.seed, simulationId, 'ordinary-reuse-v2', ch, p.collegeId, p.listId, position);
        const taa = geo.colleges.find(c => c.id === p.collegeId)!.kind === 'taa-proportional';
        const pool = (pools.get(k) ?? []).filter(c => !p.people.some(p => p?.id === c.id) && (uses.get(c.id) ?? 0) < s.candidateGeneration.maximumOrdinaryCandidacies && (!taa || !bonusIds.has(c.id)));
        const person = pool.length && rng() < s.candidateGeneration.reuseProbability ? pool[Math.floor(rng() * pool.length)] : synthetic(ch, 'ordinary', p.collegeId, p.listId, position + 1, female);
        p.people[position] = person; uses.set(person.id, (uses.get(person.id) ?? 0) + 1);
        if (!(pools.get(k) ?? []).some(c => c.id === person.id)) pools.set(k, [...(pools.get(k) ?? []), person]);
      }
      ordinarySlates.push({ collegeId: p.collegeId, listId: p.listId, candidateIds: p.people.map(p => p!.id), preferences: { kind: 'missing', reason: 'Preferences are filled after vote generation' } });
    }
    const lists = s.lists[ch], coalitions = [...new Set(lists.flatMap(l => l.coalitionId ? [l.coalitionId] : []))].sort()
      .map(id => ({ id, listIds: lists.filter(l => l.coalitionId === id).map(l => l.id) }));
    const specialCandidatures: SpecialCandidature[] = [], specialCounter = new Map<string, number>();
    for (const college of geo.colleges.filter(c => c.kind === 'special-uninominal')) {
      const coverage = s.coverage.find(c => c.chamber === ch && c.collegeId === college.id)!;
      const subjects = [...new Set(coverage.listIds.map(id => { const l = lists.find(l => l.id === id)!; return l.coalitionId ?? l.id; }))];
      for (const subject of subjects) {
        const linkedListIds = coverage.listIds.filter(id => { const l = lists.find(l => l.id === id)!; return (l.coalitionId ?? l.id) === subject; });
        const index = specialCounter.get(subject) ?? 0;
        const t = s.candidateTemplates.find(t => t.chamber === ch && t.role === 'special' && t.locationId === college.id && t.ownerId === subject);
        const person = t ? own(t.candidate) : synthetic(ch, 'special', college.id, subject, 1, index % 2 === 0);
        specialCounter.set(subject, index + 1);
        specialCandidatures.push({ collegeId: college.id, candidateId: person.id, linkedListIds, affiliationListId: t ? t.affiliationListId : linkedListIds[0] });
      }
    }
    chambers.set(ch, { lists, coalitions, ordinarySlates, bonusSlates, specialCandidatures });
  }
  return { candidates: [...people.values()], chambers };
}

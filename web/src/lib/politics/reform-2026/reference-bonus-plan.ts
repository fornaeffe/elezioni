import type { Candidate, Chamber, Subject } from './input';
import type { ReformCandidateSettings, ReformAssemblyContext, ReformGenerationSnapshot } from './generation-types';
import type { ProjectedMunicipality } from './paired-vote-generation';
import { bonusKey, bonusReferenceSupport } from './bonus-reference-support';
import { bonusPlacementOptions, type BonusPerson, type OrdinaryPlan } from './bonus-placement';
import { generationRng, sum } from './generation-math';

export interface BonusAllocation { listId: string; expectedVotes: number; share: number; referenceSlots: number; slots: number; female: number }
export interface PreparedBonusSlate {
  chamber: Chamber; territoryId: string; subject: Subject; allocations: BonusAllocation[];
  supplied: BonusPerson[];
}
export interface PreparedBonusPlan {
  policy: 'reference-proportional-v1'; referenceDate: string;
  slates: PreparedBonusSlate[];
}

/** Supplied ordinary positions are immutable. Their gender determines the
 * orientation of the whole alternating seven-slot slate. */
export function ordinaryPlans(s: ReformCandidateSettings, snapshot: ReformAssemblyContext, ch: Chamber): OrdinaryPlan[] {
  const geo = snapshot.seats.chambers.find(c => c.chamber === ch)!;
  const plans = geo.colleges.filter(c => c.kind !== 'special-uninominal').map(c =>
    s.coverage.find(r => r.chamber === ch && r.collegeId === c.id)!.listIds.filter(id => !s.lists[ch].some(l => l.id === id && l.kind === 'other')).slice().sort().map(listId => ({
      collegeId: c.id, territoryId: c.territoryId, listId, people: Array<Candidate | undefined>(7).fill(undefined)
    }) as OrdinaryPlan)).flat();
  for (const t of s.candidateTemplates.filter(t => t.chamber === ch && t.role === 'ordinary')) {
    const p = plans.find(p => p.collegeId === t.locationId && p.listId === t.ownerId);
    if (!p) throw new RangeError(`Unmatched ordinary candidate ${t.locationId}/${t.ownerId}`);
    const start = (t.candidate.gender === 'female') !== (t.position % 2 === 0);
    if (p.startsFemale !== undefined && p.startsFemale !== start) throw new RangeError(`Supplied genders do not alternate ${p.collegeId}/${p.listId}`);
    if (p.people.some(c => c?.id === t.candidate.id)) throw new RangeError(`Repeated ordinary person ${t.candidate.id}/${p.collegeId}`);
    p.startsFemale = start; p.people[t.position - 1] = t.candidate;
  }
  return plans;
}
export function suppliedOrdinaryUses(plans: readonly OrdinaryPlan[]): Map<string, number> {
  const counts = new Map<string, number>();
  for (const p of plans) for (const person of p.people) if (person) counts.set(person.id, (counts.get(person.id) ?? 0) + 1);
  return counts;
}

function largestRemainders(total: number, weights: readonly number[], seed: string, scope: string): number[] {
  const mass = sum(weights);
  if (!mass) throw new RangeError(`No reference support for coalition bonus slate ${scope}; supply reference estimates or every bonus candidate`);
  const rng = generationRng(seed, 0, 'bonus-reference-quotas-v1', scope);
  const rows = weights.map((w, i) => ({ i, quota: total * w / mass, tie: rng() }));
  const counts = rows.map(r => Math.floor(r.quota));
  rows.sort((a, b) => (b.quota % 1) - (a.quota % 1) || a.tie - b.tie);
  const remaining = total - sum(counts);
  for (let i = 0; i < remaining; i++) counts[rows[i].i]++;
  return counts;
}
type Cost = readonly [number, number, number];
const less = (a: Cost, b: Cost) => a[0] < b[0] || a[0] === b[0] &&
  (a[1] < b[1] - 1e-10 || Math.abs(a[1] - b[1]) <= 1e-10 && a[2] < b[2]);
const plus = (a: Cost, b: Cost): Cost => [a[0] + b[0], a[1] + b[1], a[2] + b[2]];

/** Freeze both allocations and feasible gender budgets at the reference date.
 * Hamilton is preferred exactly; supplied people/prefix capacity and the
 * national gender rule constrain it. Among legal alternatives minimize total
 * departures from Hamilton, then squared departures from fractional quotas.
 * No election draws, supplied election votes or territorial-support correction
 * enter this calculation. */
export function prepareReferenceBonusPlan(s: ReformCandidateSettings & { projectionDate?: string }, snapshot: ReformAssemblyContext & Partial<Pick<ReformGenerationSnapshot, 'memberships'>>,
  units: readonly ProjectedMunicipality[], compiled?: { referenceDate: string; support: ReadonlyMap<string, number> }): PreparedBonusPlan {
  const referenceDate = compiled?.referenceDate ?? s.projectionDate;
  if (!referenceDate) throw new Error('Reference bonus plan requires an explicit reference date');
  const output: PreparedBonusPlan = { policy: 'reference-proportional-v1', referenceDate, slates: [] };
  if (s.renewal !== 'joint') return output;
  if (!compiled && !snapshot.memberships) throw new Error('Reference bonus support was not compiled');
  const support = compiled?.support ?? bonusReferenceSupport(s, { seats: snapshot.seats, memberships: snapshot.memberships! }, units);
  for (const ch of ['camera', 'senato'] as const) {
    const plans = ordinaryPlans(s, snapshot, ch), uses = suppliedOrdinaryUses(plans);
    const declarations = new Map<string, BonusPerson>();
    const geo = snapshot.seats.chambers.find(c => c.chamber === ch)!;
    const subjectFor = (id: string): Subject => {
      const list = s.lists[ch].find(l => l.id === id)!;
      return { kind: list.coalitionId ? 'coalition' : 'list', id: list.coalitionId ?? id };
    };
    const groups = new Map<string, { slate: PreparedBonusSlate; options: Map<number, { cost: Cost; allocations: BonusAllocation[] }> }[]>();
    const territories = geo.territories.filter(t => t.bonusSeats > 0).slice().sort((a, b) => a.id.localeCompare(b.id));
    // Resolve supplied affiliations before prefix construction. A null
    // affiliation uses that person's ordinary badge, then the unpinned plan.
    const pending: { slate: PreparedBonusSlate; plans: OrdinaryPlan[]; total: number; targets: number[]; weights: number[] }[] = [];
    for (const t of territories) {
      const local = plans.filter(p => p.territoryId === t.id);
      const subjects = [...new Map(local.map(p => { const sub = subjectFor(p.listId); return [bonusKey(sub.kind, sub.id), sub]; })).values()];
      for (const subject of subjects) {
        const eligible = local.filter(p => bonusKey(subjectFor(p.listId).kind, subjectFor(p.listId).id) === bonusKey(subject.kind, subject.id));
        const ids = [...new Set(eligible.map(p => p.listId))].sort();
        const values = ids.map(id => support.get(bonusKey(ch, t.id, id)) ?? 0);
        const templates = s.candidateTemplates.filter(p => p.chamber === ch && p.role === 'bonus' && p.locationId === t.id && p.ownerId === subject.id)
          .slice().sort((a, b) => a.position - b.position);
        const scope = bonusKey(ch, t.id, subject.kind, subject.id);
        const weights = ids.length === 1 ? [1] : values;
        const targets = sum(weights) ? largestRemainders(t.bonusSeats, weights, s.seed, scope) :
          templates.length === t.bonusSeats ? ids.map(id => templates.filter(p => p.affiliationListId === id).length) :
            largestRemainders(t.bonusSeats, weights, s.seed, scope);
        const ownerOrder: string[] = [];
        for (let round = 0; ownerOrder.length < sum(targets); round++) for (const [i, id] of ids.entries()) if (targets[i] > round) ownerOrder.push(id);
        const supplied = templates.map(template => {
          const badges = [...new Set(plans.filter(p => p.people.some(c => c?.id === template.candidate.id)).map(p => p.listId))];
          if (badges.length > 1 || template.affiliationListId !== null && badges.some(id => id !== template.affiliationListId)) throw new RangeError(`Conflicting supplied badge ${template.candidate.id}`);
          const listId = template.affiliationListId ?? badges[0] ?? ownerOrder[template.position - 1];
          if (!ids.includes(listId)) throw new RangeError(`Supplied bonus candidate lacks a locally filed affiliation ${scope}/${template.candidate.id}`);
          if (declarations.has(template.candidate.id)) throw new RangeError(`Candidate belongs to multiple bonus slots ${template.candidate.id}`);
          const person = { template, listId }; declarations.set(template.candidate.id, person); return person;
        });
        pending.push({ slate: { chamber: ch, territoryId: t.id, subject, allocations: [], supplied }, plans: eligible, total: t.bonusSeats, targets, weights: values });
      }
    }
    if (s.candidateTemplates.some(t => t.chamber === ch && t.role === 'bonus' && !declarations.has(t.candidate.id))) throw new RangeError(`Unmatched ${ch} bonus candidate template`);
    for (const task of pending) {
      const ids = [...new Set(task.plans.map(p => p.listId))].sort();
      type State = { cost: Cost; allocations: BonusAllocation[] };
      let states = new Map<string, State>([[bonusKey(0, 0), { cost: [0, 0, 0], allocations: [] }]]);
      const mass = sum(task.weights);
      for (const [i, listId] of ids.entries()) {
        const next = new Map<string, State>();
        const required = task.slate.supplied.filter(p => p.listId === listId);
        const local = task.plans.filter(p => p.listId === listId);
        const options = Array.from({ length: task.total + 1 }, (_, slots) => bonusPlacementOptions(local, required, declarations, uses,
          s.candidateGeneration.maximumOrdinaryCandidacies, slots));
        for (const [k, previous] of states) {
          const [used, female] = JSON.parse(k) as number[];
          for (let slots = required.length; slots <= task.total - used; slots++) for (const [f, placement] of options[slots]) {
            const share = mass ? task.weights[i] / mass : 0;
            const cost = plus(previous.cost, [Math.abs(slots - task.targets[i]), (slots - task.total * share) ** 2, placement.traversalCost]);
            const key = bonusKey(used + slots, female + f), old = next.get(key);
            if (!old || less(cost, old.cost)) next.set(key, { cost, allocations: [...previous.allocations,
              { listId, expectedVotes: task.weights[i], share, referenceSlots: task.targets[i], slots, female: f }] });
          }
        }
        states = next;
      }
      const options = new Map<number, State>();
      for (const [k, state] of states) { const [used, female] = JSON.parse(k) as number[]; if (used === task.total) options.set(female, state); }
      if (!options.size) throw new RangeError(`Supplied candidates/prefix capacity cannot fill bonus slate ${ch}/${task.slate.territoryId}/${task.slate.subject.id}`);
      const k = bonusKey(task.slate.subject.kind, task.slate.subject.id), group = groups.get(k) ?? [];
      group.push({ slate: task.slate, options }); groups.set(k, group);
    }
    for (const [subject, tasks] of groups) {
      type National = { cost: Cost; slates: PreparedBonusSlate[] };
      let states = new Map<number, National>([[0, { cost: [0, 0, 0], slates: [] }]]);
      for (const task of tasks) {
        const next = new Map<number, National>();
        for (const [before, previous] of states) for (const [female, option] of task.options) {
          const total = before + female, cost = plus(previous.cost, option.cost), old = next.get(total);
          if (!old || less(cost, old.cost)) next.set(total, { cost, slates: [...previous.slates, { ...task.slate, allocations: option.allocations }] });
        }
        states = next;
      }
      const total = sum(tasks.map(t => sum([...t.options.values()][0].allocations.map(a => a.slots))));
      // C18-bis(3.1)/S9(4-bis): national subject limit, rounded to nearest unit.
      const maximum = Math.floor((total * 6 + 5) / 10);
      const feasible = [...states].filter(([female]) => female <= maximum && total - female <= maximum)
        .sort(([fa, a], [fb, b]) => less(a.cost, b.cost) ? -1 : less(b.cost, a.cost) ? 1 : Math.abs(fa - total / 2) - Math.abs(fb - total / 2));
      if (!feasible.length) throw new RangeError(`Supplied candidates cannot satisfy national bonus gender balance ${ch}/${subject}`);
      output.slates.push(...feasible[0][1].slates);
    }
  }
  return output;
}

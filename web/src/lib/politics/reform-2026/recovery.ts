import { compareExact, exactFraction } from './exact';
import type { AggregatedElectoralFigures } from './figures';
import type { Candidate, ChamberInput, OrdinarySlate, Subject } from './input';
import { resolveMultipleElections, type MultipleElectionDecision, type MultipleElectionsResult } from './multiple-elections';
import type { CandidateElection } from './proclamations';
import { rankPreferenceCandidates } from './rankings';
import type { DrawTrace, ExactFraction, RecoveryTrace, ReformWarning, ScrutinyExecutionContext, SeatLedgerEntry, SeatLocation } from './result';
import { resolveTie } from './ties';

/** A final allocation quotient, NOT a new allocation computed during recovery.
 * Supply list and coalition rows separately, including explicit null quotients.
 * College null denotes the constituency (Camera) or region (Senato). */
export interface RecoveryRemainder {
  readonly id: string;
  readonly territoryId: string;
  readonly collegeId: string | null;
  readonly subject: Subject;
  readonly value: ExactFraction | null;
  readonly state: 'unused' | 'used' | 'consumed';
}
export interface RecoveryInput {
  readonly chamber: ChamberInput;
  readonly candidates: readonly Candidate[];
  readonly figures: AggregatedElectoralFigures;
  readonly initial: MultipleElectionsResult;
  /** Only lists admitted to the applicable territorial allocation. A filed
   * slate alone does not establish admission (including local exceptions). */
  readonly eligibleLists: readonly { readonly territoryId: string; readonly listId: string }[];
  readonly remainders: readonly RecoveryRemainder[];
}
export interface RecoveryRanking {
  readonly id: string;
  readonly criterion: 'remainder' | 'list-percentage';
  readonly alternatives: readonly { readonly id: string; readonly value: ExactFraction | null;
    readonly remainder: RecoveryRemainder | null }[];
  readonly chosenId: string | null;
}
export interface InitialRecoveryTrace extends RecoveryTrace {
  readonly round: number;
  readonly vacancyAt: SeatLocation;
  readonly candidateChecks: readonly { readonly collegeId: string; readonly listId: string;
    readonly candidateId: string; readonly state: 'eligible' | 'unavailable' | 'already-elected' | 'reserved-in-slate' | 'unresolved-election' }[];
  readonly rankings: readonly RecoveryRanking[];
  /** A proposed election can be released again by N24 in this same round. */
  readonly disposition: 'retained' | 'released' | 'unresolved' | 'exhausted';
}
export interface InitialRecoveryResult {
  readonly chamber: ChamberInput['chamber'];
  readonly simulationId: number;
  readonly status: 'complete' | 'partial' | 'non-computable';
  readonly blockers: readonly string[];
  readonly terminalReason: 'all-filled' | 'permitted-routes-exhausted-or-unresolved' | 'prerequisite-non-computable';
  readonly seats: readonly SeatLedgerEntry[];
  readonly retained: readonly CandidateElection[];
  readonly rounds: readonly { readonly round: number; readonly filledBefore: number; readonly filledAfter: number;
    readonly proposed: number; readonly releasedSeatIds: readonly string[] }[];
  readonly decisions: readonly MultipleElectionDecision[];
  readonly recoveries: readonly InitialRecoveryTrace[];
  /** Owned, unchanged allocation priorities. Recovery does not recalculate quotas. */
  readonly remainders: readonly RecoveryRemainder[];
  readonly draws: readonly DrawTrace[];
  readonly warnings: readonly ReformWarning[];
}
const key = (...p: unknown[]) => JSON.stringify(p);
const subjectKey = (s: Subject) => key(s.kind, s.id);
const list = (id: string): Subject => ({ kind: 'list', id });
const order = <T>(rows: readonly T[], id: (r: T) => string) => [...rows].sort((a, b) => id(a) < id(b) ? -1 : id(a) > id(b) ? 1 : 0);
interface Leaf { readonly kind: 'candidate'; readonly slate: OrdinarySlate; readonly candidateId: string | null; readonly reason: string | null }
interface Branch { readonly kind: 'rank'; readonly id: string; readonly criterion: RecoveryRanking['criterion'];
  readonly options: readonly { readonly id: string; readonly node: Node; readonly value: ExactFraction | null; readonly remainder: RecoveryRemainder | null }[] }
type Node = Leaf | Branch;
interface Route { readonly id: string; readonly law: string; readonly node: Node | null; readonly rule: 'statutory' | 'I03' }

/** N25/N26/N27, domestic INITIAL scrutiny only. Future C86(3)/S21-ter
 * vacancies and supplementary elections are deliberately outside this API.
 *
 * C84(2): "negli altri collegi ... maggiore parte decimale ... non utilizzata";
 * then already-used decimals. C84(4) repeats this across constituencies.
 * C84(5): coalition in the original college, then the other local colleges;
 * C84(7) extends coalition recovery to other constituencies only for Camera
 * initial exhaustion, NOT C86(2)'s limited reference to (2),(4),(5).
 * C84(8): "in caso di parità ... si procede mediante sorteggio".
 *
 * S17-bis(2), S19(2), approved I07: regional (2),(5), then residual SAME LIST
 * (4). No ordinary (7). C83/S16-bis(1-quinquies) bonus shortages separately
 * import C86(1): local member-list remainder, best college list percentage,
 * then other territories ranked by the bonus-winning subject's remainder.
 *
 * Recovery uses the final ordinary quotient states as ranking evidence; it
 * does not transfer ordinary allocation cells or consume their decimals anew.
 * A correction's 'consumed' remainder is already-used, never unused. Candidate
 * availability, rather than an invented extra quota, bounds repeated recovery.
 */
export function recoverInitialSeats(input: RecoveryInput, execution: ScrutinyExecutionContext): InitialRecoveryResult {
  preflight(input);
  const { chamber: c, initial } = input;
  const simulationId = initial.simulationId;
  let seats = structuredClone([...initial.seats]);
  let retained = structuredClone([...initial.retained]);
  const rounds: InitialRecoveryResult['rounds'][number][] = [];
  const decisions: MultipleElectionDecision[] = [], recoveries: InitialRecoveryTrace[] = [];
  const draws: DrawTrace[] = [], warnings: ReformWarning[] = [];
  const remainders = order(input.remainders, r => r.id);
  const people = new Map(input.candidates.map(p => [p.id, p]));
  const eligible = new Set(input.eligibleLists.map(r => key(r.territoryId, r.listId)));
  const released = new Set([...initial.released.map(r => r.seatId), ...initial.seats.flatMap(s =>
    s.status !== 'filled' && s.reason === 'multiple-election-requires-recovery' ? [s.seatId] : [])]);
  const vacancyAt = new Map(seats.map(s => [s.seatId, s.entitlement]));
  const uncertain = new Set(initial.decisions.filter(d => d.retainedSeatId === null).map(d => d.candidateId));
  const finish = (): InitialRecoveryResult => structuredClone({ chamber: c.chamber, simulationId,
    status: initial.status === 'non-computable' ? 'non-computable' : seats.every(s => s.status === 'filled') ? 'complete' : 'partial',
    blockers: initial.blockers, terminalReason: initial.status === 'non-computable' ? 'prerequisite-non-computable' :
      seats.every(s => s.status === 'filled') ? 'all-filled' : 'permitted-routes-exhausted-or-unresolved',
    seats: order(seats, s => s.seatId), retained: order(retained, e => e.seat.seatId), rounds, decisions, recoveries, remainders, draws, warnings });
  if (initial.status === 'non-computable') return finish();
  const retryReasons = new Set(['multiple-election-requires-recovery', 'candidate-unavailable-requires-recovery',
    'candidate-exhaustion-requires-recovery', 'bonus-shortfall-requires-recovery', 'missing-preferences']);
  for (let round = 1; ; round++) {
    const occupied = new Set(retained.map(e => e.seat.candidateId));
    const reservations = new Set<string>();
    const proposals: CandidateElection[] = [];
    const traces: InitialRecoveryTrace[] = [];
    const pending = order(seats.filter(s => s.status !== 'filled' && retryReasons.has(s.reason) && s.category !== 'special-uninominal'), s => s.seatId);
    if (!pending.length) break;
    for (const seat of pending) {
      const origin = vacancyAt.get(seat.seatId)!;
      const checks: InitialRecoveryTrace['candidateChecks'][number][] = [];
      const rankings: RecoveryRanking[] = [];
      const transitions: RecoveryTrace['remainders'][number][] = [];
      const drawStart = draws.length;
      const norm = c.chamber === 'senato' ? 'N27' : seat.category === 'bonus' || released.has(seat.seatId) ? 'N26' : 'N25';
      // Build eligibility without drawing. Only the chosen route/branch consumes
      // ties, so irrelevant downstream routes never perturb the random stream.
      const leaves = new Map<string, Leaf | null>();
      function candidate(slate: OrdinarySlate): Leaf | null {
        const id = key(slate.collegeId, slate.listId);
        if (leaves.has(id)) return leaves.get(id)!;
        const f = [...input.figures.colleges, ...input.figures.taaProportional].find(f => f.collegeId === slate.collegeId)!;
        const ranking = rankPreferenceCandidates(slate, f.figures);
        const ids = [ranking.capolistaId, ...(ranking.status === 'complete' ? ranking.candidates.map(p => p.candidateId) : [])];
        const inspect = (candidateId: string) => {
          const state = people.get(candidateId)!.availability.kind !== 'available' ? 'unavailable' : uncertain.has(candidateId) ? 'unresolved-election' :
            occupied.has(candidateId) ? 'already-elected' : reservations.has(key(id, candidateId)) ? 'reserved-in-slate' : 'eligible';
          checks.push({ collegeId: slate.collegeId, listId: slate.listId, candidateId, state });
          return state;
        };
        for (const candidateId of ids) {
          const state = inspect(candidateId);
          if (state === 'eligible') {
            const leaf: Leaf = { kind: 'candidate', slate, candidateId, reason: null }; leaves.set(id, leaf); return leaf;
          }
          if (state === 'unresolved-election') {
            const leaf: Leaf = { kind: 'candidate', slate, candidateId: null, reason: 'unresolved-candidate-election' }; leaves.set(id, leaf); return leaf;
          }
        }
        // Missing ranking cannot conceal proven exhaustion: availability and
        // already-retained identities can exclude every remaining filed slot.
        const remaining = ranking.status === 'missing-preferences' ? slate.candidateIds.slice(1).map(inspect) : [];
        const leaf: Leaf | null = remaining.some(s => s === 'eligible' || s === 'unresolved-election') ?
          { kind: 'candidate', slate, candidateId: null, reason: 'missing-preferences' } : null;
        leaves.set(id, leaf); return leaf;
      }
      const slates = order(c.ordinarySlates.filter(s => eligible.has(key(c.colleges.find(t => t.id === s.collegeId)!.territoryId, s.listId))), s => key(s.collegeId, s.listId));
      const at = (s: OrdinarySlate) => c.colleges.find(t => t.id === s.collegeId)!.territoryId;
      function rank(id: string, criterion: Branch['criterion'], options: { id: string; node: Node | null; value: ExactFraction | null; remainder: RecoveryRemainder | null }[]): Branch | null {
        const active = options.flatMap(o => o.node ? [{ ...o, node: o.node }] : []);
        const ordered = order(active, o => o.id).sort((a, b) => {
          if (a.value === null || b.value === null) return a.value === b.value ? 0 : a.value === null ? 1 : -1;
          const state = criterion === 'remainder' ? Number(b.remainder?.state === 'unused') - Number(a.remainder?.state === 'unused') : 0;
          return state || -compareExact(a.value, b.value);
        });
        return active.length ? { kind: 'rank', id, criterion, options: ordered } : null;
      }
      function decimal(territoryId: string, collegeId: string | null, subject: Subject, node: Node | null) {
        const r = remainders.find(r => r.territoryId === territoryId && r.collegeId === collegeId && subjectKey(r.subject) === subjectKey(subject)) ?? null;
        return { id: key(territoryId, collegeId, subjectKey(subject)), node, value: r?.value ?? null, remainder: r };
      }
      const sameList = (territoryId: string, listId: string, exclude: string | null = null) => rank(key('same-list', territoryId, listId), 'remainder',
        slates.filter(s => at(s) === territoryId && s.listId === listId && s.collegeId !== exclude)
          .map(s => decimal(territoryId, s.collegeId, list(listId), candidate(s))));
      const memberIds = (subject: Subject) => subject.kind === 'list' ? [subject.id] : c.coalitions.find(g => g.id === subject.id)!.listIds;
      function coalitionCollege(territoryId: string, collegeId: string, subject: Subject): Node | null {
        return rank(key('coalition-lists', collegeId, subjectKey(subject)), 'remainder', slates.filter(s => at(s) === territoryId && s.collegeId === collegeId && memberIds(subject).includes(s.listId))
          .map(s => decimal(territoryId, collegeId, list(s.listId), candidate(s))));
      }
      function coalitionTerritory(territoryId: string, subject: Subject, exclude: string | null = null): Node | null {
        return rank(key('coalition-colleges', territoryId, subjectKey(subject)), 'remainder',
          c.colleges.filter(t => t.territoryId === territoryId && t.kind !== 'special-uninominal' && t.id !== exclude)
            .map(t => decimal(territoryId, t.id, subject, coalitionCollege(territoryId, t.id, subject))));
      }
      function bonusTerritory(territoryId: string, subject: Subject): Node | null {
        // C86(1): "nel collegio ... maggiore cifra elettorale percentuale";
        // coalition-list decimals precede that college percentage comparison.
        const members = memberIds(subject).map(listId => {
          const options = slates.filter(s => at(s) === territoryId && s.listId === listId).map(s => {
            const f = [...input.figures.colleges, ...input.figures.taaProportional].find(f => f.collegeId === s.collegeId)!;
            const percentage = f.figures.lists.find(l => l.listId === listId)!.percentage;
            return { id: s.collegeId, node: candidate(s), value: percentage.status === 'defined' ? percentage.value : null, remainder: null };
          });
          return decimal(territoryId, null, list(listId), rank(key('bonus-colleges', territoryId, listId), 'list-percentage', options));
        });
        // A single list needs no coalition-list remainder to identify itself.
        return subject.kind === 'list' ? members[0].node : rank(key('bonus-lists', territoryId), 'remainder', members);
      }
      const otherTerritories = (subject: Subject, make: (id: string) => Node | null) => rank(key('other-territories', subjectKey(subject)), 'remainder',
        c.territories.filter(t => t.id !== origin.territoryId && t.kind === 'ordinary')
          .map(t => decimal(t.id, null, subject, make(t.id))));
      const routes: Route[] = [];
      if (seat.category === 'bonus') {
        routes.push({ id: 'bonus-local', law: 'C83(1-quinquies)/C86(1)', rule: 'I03', node: bonusTerritory(origin.territoryId, seat.entitlement.subject) },
          { id: 'bonus-other-territory', law: 'C83(1-quinquies)/C86(1)', rule: 'I03', node: otherTerritories(seat.entitlement.subject, t => bonusTerritory(t, seat.entitlement.subject)) });
      } else {
        const listId = origin.listId!;
        const coalitionId = c.lists.find(l => l.id === listId)!.coalitionId;
        const coalition: Subject | null = coalitionId && origin.subject.kind === 'coalition' ? { kind: 'coalition', id: coalitionId } : null;
        routes.push({ id: 'same-college', law: 'C86(1)', rule: 'statutory', node: candidate(c.ordinarySlates.find(s => s.collegeId === origin.collegeId && s.listId === listId)!) },
          { id: 'same-list-local', law: 'C84(2)', rule: 'statutory', node: sameList(origin.territoryId, listId, origin.collegeId) });
        const cross: Route = { id: 'same-list-other-territory', law: c.chamber === 'senato' ? 'S19(2)/C86(2)/C84(4)' : 'C84(4)', rule: 'statutory',
          node: otherTerritories(list(listId), t => sameList(t, listId)) };
        const local: Route[] = coalition ? [
          { id: 'coalition-original-college', law: 'C84(5)', rule: 'statutory', node: coalitionCollege(origin.territoryId, origin.collegeId!, coalition) },
          { id: 'coalition-local-colleges', law: 'C84(5)', rule: 'statutory', node: coalitionTerritory(origin.territoryId, coalition, origin.collegeId) }
        ] : [];
        if (c.chamber === 'senato') routes.push(...local, cross);
        else {
          routes.push(cross, ...local);
          if (coalition && !released.has(seat.seatId)) routes.push({ id: 'coalition-other-territory', law: 'C84(7)/(4)/(5)', rule: 'statutory',
            node: otherTerritories(coalition, t => coalitionTerritory(t, coalition)) });
        }
      }
      const route = routes.find(r => r.node);
      let failure = 'permitted-candidates-exhausted';
      function choose(node: Node, route: Route): Leaf | null {
        if (node.kind === 'candidate') { failure = node.reason ?? failure; return node; }
        const all = node.options;
        // Unknown lower-tier decimals do not obstruct a known unused tier.
        // A wholly missing remainder also lacks its tier, so remains blocking.
        const unknownTier = node.criterion === 'remainder' && all.some(o => o.remainder === null);
        const options = node.criterion === 'remainder' && all.some(o => o.remainder?.state === 'unused') ?
          all.filter(o => o.remainder?.state === 'unused') : all;
        if (unknownTier || options.some(o => o.value === null)) {
          rankings.push({ id: node.id, criterion: node.criterion, alternatives: all.map(({ id, value, remainder }) => ({ id, value, remainder })), chosenId: null });
          failure = node.criterion === 'remainder' ? 'missing-recovery-remainder' : 'undefined-recovery-percentage'; return null;
        }
        const resolved = resolveTie(options.map(o => ({ id: o.id, values: node.criterion === 'remainder' ?
          [exactFraction(o.remainder?.state === 'unused' ? 1 : 0), o.value!] : [o.value!] })),
        (node.criterion === 'remainder' ? ['unused-before-used', 'decimal'] : ['list-percentage']).map(name => ({ name, direction: 'descending' })),
        { simulationId, request: { contextId: key(norm, c.chamber, simulationId, round, seat.seatId, route.id, node.id), chamber: c.chamber,
          norm, lawReference: route.law, rule: route.rule } }, execution);
        draws.push(...resolved.draws); warnings.push(...resolved.warnings);
        rankings.push({ id: node.id, criterion: node.criterion, alternatives: all.map(({ id, value, remainder }) => ({ id, value, remainder })), chosenId: resolved.chosenId });
        const selected = options.find(o => o.id === resolved.chosenId)!;
        if (selected.remainder && selected.value) transitions.push({ id: selected.remainder.id, value: selected.value,
          before: selected.remainder.state, after: selected.remainder.state });
        return choose(selected.node, route);
      }
      const leaf = route ? choose(route.node!, route) : null;
      const law = route ? (c.chamber === 'senato' ? `${seat.category === 'bonus' ? 'S16-bis(1-quinquies)' : 'S17-bis(2)'}; ${route.law}` : route.law) :
        c.chamber === 'senato' ? 'S17-bis(2)/S19(2)' : 'C84/C86';
      let election: CandidateElection | null = null;
      if (leaf?.candidateId) {
        const t = at(leaf.slate);
        const electedAt: SeatLocation = { chamber: c.chamber, territoryId: t, collegeId: leaf.slate.collegeId,
          subject: seat.entitlement.subject, listId: leaf.slate.listId };
        const f = [...input.figures.colleges, ...input.figures.taaProportional].find(f => f.collegeId === leaf.slate.collegeId)!;
        const ranking = rankPreferenceCandidates(leaf.slate, f.figures);
        election = { source: 'ordinary-recovery', seat: { seatId: seat.seatId, category: seat.category, entitlement: seat.entitlement, status: 'filled', electedAt, candidateId: leaf.candidateId },
          role: seat.category === 'bonus' ? 'bonus' : leaf.candidateId === ranking.capolistaId ? 'capolista' : 'preference', affiliationListId: leaf.slate.listId,
          listPercentage: ranking.listPercentage, individualPercentage: ranking.status === 'complete' ? ranking.candidates.find(p => p.candidateId === leaf.candidateId)?.percentage ?? null : null };
        proposals.push(election); reservations.add(key(key(leaf.slate.collegeId, leaf.slate.listId), leaf.candidateId));
      } else {
        seats = seats.map(s => s.seatId === seat.seatId ? { seatId: s.seatId, category: s.category, entitlement: s.entitlement,
          status: route ? 'unresolved' : 'unfilled', reason: failure } : s);
      }
      const crossRegion = election && c.chamber === 'senato' &&
        c.territories.find(t => t.id === seat.entitlement.territoryId)!.regionId !== c.territories.find(t => t.id === election!.seat.electedAt.territoryId)!.regionId;
      const trace: InitialRecoveryTrace = { round, seatId: seat.seatId, route: route?.id ?? 'exhausted', lawReference: law,
        interpretationId: crossRegion ? 'I07' : null, originalEntitlement: seat.entitlement, vacancyAt: origin,
        electedAt: election?.seat.electedAt ?? null, candidateId: election?.seat.candidateId ?? null,
        consideredCandidateIds: [...new Set(checks.map(p => p.candidateId))].sort(), candidateChecks: order(checks, p => key(p.collegeId, p.listId)),
        remainders: transitions, rankings, drawContextIds: draws.slice(drawStart).map(d => d.contextId),
        outcome: election ? 'filled' : route ? 'unresolved' : 'unfilled', disposition: election ? 'retained' : route ? 'unresolved' : 'exhausted' };
      traces.push(trace);
    }
    if (!proposals.length) { recoveries.push(...traces); break; }
    const before = retained.length;
    const bySeat = new Map(proposals.map(e => [e.seat.seatId, e]));
    const result = resolveMultipleElections({ chamber: c.chamber, simulationId, status: 'partial', blockers: [],
      seats: seats.map(s => bySeat.get(s.seatId)?.seat ?? s), elections: [...retained, ...proposals] }, execution);
    draws.push(...result.draws); warnings.push(...result.warnings); decisions.push(...result.decisions);
    for (const r of result.released) { released.add(r.seatId); vacancyAt.set(r.seatId, bySeat.get(r.seatId)!.seat.electedAt); }
    for (const d of result.decisions) if (d.retainedSeatId === null) uncertain.add(d.candidateId);
    for (const trace of traces) {
      const final = result.seats.find(s => s.seatId === trace.seatId)!;
      const disposition = trace.candidateId === null ? trace.disposition : final.status === 'filled' ? 'retained' :
        result.released.some(r => r.seatId === trace.seatId) ? 'released' : 'unresolved';
      const precedenceDraws = result.decisions.find(d => d.candidateId === trace.candidateId)?.drawContextIds ?? [];
      recoveries.push({ ...trace, disposition,
        outcome: disposition === 'released' ? 'unfilled' : disposition === 'unresolved' ? 'unresolved' : trace.outcome,
        drawContextIds: [...new Set([...trace.drawContextIds, ...precedenceDraws])] });
      if (trace.interpretationId === 'I07') warnings.push({ code: 'I07_CROSS_REGION_RECOVERY', severity: 'warning', electionKind: 'politiche',
        simulationId, chamber: 'senato', interpretationId: 'I07', seatId: trace.seatId, lawReference: trace.lawReference,
        message: `Provisional I07 ${trace.route}: ${trace.originalEntitlement.territoryId} -> ${trace.electedAt!.territoryId}; ${disposition}. Original entitlement preserved.` });
    }
    seats = [...result.seats]; retained = [...result.retained];
    rounds.push({ round, filledBefore: before, filledAfter: retained.length, proposed: proposals.length, releasedSeatIds: result.released.map(r => r.seatId) });
    // Every successful wave adds a previously unelected person. Undefined N24
    // comparisons freeze the competing seats. Never retry an unchanged state.
    if (retained.length <= before && !result.decisions.some(d => d.retainedSeatId === null)) throw new Error('Initial recovery made no progress');
  }
  return finish();
}

function preflight({ chamber: c, candidates, figures, initial, eligibleLists, remainders }: RecoveryInput): void {
  const unique = (ids: readonly string[]) => ids.every(Boolean) && new Set(ids).size === ids.length;
  if (initial.chamber !== c.chamber || !Number.isSafeInteger(initial.simulationId) || initial.simulationId < 0 ||
      !unique(candidates.map(p => p.id)) || !unique(initial.seats.map(s => s.seatId)) ||
      !unique(initial.retained.map(e => e.seat.candidateId)) || !unique(initial.retained.map(e => e.seat.seatId)) ||
      !unique(remainders.map(r => r.id)) || !unique(remainders.map(r => key(r.territoryId, r.collegeId, subjectKey(r.subject)))) ||
      !unique(eligibleLists.map(r => key(r.territoryId, r.listId)))) throw new RangeError('Supply matching unique recovery communications');
  const people = new Map(candidates.map(p => [p.id, p]));
  const validSubject = (s: Subject) => s.kind === 'list' ? c.lists.some(l => l.id === s.id) : c.coalitions.some(g => g.id === s.id);
  for (const r of remainders) {
    if (!c.territories.some(t => t.id === r.territoryId) || (r.collegeId !== null && !c.colleges.some(t => t.id === r.collegeId && t.territoryId === r.territoryId && t.kind !== 'special-uninominal')) ||
        !validSubject(r.subject) || !['unused', 'used', 'consumed'].includes(r.state) ||
        (r.value && compareExact(exactFraction(r.value.numerator, r.value.denominator), exactFraction(1)) >= 0)) throw new RangeError('Invalid recovery remainder');
  }
  for (const r of eligibleLists) if (!c.territories.some(t => t.id === r.territoryId) || !c.lists.some(l => l.id === r.listId)) throw new RangeError('Unknown recovery admission');
  if (!unique(c.ordinarySlates.map(s => key(s.collegeId, s.listId)))) throw new RangeError('Duplicate recovery slate');
  for (const s of c.ordinarySlates) {
    const college = c.colleges.find(t => t.id === s.collegeId);
    const fs = [...figures.colleges, ...figures.taaProportional].filter(f => f.collegeId === s.collegeId);
    if (!college || college.kind === 'special-uninominal' || !c.lists.some(l => l.id === s.listId) || !s.candidateIds.length || !unique(s.candidateIds) ||
        s.candidateIds.some(id => !people.has(id)) || fs.length !== 1) throw new RangeError('Supply filed recovery slates and matching figures');
    rankPreferenceCandidates(s, fs[0].figures);
  }
  for (const s of initial.seats) {
    const location = s.entitlement;
    const election = initial.retained.find(e => e.seat.seatId === s.seatId);
    const subjectMatchesList = location.subject.kind === 'list' ? location.listId === location.subject.id :
      c.coalitions.find(g => g.id === location.subject.id)?.listIds.includes(location.listId ?? '');
    if (location.chamber !== c.chamber || !c.territories.some(t => t.id === location.territoryId) || !validSubject(location.subject) ||
        (s.category !== 'bonus' && !c.colleges.some(t => t.id === location.collegeId && t.territoryId === location.territoryId)) ||
        (['ordinary', 'taa-proportional'].includes(s.category) && (!subjectMatchesList ||
          !eligibleLists.some(r => r.territoryId === location.territoryId && r.listId === location.listId) ||
          !c.ordinarySlates.some(t => t.collegeId === location.collegeId && t.listId === location.listId))) ||
        (s.category === 'bonus' && (location.collegeId !== null || location.listId !== (location.subject.kind === 'list' ? location.subject.id : null))) ||
        (s.status === 'filled' && (!election || JSON.stringify(election.seat) !== JSON.stringify(s) || people.get(s.candidateId)?.availability.kind !== 'available')) ||
        (s.status !== 'filled' && election)) throw new RangeError('Invalid recovery seat or retained election');
  }
  if (initial.retained.some(e => !initial.seats.some(s => s.seatId === e.seat.seatId))) throw new RangeError('Unknown retained seat');
}

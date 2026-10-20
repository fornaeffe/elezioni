import { addExact, compareExact, divideExact, exactFraction, floorExact, fractionalPart, truncateToSixDecimals } from './exact';
import { sumVoteCounts } from './figures';
import type { DrawTrace, ExactFraction, NormId, ReformWarning, ScrutinyExecutionContext } from './result';
import { resolveTie, type TieCriterion, type TieResolution } from './ties';

/** Shared exact matrix arithmetic. Callers supply chamber, statutory scope,
 * electoral figures and correction route; no chamber tie policy is inferred. */
export interface TerritorialMatrixInput {
  readonly chamber: 'camera' | 'senato';
  readonly contextId: string;
  readonly simulationId: number;
  readonly norm: NormId;
  readonly lawReference: string;
  readonly targets: readonly { readonly id: string; readonly seats: number; readonly votes: number }[];
  readonly areas: readonly {
    readonly id: string;
    readonly seats: number;
    readonly entries: readonly { readonly id: string; readonly votes: number; readonly tieVotes: number;
      /** I12: filed territorial coverage, independent of a zero vote figure. */
      readonly eligible?: boolean }[];
  }[];
  /** C83(h)/(i); paired college lists; C83-bis(1)(b)(3)'s independent
   * Camera corrections; or C83-bis(1-bis)(c)/S17(5)(c), respectively. */
  readonly correction: 'constituencies' | 'colleges' | 'independent-colleges' | 'college-groups';
  readonly initialTieFigure: 'national-electoral-figure' | 'constituency-electoral-figure' | 'college-electoral-figure';
  /** Absent: integer local quota. Present: truncated indices, then normalization.
   * null marks an inactive zero-target group; its quota/index is never divided. */
  readonly indexQuotas?: readonly { readonly id: string; readonly quota: ExactFraction | null }[];
}

export interface TerritorialMatrixCell {
  readonly id: string;
  readonly areaId: string;
  readonly targetId: string;
  readonly votes: number;
  readonly allocationEligible: boolean;
  readonly rawIndex: ExactFraction | null;
  readonly index: ExactFraction | null;
  readonly quotient: ExactFraction | null;
  readonly integerSeats: number;
  readonly decimalPart: ExactFraction | null;
  readonly remainderExcluded: boolean;
  readonly remainderSeat: boolean;
  readonly provisionalSeats: number;
  readonly remainderState: 'unused' | 'used' | 'consumed';
  readonly seats: number;
}
export interface TerritorialMatrixTransfer {
  /** Warned loose reading of Camera C83-bis donor eligibility. */
  readonly interpretationId?: 'I13';
  /** Independent b(3) corrections have only a donor or only a recipient. */
  readonly fromCellId: string | null;
  readonly toCellId: string | null;
  readonly route: 'same-area' | 'cross-area' | 'removal' | 'addition';
  readonly fromSeatsBefore: number | null;
  readonly toSeatsBefore: number | null;
  readonly remainders: readonly { readonly id: string; readonly value: ExactFraction;
    readonly before: 'unused' | 'used' | 'consumed'; readonly after: 'unused' | 'used' | 'consumed' }[];
  readonly drawContextIds: readonly string[];
}
interface MatrixTrace {
  readonly norm: NormId;
  readonly lawReference: string;
  readonly chamber: 'camera' | 'senato';
  readonly contextId: string;
  readonly initialTieFigure: TerritorialMatrixInput['initialTieFigure'];
  readonly correction: TerritorialMatrixInput['correction'];
  readonly indexQuotas: TerritorialMatrixInput['indexQuotas'] | null;
  readonly areas: readonly { readonly id: string; readonly capacity: number; readonly rawQuota: ExactFraction | null;
    readonly quota: ExactFraction | null; readonly finalSeats: number }[];
  readonly targets: TerritorialMatrixInput['targets'];
  readonly cells: readonly TerritorialMatrixCell[];
  readonly transfers: readonly TerritorialMatrixTransfer[];
  readonly resolutions: readonly TieResolution[];
  readonly draws: readonly DrawTrace[];
  readonly warnings: readonly ReformWarning[];
}
export type TerritorialMatrixResult = MatrixTrace & (
  | { readonly status: 'complete' }
  | { readonly status: 'non-computable'; readonly phase: 'initial' | 'remainders' | 'correction'; readonly reason: string }
);

const asc = (name: string): TieCriterion => ({ name, direction: 'ascending' });
const desc = (name: string): TieCriterion => ({ name, direction: 'descending' });
const zero = exactFraction(0);
const cellId = (area: string, target: string) => JSON.stringify([area, target]);

export function allocateTerritorialMatrix(input: TerritorialMatrixInput, execution: ScrutinyExecutionContext): TerritorialMatrixResult {
  const targets = [...input.targets].sort((a, b) => a.id < b.id ? -1 : a.id > b.id ? 1 : 0);
  const areas = [...input.areas].sort((a, b) => a.id < b.id ? -1 : a.id > b.id ? 1 : 0);
  const unique = (ids: readonly string[]) => ids.every(Boolean) && new Set(ids).size === ids.length;
  if (!unique(targets.map(t => t.id)) || !unique(areas.map(a => a.id)) ||
      !Number.isSafeInteger(input.simulationId) || input.simulationId < 0) throw new RangeError('Invalid territorial matrix IDs');
  targets.forEach(t => sumVoteCounts([t.seats, t.votes]));
  const independent = input.correction === 'independent-colleges';
  if (independent && (input.chamber !== 'camera' || input.norm !== 'N18' || input.indexQuotas)) {
    throw new RangeError('Independent college corrections require the Camera N18 list quota path');
  }
  // Endowments always determine the INITIAL quotas. Only Camera b(3) can
  // independently remove/add seats to reach a different communicated total.
  const capacity = sumVoteCounts(areas.map(a => a.seats));
  const targetSeats = sumVoteCounts(targets.map(t => t.seats));
  if (!independent && targetSeats !== capacity) {
    throw new RangeError('Territorial matrix capacities must sum to its targets');
  }
  for (const area of areas) {
    if (!unique(area.entries.map(e => e.id)) || area.entries.length !== targets.length ||
        targets.some(t => !area.entries.some(e => e.id === t.id))) throw new RangeError('Supply every matrix figure once');
    area.entries.forEach(e => {
      sumVoteCounts([e.votes, e.tieVotes]);
      if (e.eligible === false && e.votes !== 0) throw new RangeError('Absent territorial lists cannot have votes');
    });
  }
  if (input.indexQuotas && (!unique(input.indexQuotas.map(q => q.id)) || input.indexQuotas.length !== targets.length ||
      targets.some(t => !input.indexQuotas!.some(q => q.id === t.id)))) throw new RangeError('Supply every index quota once');

  let cells: TerritorialMatrixCell[] = [];
  const quotas: { id: string; capacity: number; rawQuota: ExactFraction | null; quota: ExactFraction | null }[] = [];
  const transfers: TerritorialMatrixTransfer[] = [];
  const warnings: ReformWarning[] = [];
  const resolutions: TieResolution[] = [];
  const finish = (failure?: { phase: 'initial' | 'remainders' | 'correction'; reason: string }): TerritorialMatrixResult => structuredClone({
    chamber: input.chamber, norm: input.norm, lawReference: input.lawReference, contextId: input.contextId,
    initialTieFigure: input.initialTieFigure, correction: input.correction,
    indexQuotas: input.indexQuotas ? [...input.indexQuotas].sort((a, b) => a.id < b.id ? -1 : a.id > b.id ? 1 : 0) : null,
    targets, cells, transfers, resolutions,
    areas: quotas.map(a => ({ ...a, finalSeats: sumVoteCounts(cells.filter(c => c.areaId === a.id).map(c => c.seats)) })),
    draws: resolutions.flatMap(r => r.draws), warnings: [...resolutions.flatMap(r => r.warnings), ...warnings],
    ...(failure ? { status: 'non-computable' as const, ...failure } : { status: 'complete' as const })
  });
  const choose = (options: readonly { id: string; values: readonly ExactFraction[] }[], criteria: readonly TieCriterion[],
    suffix: string, rule: 'statutory' | 'I03') => {
    const r = resolveTie(options, criteria, { simulationId: input.simulationId, request: {
      contextId: `${input.contextId}/${suffix}`, chamber: input.chamber, norm: input.norm, lawReference: input.lawReference, rule
    } }, execution);
    resolutions.push(r);
    return r.chosenId;
  };
  const total = (id: string) => sumVoteCounts(cells.filter(c => c.targetId === id).map(c => c.seats));
  const target = (id: string) => targets.find(t => t.id === id)!;
  const remainder = (c: TerritorialMatrixCell) => c.decimalPart!;
  const change = (c: TerritorialMatrixCell, patch: Partial<TerritorialMatrixCell>) => {
    const updated = { ...c, ...patch };
    cells[cells.findIndex(row => row.id === c.id)] = updated;
    return updated;
  };
  // Strict reading of C83-bis(1)(b)(3), also imported by (1-bis)(d),
  // and the express wording of (1-bis)(c): remove remainder-awarded seats.
  // Approved provisional I13-v1: only when those donors are exhausted,
  // rank positive-seat donors by their existing decimal, including integer
  // seats. Repeated removals are bounded by the surplus and available seats;
  // they do not consume or invent a remainder. Other chambers/stages stay strict.
  const looseDonors = input.chamber === 'camera' && input.norm === 'N18' && input.correction !== 'constituencies';
  const integerDonors = (ids: ReadonlySet<string>) => looseDonors
    ? cells.filter(c => ids.has(c.targetId) && c.allocationEligible && c.seats > 0 && c.decimalPart !== null) : [];
  const warnIntegerRemoval = (cell: TerritorialMatrixCell) => warnings.push({
    code: 'I13_INTEGER_SEAT_REMOVAL', severity: 'warning', electionKind: 'politiche',
    simulationId: input.simulationId, chamber: input.chamber, interpretationId: 'I13', lawReference: input.lawReference,
    message: `Provisional I13-v1: ${input.contextId}; remove an integer seat from ${cell.targetId} in ${cell.areaId} by ascending decimal part after eligible remainder-seat donors are exhausted (${cell.seats} -> ${cell.seats - 1}). Decimal value and remainder state preserved.`
  });

  // C83(h)/(i), C83-bis(1)(b), S17(4)(a): "non tiene conto ... parte frazionaria".
  // Calculate ALL integer parts before applying reached-target exclusions.
  // This is a simultaneous territorial allocation, independent of input order.
  for (const area of areas) {
    const indices: { raw: ExactFraction | null; applied: ExactFraction }[] = [];
    for (const t of targets) {
      const e = area.entries.find(e => e.id === t.id)!;
      if (e.eligible === false) { indices.push({ raw: null, applied: zero }); continue; }
      const q = input.indexQuotas?.find(q => q.id === t.id);
      if (q && t.seats > 0 && (!q.quota || q.quota.numerator === '0')) {
        return finish({ phase: 'initial', reason: 'zero-index-quota' });
      }
      if (q && t.seats === 0) indices.push({ raw: null, applied: zero });
      else if (q) {
        const raw = divideExact(exactFraction(e.votes), q.quota!);
        // C83(1-ter)(d), I05-v1; C83-bis(1-bis)(b) and S17(5)(b)
        // expressly truncate BOTH groups: "troncato alla sesta cifra decimale".
        indices.push({ raw, applied: truncateToSixDecimals(raw) });
      } else indices.push({ raw: null, applied: exactFraction(e.votes) });
    }
    const sum = indices.reduce((s, i) => addExact(s, i.applied), zero);
    const rawQuota = area.seats === 0 ? null : divideExact(sum, exactFraction(area.seats));
    const quota = rawQuota === null ? null : input.indexQuotas ? rawQuota : exactFraction(floorExact(rawQuota));
    quotas.push({ id: area.id, capacity: area.seats, rawQuota, quota });
    if (area.seats > 0 && sum.numerator === '0') return finish({ phase: 'initial', reason: 'zero-vote-total' });
    if (area.seats > 0 && quota!.numerator === '0') return finish({ phase: 'initial', reason: 'zero-quota' });
    const quotients = indices.map((i, n) => quota && area.entries.find(e => e.id === targets[n].id)!.eligible !== false
      ? divideExact(i.applied, quota) : null);
    const integers = quotients.map(q => q ? BigInt(floorExact(q)) : 0n);
    if (integers.reduce((s, n) => s + n, 0n) > BigInt(area.seats)) return finish({ phase: 'initial', reason: 'initial-overallocation' });
    targets.forEach((t, i) => cells.push({ id: cellId(area.id, t.id), areaId: area.id, targetId: t.id,
      allocationEligible: area.entries.find(e => e.id === t.id)!.eligible !== false,
      votes: area.entries.find(e => e.id === t.id)!.votes, rawIndex: indices[i].raw,
      index: input.indexQuotas ? indices[i].applied : null, quotient: quotients[i], integerSeats: Number(integers[i]),
      decimalPart: quotients[i] ? fractionalPart(quotients[i]) : null,
      remainderExcluded: false, remainderSeat: false, provisionalSeats: Number(integers[i]),
      remainderState: 'unused', seats: Number(integers[i]) }));
  }
  // Exclusion is based on the completed integer allocation across all areas,
  // before their parallel remainder allocations; no arbitrary area wins priority.
  const reached = new Set(targets.filter(t => total(t.id) >= t.seats).map(t => t.id));
  cells = cells.map(c => ({ ...c, remainderExcluded: !c.allocationEligible ||
    (input.correction !== 'college-groups' && reached.has(c.targetId)) }));
  // Preflight every residual pool before consuming any initial draw (I04).
  for (const area of areas) {
    const rows = cells.filter(c => c.areaId === area.id);
    const residual = area.seats - sumVoteCounts(rows.map(c => c.seats));
    if (residual > rows.filter(c => !c.remainderExcluded && c.decimalPart !== null).length) {
      return finish({ phase: 'remainders', reason: 'impossible-residual' });
    }
  }
  for (const area of areas) {
    const rows = cells.filter(c => c.areaId === area.id);
    const residual = area.seats - sumVoteCounts(rows.map(c => c.seats));
    for (let n = 0; n < residual; n++) {
      const pool = cells.filter(c => c.areaId === area.id && !c.remainderExcluded && c.remainderState === 'unused' && c.decimalPart !== null);
      const id = choose(pool.map(c => ({ id: c.id, values: [remainder(c), exactFraction(area.entries.find(e => e.id === c.targetId)!.tieVotes)] })),
        [desc('decimal-part'), desc(input.initialTieFigure)], `${area.id}/remainder-${n + 1}`, 'statutory');
      const c = cells.find(c => c.id === id)!;
      change(c, { remainderSeat: true, provisionalSeats: c.seats + 1, seats: c.seats + 1, remainderState: 'used' });
    }
  }

  if (independent) {
    // C83-bis(1)(b)(3): "i seggi attribuiti in eccesso sono sottratti ...;
    // alla lista cui sono stati attribuiti seggi in numero minore ... i seggi
    // sono assegnati ... [con] parte decimale ... non utilizzata".
    // Approved reading: subtraction and addition are independent, including
    // when an upstream C83(h)/(i) fallback changed this constituency's total.
    // Preserve single-use remainders; I13 separately permits integer removals.
    for (const direction of ['removal', 'addition'] as const) {
      let movement = 0;
      const pending = () => new Set(targets.filter(t => direction === 'removal'
        ? total(t.id) > t.seats : total(t.id) < t.seats).map(t => t.id));
      while (pending().size) {
        const ids = pending();
        let pool = cells.filter(c => ids.has(c.targetId) && (direction === 'removal'
          ? c.remainderSeat && c.remainderState === 'used' && c.seats > 0
          : c.remainderState === 'unused' && c.decimalPart !== null));
        const fallback = direction === 'removal' && !pool.length && looseDonors;
        if (fallback) pool = integerDonors(ids);
        if (!pool.length) return finish({ phase: 'correction', reason: direction === 'removal'
          ? 'no-eligible-donor-remainder' : 'no-eligible-recipient-remainder' });
        const id = choose(pool.map(c => ({ id: c.id, values: [remainder(c)] })),
          [direction === 'removal' ? asc('donor-decimal-part') : desc('recipient-decimal-part')],
          `${direction}-${++movement}`, 'I03');
        const cell = cells.find(c => c.id === id)!;
        const after = fallback ? cell.remainderState : direction === 'removal' ? 'consumed' : 'used';
        transfers.push({ fromCellId: direction === 'removal' ? id : null, toCellId: direction === 'addition' ? id : null,
          ...(fallback ? { interpretationId: 'I13' as const } : {}),
          route: direction, fromSeatsBefore: direction === 'removal' ? cell.seats : null,
          toSeatsBefore: direction === 'addition' ? cell.seats : null,
          remainders: [{ id, value: remainder(cell), before: cell.remainderState, after }],
          drawContextIds: resolutions.at(-1)!.draws.map(d => d.contextId) });
        if (fallback) warnIntegerRemoval(cell);
        change(cell, { seats: cell.seats + (direction === 'removal' ? -1 : 1), remainderState: after });
      }
    }
    return finish();
  }

  // C83(h), ninth and following sentences: largest surplus, then national
  // figure; donor's USED decimals ascending, recipient's UNUSED descending.
  // Try EVERY eligible same-area donor before the cross-area fallback.
  // C83-bis(1)(b)(3), S17(4)(c) select donor/recipient colleges independently.
  // C83-bis(1-bis)(c), S17(5)(c): "nei medesimi collegi" even if the other remainder is used.
  const surplusOrder: string[] = [];
  const surplusDrawContexts = new Map<string, readonly string[]>();
  let remaining = targets.filter(t => total(t.id) > t.seats);
  while (remaining.length) {
    const criteria = input.correction === 'constituencies' ? [desc('surplus-seats'), desc('national-electoral-figure')] : [];
    // College list corrections have no statutory cross-list priority. Rank
    // their actual donor decimals below, without inventing a national criterion.
    if (input.correction !== 'constituencies') break;
    const id = choose(remaining.map(t => ({ id: t.id, values: [exactFraction(total(t.id) - t.seats), exactFraction(t.votes)] })),
      criteria, `surplus-${surplusOrder.length + 1}`, 'I03');
    surplusOrder.push(id);
    surplusDrawContexts.set(id, resolutions.at(-1)!.draws.map(d => d.contextId));
    remaining = remaining.filter(t => t.id !== id);
  }
  let movement = 0;
  while (targets.some(t => total(t.id) > t.seats)) {
    const resolutionStart = resolutions.length;
    const deficits = new Set(targets.filter(t => total(t.id) < t.seats).map(t => t.id));
    const surplus = new Set(targets.filter(t => total(t.id) > t.seats).map(t => t.id));
    const ordered = surplusOrder.find(id => surplus.has(id));
    let donors = cells.filter(c => surplus.has(c.targetId) && (!ordered || c.targetId === ordered) &&
      c.remainderSeat && c.remainderState === 'used' && c.seats > 0);
    const fallback = !donors.length && looseDonors;
    if (fallback) donors = integerDonors(surplus).filter(d => input.correction !== 'college-groups' ||
      cells.some(r => r.areaId === d.areaId && r.allocationEligible && deficits.has(r.targetId)));
    const recipients = cells.filter(c => deficits.has(c.targetId) && c.remainderState === 'unused' && c.decimalPart !== null);
    if (input.correction === 'constituencies') {
      const local = donors.filter(d => recipients.some(r => r.areaId === d.areaId));
      if (local.length) donors = local;
    }
    if (!donors.length) return finish({ phase: 'correction', reason: 'no-eligible-donor-remainder' });
    if (input.correction !== 'college-groups' && !recipients.length) {
      return finish({ phase: 'correction', reason: 'no-eligible-recipient-remainder' });
    }
    const donorId = choose(donors.map(c => ({ id: c.id, values: [remainder(c)] })), [asc('donor-decimal-part')], `movement-${movement}/donor`, 'I03');
    const donor = cells.find(c => c.id === donorId)!;
    let pool = recipients;
    if (input.correction === 'constituencies' && recipients.some(r => r.areaId === donor.areaId)) {
      pool = recipients.filter(r => r.areaId === donor.areaId);
    } else if (input.correction === 'college-groups') {
      pool = cells.filter(c => c.allocationEligible && deficits.has(c.targetId) && c.areaId === donor.areaId);
    }
    if (!pool.length) return finish({ phase: 'correction', reason: 'no-eligible-recipient-remainder' });
    const localRecipient = donor.areaId === pool[0].areaId && pool.every(c => c.areaId === donor.areaId);
    const nationalRecipientCriterion = input.correction === 'constituencies' && localRecipient;
    const recipientCriteria = nationalRecipientCriterion
      ? [desc('recipient-decimal-part'), desc('national-electoral-figure')]
      : input.correction === 'college-groups' ? [] : [desc('recipient-decimal-part')];
    const recipientId = choose(pool.map(c => ({ id: c.id, values: input.correction === 'college-groups' ? [] :
      [remainder(c), ...(nationalRecipientCriterion ? [exactFraction(target(c.targetId).votes)] : [])] })),
      recipientCriteria, `movement-${movement}/recipient`, 'I03');
    const recipient = cells.find(c => c.id === recipientId)!;
    const donorAfter = fallback ? donor.remainderState : 'consumed';
    transfers.push({ fromCellId: donor.id, toCellId: recipient.id,
      ...(fallback ? { interpretationId: 'I13' as const } : {}),
      route: donor.areaId === recipient.areaId ? 'same-area' : 'cross-area',
      fromSeatsBefore: donor.seats, toSeatsBefore: recipient.seats,
      remainders: [{ id: donor.id, value: remainder(donor), before: donor.remainderState, after: donorAfter },
        { id: recipient.id, value: recipient.decimalPart ?? zero, before: recipient.remainderState, after: 'used' }],
      drawContextIds: [...(surplusDrawContexts.get(donor.targetId) ?? []),
        ...resolutions.slice(resolutionStart).flatMap(r => r.draws.map(d => d.contextId))] });
    if (fallback) warnIntegerRemoval(donor);
    change(donor, { seats: donor.seats - 1, remainderState: donorAfter });
    change(recipient, { seats: recipient.seats + 1, remainderState: 'used' });
    movement++;
  }
  if (targets.some(t => total(t.id) !== t.seats)) return finish({ phase: 'correction', reason: 'unreconciled-targets' });
  return finish();
}

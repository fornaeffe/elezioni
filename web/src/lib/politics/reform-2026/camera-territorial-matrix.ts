import { addExact, compareExact, divideExact, exactFraction, floorExact, fractionalPart, truncateToSixDecimals } from './exact';
import { sumVoteCounts } from './figures';
import type { DrawTrace, ExactFraction, NormId, ReformWarning, ScrutinyExecutionContext } from './result';
import { resolveTie, type TieCriterion, type TieResolution } from './ties';

/** Internal Camera matrix engine. Callers select the actual statutory scope,
 * electoral figures and correction route; this is not a Senate algorithm. */
export interface CameraMatrixInput {
  readonly contextId: string;
  readonly simulationId: number;
  readonly norm: NormId;
  readonly lawReference: string;
  readonly targets: readonly { readonly id: string; readonly seats: number; readonly votes: number }[];
  readonly areas: readonly {
    readonly id: string;
    readonly seats: number;
    readonly entries: readonly { readonly id: string; readonly votes: number; readonly tieVotes: number }[];
  }[];
  /** C83(h)/(i), C83-bis(b)(3), or C83-bis(1-bis)(c), respectively. */
  readonly correction: 'constituencies' | 'colleges' | 'college-groups';
  readonly initialTieFigure: 'national-electoral-figure' | 'constituency-electoral-figure';
  /** Absent: integer local quota. Present: truncated indices, then normalization.
   * null marks an inactive zero-target group; its quota/index is never divided. */
  readonly indexQuotas?: readonly { readonly id: string; readonly quota: ExactFraction | null }[];
}

export interface CameraMatrixCell {
  readonly id: string;
  readonly areaId: string;
  readonly targetId: string;
  readonly votes: number;
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
export interface CameraMatrixTransfer {
  readonly fromCellId: string;
  readonly toCellId: string;
  readonly route: 'same-area' | 'cross-area';
  readonly fromSeatsBefore: number;
  readonly toSeatsBefore: number;
  readonly remainders: readonly { readonly id: string; readonly value: ExactFraction;
    readonly before: 'unused' | 'used' | 'consumed'; readonly after: 'used' | 'consumed' }[];
  readonly drawContextIds: readonly string[];
}
interface MatrixTrace {
  readonly norm: NormId;
  readonly lawReference: string;
  readonly contextId: string;
  readonly initialTieFigure: CameraMatrixInput['initialTieFigure'];
  readonly correction: CameraMatrixInput['correction'];
  readonly indexQuotas: CameraMatrixInput['indexQuotas'] | null;
  readonly areas: readonly { readonly id: string; readonly capacity: number; readonly rawQuota: ExactFraction | null;
    readonly quota: ExactFraction | null; readonly finalSeats: number }[];
  readonly targets: CameraMatrixInput['targets'];
  readonly cells: readonly CameraMatrixCell[];
  readonly transfers: readonly CameraMatrixTransfer[];
  readonly resolutions: readonly TieResolution[];
  readonly draws: readonly DrawTrace[];
  readonly warnings: readonly ReformWarning[];
}
export type CameraMatrixResult = MatrixTrace & (
  | { readonly status: 'complete' }
  | { readonly status: 'non-computable'; readonly phase: 'initial' | 'remainders' | 'correction'; readonly reason: string }
);

const asc = (name: string): TieCriterion => ({ name, direction: 'ascending' });
const desc = (name: string): TieCriterion => ({ name, direction: 'descending' });
const zero = exactFraction(0);
const cellId = (area: string, target: string) => JSON.stringify([area, target]);

export function allocateCameraMatrix(input: CameraMatrixInput, execution: ScrutinyExecutionContext): CameraMatrixResult {
  const targets = [...input.targets].sort((a, b) => a.id < b.id ? -1 : a.id > b.id ? 1 : 0);
  const areas = [...input.areas].sort((a, b) => a.id < b.id ? -1 : a.id > b.id ? 1 : 0);
  const unique = (ids: readonly string[]) => ids.every(Boolean) && new Set(ids).size === ids.length;
  if (!unique(targets.map(t => t.id)) || !unique(areas.map(a => a.id)) ||
      !Number.isSafeInteger(input.simulationId) || input.simulationId < 0) throw new RangeError('Invalid Camera matrix IDs');
  targets.forEach(t => sumVoteCounts([t.seats, t.votes]));
  if (sumVoteCounts(targets.map(t => t.seats)) !== sumVoteCounts(areas.map(a => a.seats))) {
    throw new RangeError('Camera matrix capacities must sum to its targets');
  }
  for (const area of areas) {
    if (!unique(area.entries.map(e => e.id)) || area.entries.length !== targets.length ||
        targets.some(t => !area.entries.some(e => e.id === t.id))) throw new RangeError('Supply every matrix figure once');
    area.entries.forEach(e => sumVoteCounts([e.votes, e.tieVotes]));
  }
  if (input.indexQuotas && (!unique(input.indexQuotas.map(q => q.id)) || input.indexQuotas.length !== targets.length ||
      targets.some(t => !input.indexQuotas!.some(q => q.id === t.id)))) throw new RangeError('Supply every index quota once');

  let cells: CameraMatrixCell[] = [];
  const quotas: { id: string; capacity: number; rawQuota: ExactFraction | null; quota: ExactFraction | null }[] = [];
  const transfers: CameraMatrixTransfer[] = [];
  const resolutions: TieResolution[] = [];
  const finish = (failure?: { phase: 'initial' | 'remainders' | 'correction'; reason: string }): CameraMatrixResult => structuredClone({
    norm: input.norm, lawReference: input.lawReference, contextId: input.contextId,
    initialTieFigure: input.initialTieFigure, correction: input.correction,
    indexQuotas: input.indexQuotas ? [...input.indexQuotas].sort((a, b) => a.id < b.id ? -1 : a.id > b.id ? 1 : 0) : null,
    targets, cells, transfers, resolutions,
    areas: quotas.map(a => ({ ...a, finalSeats: sumVoteCounts(cells.filter(c => c.areaId === a.id).map(c => c.seats)) })),
    draws: resolutions.flatMap(r => r.draws), warnings: resolutions.flatMap(r => r.warnings),
    ...(failure ? { status: 'non-computable' as const, ...failure } : { status: 'complete' as const })
  });
  const choose = (options: readonly { id: string; values: readonly ExactFraction[] }[], criteria: readonly TieCriterion[],
    suffix: string, rule: 'statutory' | 'I03') => {
    const r = resolveTie(options, criteria, { simulationId: input.simulationId, request: {
      contextId: `${input.contextId}/${suffix}`, chamber: 'camera', norm: input.norm, lawReference: input.lawReference, rule
    } }, execution);
    resolutions.push(r);
    return r.chosenId;
  };
  const total = (id: string) => sumVoteCounts(cells.filter(c => c.targetId === id).map(c => c.seats));
  const target = (id: string) => targets.find(t => t.id === id)!;
  const remainder = (c: CameraMatrixCell) => c.decimalPart!;
  const change = (c: CameraMatrixCell, patch: Partial<CameraMatrixCell>) => {
    const updated = { ...c, ...patch };
    cells[cells.findIndex(row => row.id === c.id)] = updated;
    return updated;
  };

  // C83(h)/(i), C83-bis(1)(b): "non tiene conto ... parte frazionaria".
  // Calculate ALL integer parts before applying reached-target exclusions.
  // This is a simultaneous territorial allocation, independent of input order.
  for (const area of areas) {
    const indices: { raw: ExactFraction | null; applied: ExactFraction }[] = [];
    for (const t of targets) {
      const e = area.entries.find(e => e.id === t.id)!;
      const q = input.indexQuotas?.find(q => q.id === t.id);
      if (q && t.seats > 0 && (!q.quota || q.quota.numerator === '0')) {
        return finish({ phase: 'initial', reason: 'zero-index-quota' });
      }
      if (q && t.seats === 0) indices.push({ raw: null, applied: zero });
      else if (q) {
        const raw = divideExact(exactFraction(e.votes), q.quota!);
        // C83(1-ter)(d), I05-v1; C83-bis(1-bis)(b) expressly truncates BOTH groups.
        indices.push({ raw, applied: truncateToSixDecimals(raw) });
      } else indices.push({ raw: null, applied: exactFraction(e.votes) });
    }
    const sum = indices.reduce((s, i) => addExact(s, i.applied), zero);
    const rawQuota = area.seats === 0 ? null : divideExact(sum, exactFraction(area.seats));
    const quota = rawQuota === null ? null : input.indexQuotas ? rawQuota : exactFraction(floorExact(rawQuota));
    quotas.push({ id: area.id, capacity: area.seats, rawQuota, quota });
    if (area.seats > 0 && sum.numerator === '0') return finish({ phase: 'initial', reason: 'zero-vote-total' });
    if (area.seats > 0 && quota!.numerator === '0') return finish({ phase: 'initial', reason: 'zero-quota' });
    const quotients = indices.map(i => quota ? divideExact(i.applied, quota) : null);
    const integers = quotients.map(q => q ? BigInt(floorExact(q)) : 0n);
    if (integers.reduce((s, n) => s + n, 0n) > BigInt(area.seats)) return finish({ phase: 'initial', reason: 'initial-overallocation' });
    targets.forEach((t, i) => cells.push({ id: cellId(area.id, t.id), areaId: area.id, targetId: t.id,
      votes: area.entries.find(e => e.id === t.id)!.votes, rawIndex: indices[i].raw,
      index: input.indexQuotas ? indices[i].applied : null, quotient: quotients[i], integerSeats: Number(integers[i]),
      decimalPart: quotients[i] ? fractionalPart(quotients[i]) : null,
      remainderExcluded: false, remainderSeat: false, provisionalSeats: Number(integers[i]),
      remainderState: 'unused', seats: Number(integers[i]) }));
  }
  // Exclusion is based on the completed integer allocation across all areas,
  // before their parallel remainder allocations; no arbitrary area wins priority.
  const reached = new Set(targets.filter(t => total(t.id) >= t.seats).map(t => t.id));
  cells = cells.map(c => ({ ...c, remainderExcluded: input.correction !== 'college-groups' && reached.has(c.targetId) }));
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

  // C83(h), ninth and following sentences: largest surplus, then national
  // figure; donor's USED decimals ascending, recipient's UNUSED descending.
  // Try EVERY eligible same-area donor before the cross-area fallback.
  // C83-bis(1)(b)(3) selects donor/recipient colleges independently.
  // C83-bis(1-bis)(c): "nei medesimi collegi" even if the other remainder is used.
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
      pool = cells.filter(c => deficits.has(c.targetId) && c.areaId === donor.areaId);
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
    transfers.push({ fromCellId: donor.id, toCellId: recipient.id,
      route: donor.areaId === recipient.areaId ? 'same-area' : 'cross-area',
      fromSeatsBefore: donor.seats, toSeatsBefore: recipient.seats,
      remainders: [{ id: donor.id, value: remainder(donor), before: donor.remainderState, after: 'consumed' },
        { id: recipient.id, value: recipient.decimalPart ?? zero, before: recipient.remainderState, after: 'used' }],
      drawContextIds: [...(surplusDrawContexts.get(donor.targetId) ?? []),
        ...resolutions.slice(resolutionStart).flatMap(r => r.draws.map(d => d.contextId))] });
    change(donor, { seats: donor.seats - 1, remainderState: 'consumed' });
    change(recipient, { seats: recipient.seats + 1, remainderState: 'used' });
    movement++;
  }
  if (targets.some(t => total(t.id) !== t.seats)) return finish({ phase: 'correction', reason: 'unreconciled-targets' });
  return finish();
}

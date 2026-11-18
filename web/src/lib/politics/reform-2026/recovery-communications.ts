import type { CameraConstituencyResult, CameraConstituencyListResult, CameraCollegeResult } from './camera-territorial';
import type { ChamberInput, Subject } from './input';
import type { RecoveryInput, RecoveryRemainder } from './recovery';
import type { AdmissionResult } from './admission';
import type { SenateRegionalAllocationResult } from './senate-regional';
import type { SenateRegionalListResult, SenateCollegeResult } from './senate-territorial';
import type { CameraTaaAllocationResult } from './taa-allocation';
import type { TerritorialMatrixResult } from './territorial-matrix';

const decodeSubject = (id: string): Subject => { const [kind, value] = JSON.parse(id); return { kind, id: value }; };

/** Preserve the source cell ID, exact decimal and FINAL correction state.
 * Group indices are not coalition/list quotients. Missing quotients stay absent;
 * recovery will report missing evidence rather than use an invented vote ratio. */
export interface RecoveryAllocationCommunications {
  constituencies?: CameraConstituencyResult;
  lists?: CameraConstituencyListResult | SenateRegionalListResult;
  colleges: CameraCollegeResult | SenateCollegeResult;
  regional?: SenateRegionalAllocationResult;
  taa?: CameraTaaAllocationResult;
}

/** Carry admission and final territorial list targets together. A missing
 * target is an inconsistent communication, not an inferred zero entitlement. */
export function assembleRecoveryEligibility(chamber: ChamberInput, admission: AdmissionResult,
  stages: RecoveryAllocationCommunications): RecoveryInput['eligibleLists'] {
  if (admission.status !== 'complete' || stages.colleges.status !== 'complete' || stages.lists?.status !== 'complete' ||
      stages.taa?.status === 'non-computable') return [];
  const targets = stages.lists.targets;
  return chamber.territories.flatMap(t => {
    if (t.kind === 'vda' || t.kind === 'taa' && chamber.chamber === 'senato') return [];
    return admission.lists.filter(l => (l.admitted || admission.taaLocalExceptions.some(e => e.territoryId === t.id && e.listId === l.listId)) &&
      chamber.ordinarySlates.some(s => s.listId === l.listId && chamber.colleges.find(p => p.id === s.collegeId)!.territoryId === t.id))
      .map(l => {
        const ordinarySeats = t.kind === 'taa' ? stages.taa && stages.taa.status !== 'non-computable'
          ? stages.taa.listEntitlements.find(r => r.listId === l.listId)?.seats : undefined :
          targets.find(r => r.listId === l.listId && ('territoryId' in r ? r.territoryId === t.id : r.regionId === t.regionId))?.ordinarySeats;
        if (ordinarySeats === undefined) throw new RangeError('Missing final territorial list target for recovery');
        return { territoryId: t.id, listId: l.listId, ordinarySeats };
      });
  });
}

export function assembleRecoveryRemainders(chamber: ChamberInput, stages: RecoveryAllocationCommunications): readonly RecoveryRemainder[] {
  const rows = new Map<string, RecoveryRemainder>();
  const put = (r: RecoveryRemainder) => rows.set(JSON.stringify([r.territoryId, r.collegeId, r.subject]), r);
  function matrices(ms: readonly TerritorialMatrixResult[], subjects: boolean, colleges: boolean) {
    for (const m of ms) {
      if (m.status !== 'complete' || m.correction === 'college-groups') continue;
      for (const cell of m.cells) {
        const subject: Subject = subjects ? decodeSubject(cell.targetId) : { kind: 'list', id: cell.targetId };
        const territoryId = colleges ? chamber.colleges.find(c => c.id === cell.areaId)!.territoryId : cell.areaId;
        put({ id: JSON.stringify([m.contextId, cell.id]), territoryId, collegeId: colleges ? cell.areaId : null,
          subject, value: cell.decimalPart, state: cell.remainderState });
      }
    }
  }
  matrices(stages.constituencies?.matrices ?? [], true, false);
  if (stages.lists?.chamber === 'camera') matrices(stages.lists.matrices, false, false);
  const regional = stages.regional;
  if (regional?.status === 'complete') for (const region of regional.regions) {
    const territoryId = chamber.territories.find(t => t.regionId === region.regionId && t.kind === 'ordinary')!.id;
    for (const a of region.allocation.assignments) {
      const subject = decodeSubject(a.id);
      const final = regional.cap?.remainders.find(r => r.regionId === region.regionId && JSON.stringify(r.subject) === JSON.stringify(subject));
      put({ id: JSON.stringify(['N19', regional.simulationId, region.regionId, a.id]), territoryId, collegeId: null,
        subject, value: a.decimalPart, state: final?.state ?? a.remainderState });
    }
  }
  if (stages.lists?.chamber === 'senato') for (const group of stages.lists.coalitionAllocations) {
    const territoryId = chamber.territories.find(t => t.regionId === group.regionId && t.kind === 'ordinary')!.id;
    if (group.allocation.status !== 'complete') continue;
    for (const a of group.allocation.assignments) put({ id: JSON.stringify(['N21', group.regionId, group.coalitionId, a.id]),
      territoryId, collegeId: null, subject: { kind: 'list', id: a.id }, value: a.decimalPart, state: a.remainderState });
  }
  matrices(stages.colleges.matrices, false, true);
  // S17(3) assigns regional targets directly to the sole college. Its regional
  // list quotient is already known (N19 standalone, N21 coalition member).
  // Carry the existing value/state downstream, without a second allocation.
  if (chamber.chamber === 'senato' && stages.colleges.status === 'complete') {
    for (const territory of chamber.territories.filter(t => t.kind === 'ordinary')) {
      const colleges = chamber.colleges.filter(c => c.territoryId === territory.id && c.kind === 'ordinary');
      if (colleges.length !== 1) continue;
      const regionalLists = [...rows.values()].filter(r => r.territoryId === territory.id && r.collegeId === null && r.subject.kind === 'list' &&
        stages.lists?.status === 'complete' && stages.lists.targets.some(t => 'regionId' in t && t.regionId === territory.regionId &&
          t.listId === r.subject.id && t.ordinarySeats > 0));
      for (const r of regionalLists) put({ ...r, id: JSON.stringify([r.id, 'single-college', colleges[0].id]),
        sourceId: r.id, collegeId: colleges[0].id });
    }
  }
  const taa = stages.taa;
  if (taa && taa.status !== 'non-computable') {
    for (const a of taa.subjectAllocation?.assignments ?? []) {
      const subject = decodeSubject(a.id);
      put({ id: JSON.stringify(['N11', taa.collegeId, a.id]), territoryId: taa.territoryId,
        collegeId: null, subject, value: a.decimalPart, state: a.remainderState });
      if (subject.kind !== 'list' || taa.listEntitlements.some(t => t.listId === subject.id && t.seats > 0)) {
        put({ id: JSON.stringify(['N11', taa.collegeId, 'college', a.id]), territoryId: taa.territoryId,
          collegeId: taa.collegeId, subject, value: a.decimalPart, state: a.remainderState });
      }
    }
    for (const g of taa.coalitionAllocations) for (const a of g.allocation.assignments) {
      for (const collegeId of [null, taa.collegeId]) {
        if (collegeId !== null && !taa.listEntitlements.some(t => t.listId === a.id && t.seats > 0)) continue;
        put({ id: JSON.stringify(['N11', taa.collegeId, g.coalitionId, collegeId, a.id]),
          territoryId: taa.territoryId, collegeId, subject: { kind: 'list', id: a.id }, value: a.decimalPart, state: a.remainderState });
      }
    }
  }
  return structuredClone([...rows.values()].sort((a, b) => a.id < b.id ? -1 : a.id > b.id ? 1 : 0));
}

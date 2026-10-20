import type { ChamberInput, Subject } from './input';
import type { SeatLocation, SeatTransferTrace } from './result';
import type { ReformChamberExecution } from './scrutiny';

/** Located allocation movements only. National cap arithmetic and temporary
 * majority/minority group movements retain their original, richer stage traces
 * in the chamber communication; they are not fictitious candidate seat IDs. */
export function allocationTransfers(chambers: readonly ChamberInput[], executions: readonly ReformChamberExecution[]): readonly SeatTransferTrace[] {
  const transfers: SeatTransferTrace[] = [];
  for (const s of executions) {
    const chamber = chambers.find(c => c.chamber === s.chamber)!;
    const stages = s.cameraTerritorial ? [s.cameraTerritorial.constituencies, s.cameraTerritorial.lists, s.cameraTerritorial.colleges] :
      s.senateTerritorial ? [s.senateTerritorial.lists, s.senateTerritorial.colleges] : [];
    for (const stage of stages) for (const m of stage.matrices) {
      if (m.correction === 'college-groups') continue;
      const location = (id: string | null): SeatLocation | null => {
        if (id === null) return null;
        const cell = m.cells.find(c => c.id === id)!;
        const college = m.correction === 'colleges' || m.correction === 'independent-colleges'
          ? chamber.colleges.find(c => c.id === cell.areaId) : undefined;
        const isSubject = m.norm === 'N15' || m.norm === 'N17';
        const [kind, subjectId] = isSubject ? JSON.parse(cell.targetId) : ['list', cell.targetId];
        const member = !isSubject && s.admission?.status === 'complete' ? s.admission.lists.find(l => l.listId === cell.targetId) : undefined;
        const subject: Subject = member?.admittedAs === 'coalition-member' && member.coalitionId ?
          { kind: 'coalition', id: member.coalitionId } : { kind, id: subjectId };
        return { chamber: s.chamber, territoryId: college?.territoryId ?? cell.areaId, collegeId: college?.id ?? null,
          subject, listId: isSubject ? subject.kind === 'list' ? subject.id : null : cell.targetId };
      };
      m.transfers.forEach((t, i) => transfers.push({ seatId: JSON.stringify(['allocation', m.contextId, i]), chamber: s.chamber,
        norm: m.norm, lawReference: m.lawReference, interpretationId: t.interpretationId ?? (m.norm === 'N17' ? 'I05' : null),
        from: location(t.fromCellId), to: location(t.toCellId), remainders: t.remainders, drawContextIds: t.drawContextIds }));
    }
    if (s.regional?.cap) for (const move of s.regional.cap.movements) {
      const territoryId = chamber.territories.find(t => t.regionId === move.regionId)!.id;
      const majority = s.regional.cap.check.subject;
      const location = (subject: Subject): SeatLocation => ({ chamber: s.chamber, territoryId, collegeId: null,
        subject, listId: subject.kind === 'list' ? subject.id : null });
      transfers.push({ seatId: JSON.stringify(['N20', s.simulationId, move.regionId, move.sequence]), chamber: s.chamber,
        norm: 'N20', lawReference: 'S16-bis(1-ter)', interpretationId: 'I06', from: location(majority), to: location(move.recipient.subject),
        remainders: [majority, move.recipient.subject].flatMap((subject, i) => {
          const r = s.regional!.cap!.remainders.find(r => r.regionId === move.regionId && r.subject.kind === subject.kind && r.subject.id === subject.id);
          const step = i === 0 ? move.removal : move.recipient;
          return r?.remainder ? [{ id: JSON.stringify(['N19', s.simulationId, move.regionId, JSON.stringify([subject.kind, subject.id])]), value: r.remainder,
            before: step.remainderBefore, after: step.remainderAfter }] : [];
        }), drawContextIds: [...move.removal.resolution.draws, ...move.recipient.resolution.draws].map(d => d.contextId) });
    }
  }
  return transfers;
}

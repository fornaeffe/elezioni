import type { ChamberInput, Provenance, ReformScrutinyInput } from './input';
import type { ReformScrutinyResult, ReformWarning } from './result';
import type { ReformChamberExecution } from './scrutiny';
import { territorialEligibility } from './territorial-eligibility';

export function scrutinyMetadata(input: ReformScrutinyInput, renewed: readonly ChamberInput[],
  executions: readonly ReformChamberExecution[], warnings: ReformWarning[]) {
  const conditions: ReformScrutinyResult['conditions'][number][] = [];
  const provenance = new Map<string, Provenance>();
  const policies = new Map<ReformScrutinyResult['interpretations'][number]['id'], string>();
  const decisions: ReformScrutinyResult['subjectDecisions'][number][] = [];
  const add = (p: Provenance) => provenance.set(JSON.stringify(p), p);
  for (const c of renewed) {
    for (const row of [...c.ordinaryVotes, ...c.specialVotes]) {
      add(row.votes.provenance);
      if (row.votes.kind === 'attributed') conditions.push({ kind: 'unknown-ballot-composition', chamber: c.chamber,
        affectedIds: [row.collegeId], reference: 'Final attributed totals; direct/bonus-only composition is unspecified.' });
    }
    for (const s of c.ordinarySlates) {
      if (s.preferences.kind === 'missing') conditions.push({ kind: 'missing-preferences', chamber: c.chamber,
        affectedIds: [s.collegeId, s.listId], reference: s.preferences.reason });
      else {
        add(s.preferences.provenance);
        if (s.preferences.provenance.kind === 'generated') conditions.push({ kind: 'synthetic-preferences', chamber: c.chamber,
          affectedIds: [s.collegeId, s.listId], reference: JSON.stringify(s.preferences.provenance) });
      }
    }
    if (c.seatTable.kind === 'provisional-census') {
      policies.set('I08', 'I08-v1');
      conditions.push({ kind: 'provisional-seats', chamber: c.chamber, affectedIds: c.territories.map(t => t.id), reference: JSON.stringify(c.seatTable) });
      warnings.push({ code: 'I08_PROVISIONAL_SEATS', electionKind: 'politiche', simulationId: input.simulationId, chamber: c.chamber,
        severity: 'warning', interpretationId: 'I08', message: `Census-based provisional seat table ${c.seatTable.version}; this is not an official decree allocation.` });
    }
    const s = executions.find(s => s.chamber === c.chamber)!;
    if (s.admission?.status === 'complete') {
      const coverage = territorialEligibility(c);
      if (c.territories.some(t => t.kind === 'ordinary' && s.admission?.status === 'complete' &&
          s.admission.lists.some(l => l.admitted && !coverage.list(t.id, l.listId)))) {
        warnings.push({ code: 'I12_TERRITORIAL_ELIGIBILITY', electionKind: 'politiche', simulationId: input.simulationId,
          chamber: c.chamber, severity: 'warning', interpretationId: 'I12',
          message: 'Provisional I12-v1: national admission does not permit ordinary seat allocation to a list absent from the constituency/region. Eligibility uses filed slates, independently of zero votes or zero remainders.' });
      }
      for (const l of s.admission.lists) decisions.push({ norm: s.admission.norm, chamber: c.chamber, subject: { kind: 'list', id: l.listId },
        decision: l.admitted ? 'admitted' : 'excluded', reason: l.reasons.join(','), lawReference: l.lawReference });
      for (const g of s.admission.coalitions) decisions.push({ norm: s.admission.norm, chamber: c.chamber, subject: { kind: 'coalition', id: g.coalitionId },
        decision: g.admitted ? 'admitted' : 'excluded', reason: g.reasons.join(','), lawReference: g.lawReference });
    }
    for (const p of s.cameraTerritorial?.constituencies.interpretations ?? []) policies.set(p.id, p.version);
    for (const p of s.regional?.cap?.interpretations ?? []) policies.set(p.id, p.version);
    if (c.chamber === 'senato' && s.recovery) policies.set('I07', 'approved-2026-10-16');
    for (const p of s.recovery?.interpretations ?? []) policies.set(p.id, p.version);
  }
  if (input.chamberModel.kind === 'generated') conditions.push({ kind: 'modelled-chamber-votes', chamber: null,
    affectedIds: [], reference: JSON.stringify(input.chamberModel) });
  // These policies govern all accepted inputs, including candidature validation.
  policies.set('I01', 'I01-v1'); policies.set('I04', 'I04-v1'); policies.set('I09', 'I09-v1');
  policies.set('I12', 'I12-v1');
  if (renewed.some(c => c.chamber === 'senato')) policies.set('I02', 'I02-v1');
  for (const w of warnings) if (w.interpretationId) policies.set(w.interpretationId,
    w.interpretationId === 'I07' ? 'approved-2026-10-16' :
      w.interpretationId === 'I14' || w.interpretationId === 'I15' ? `${w.interpretationId}-v2` : `${w.interpretationId}-v1`);
  return { conditions, provenance: [...provenance.values()], subjectDecisions: decisions,
    interpretations: [...policies].sort(([a], [b]) => a < b ? -1 : 1).map(([id, version]) => ({ id, version })) };
}

import type { NormalizedPollQuestion, PollEntityCatalog } from '../../src/lib/polls/types';
import { estimatePollReference } from '../../src/lib/polls/forecast';
import type { PollReferences, ReformGenerationScenario, ReformGenerationSnapshot } from '../../src/lib/politics/reform-2026/generation-types';
import { requireReformScenario } from '../../src/lib/politics/reform-2026/generation-scenario';
import { deriveCommonReferences, preparePairedVoteReferences, renewedChamberIds } from '../../src/lib/politics/reform-2026/paired-vote-generation';
import { reformPresentation } from '../../src/lib/politics/reform-2026/presentation-metadata';
import { sha256 } from './sources';

export interface PollCalibrationReceipt { version: string; accepted: boolean; reasons: string[]; }

export function createPollInformedScenario(value: ReformGenerationScenario, snapshot: ReformGenerationSnapshot,
  archive: { version: string; entityCatalog: PollEntityCatalog; questions: NormalizedPollQuestion[] },
  asOf = value.projectionDate, conditionalLabel: string | null = null, calibration?: PollCalibrationReceipt) {
  if (calibration?.accepted) throw new Error('Accepted calibration requires reviewed scenario-level correction and full-generator interval integration');
  const scenario = requireReformScenario(value, snapshot);
  if (scenario.pollReferences) { delete scenario.pollReferences; scenario.schemaVersion = 2; }
  scenario.presentation ??= reformPresentation(scenario);
  if (asOf < snapshot.historicalDate || asOf > scenario.electionDate) throw new Error('Poll date is outside historical/election interval');
  if (conditionalLabel !== null && !archive.questions.some(q => q.ballot.label === conditionalLabel)) throw new Error('Unknown conditional ballot');
  const historical = structuredClone(scenario); delete historical.pollReferences;
  historical.schemaVersion = 2; historical.shares = []; historical.abstention = [];
  const prepared = preparePairedVoteReferences(historical, snapshot);
  const contexts = renewedChamberIds(scenario).map(chamber => {
    const colleges = new Set(snapshot.seats.geography.colleges.filter(c => c.chamber === chamber && c.kind === 'ordinary').map(c => c.id));
    const codes = new Set(snapshot.memberships.filter(m => m.chamber === chamber && colleges.has(m.collegeId)).map(m => m.municipalityCode));
    const units = prepared.units.filter(u => codes.has(u.municipality.code));
    const eligible = new Set(scenario.coverage.filter(c => c.chamber === chamber && colleges.has(c.collegeId)).flatMap(c => c.listIds));
    const total = units.reduce((n, u) => n + u.municipality[chamber].electors * (1 - u[chamber].abstention), 0);
    const lists = scenario.lists[chamber].filter(l => eligible.has(l.id)).map(l => ({ id: l.id, name: l.name,
      baselineShare: total ? units.reduce((n, u) => n + u.municipality[chamber].electors * (1 - u[chamber].abstention) * (u[chamber].shares.get(l.id) ?? 0), 0) / total : 0 }));
    return { chamber, lists, historicalLists: snapshot.historicalLists, correspondences: scenario.correspondences.filter(c => c.chamber === chamber && eligible.has(c.futureListId) && c.factor > 0) };
  });
  // One common political anchor where the same list exists in both chambers.
  const estimates = contexts.map(context => ({ chamber: context.chamber, ...estimatePollReference(archive.questions, archive.entityCatalog,
    { ...context, lists: context.lists.map(l => ({ ...l, baselineShare: contexts.reduce((n, c) => n + (c.lists.find(p => p.id === l.id)?.baselineShare ?? l.baselineShare), 0) / contexts.length })) }, asOf, conditionalLabel) }));
  const rows: PollReferences['rows'] = [];
  for (const estimate of estimates.filter(e => e.applied)) {
    const profile = snapshot.models.chambers.profiles.find(p => p.id === scenario.chamberProfileId)!;
    const derived = estimate.rows.map(r => {
      const baseline = (ch: string) => contexts.find(c => c.chamber === ch)?.lists.find(l => l.id === r.listId)?.baselineShare ?? r.share;
      const share = deriveCommonReferences(r.share, baseline('camera'), baseline('senato'), profile.gapMultiplier)[estimate.chamber];
      const prior = estimate.evidence[0].reconciliation.territorialPriors.find(p => p.listId === r.listId)!;
      return { chamber: estimate.chamber, listId: r.listId, share, sigma: r.sigma, donorHistoricalListIds: prior.historicalListIds };
    });
    const total = derived.reduce((n, r) => n + r.share, 0);
    rows.push(...derived.map(r => ({ ...r, share: r.share / total })));
  }
  const calibrationNotes = calibration ? [`Historical calibration ${calibration.version}: corrections withheld. ${calibration.reasons.join(' ')}`] : [];
  const body = { archiveVersion: archive.version, snapshotVersion: snapshot.version, modelVersion: snapshot.models.version, asOf, estimates,
    ...(calibration ? { calibration } : {}) };
  const version = `poll-reference-v1-${sha256(JSON.stringify(body)).slice(0, 12)}`;
  if (rows.length) {
    scenario.schemaVersion = 3; scenario.projectionDate = asOf;
    scenario.pollReferences = { version, archiveVersion: archive.version, policyVersion: 'poll-reference-v1', date: asOf,
      sourceQuestionIds: [...new Set(estimates.flatMap(e => e.evidence.map(p => p.questionId)))],
      assumptions: [...estimates[0].assumptions, ...calibrationNotes], rows };
  }
  return { scenario: requireReformScenario(scenario, snapshot), report: { ...body, version, applied: rows.length > 0 } };
}

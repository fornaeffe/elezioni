import type { NormalizedPollQuestion, PollEntityCatalog, PollReconciliationContext } from './types';
import { reconcilePoll } from './reconciliation';

/** Versioned central convention. Historical calibration evaluates corrections;
 * rejected/unsupported candidates never silently change these assumptions. */
export const POLL_POLICY = { version: 'poll-reference-v1', halfLifeDays: 14, maximumAgeDays: 60,
  designEffect: 2, maximumEffectiveSample: 600, unknownDecidedFraction: 0.5,
  historicalPseudoSample: 200, sharedErrorLogitSigma: 0.08 } as const;
export function estimatePollReference(questions: NormalizedPollQuestion[], entities: PollEntityCatalog,
  context: PollReconciliationContext, asOf: string, conditionalLabel: string | null = null) {
  if (!/^\d{4}-\d{2}-\d{2}$/.test(asOf) || (!Number.isFinite(Date.parse(asOf)) || new Date(asOf).toISOString().slice(0, 10) !== asOf)) throw new Error('Invalid poll reference date');
  const day = (d: string) => Date.parse(d) / 86400000;
  const eligible = questions.filter(q => q.source.publicationDate <= asOf && q.source.fieldworkEnd <= asOf &&
    day(asOf) - day(q.source.fieldworkEnd) <= POLL_POLICY.maximumAgeDays &&
    (q.ballot.kind === 'generic' || q.ballot.label === conditionalLabel));
  // Explicit conditional selection replaces generic questions from that house.
  const selected = [...new Set(eligible.map(q => q.source.pollster))].sort().map(house => eligible.filter(q => q.source.pollster === house)
    .sort((a, b) => Number(b.ballot.kind === 'conditional') - Number(a.ballot.kind === 'conditional') ||
      b.source.fieldworkEnd.localeCompare(a.source.fieldworkEnd) || a.id.localeCompare(b.id))[0]);
  const baselineTotal = context.lists.reduce((n, l) => n + (l.baselineShare ?? 0), 0);
  const baseline = context.lists.map(l => ({ listId: l.id, share: baselineTotal ? (l.baselineShare ?? 0) / baselineTotal : 1 / context.lists.length }));
  const evidence = selected.map(q => {
    const nonresponse = q.answers.find(a => a.kind === 'nonresponse')?.percentage;
    const decided = nonresponse === undefined ? POLL_POLICY.unknownDecidedFraction : 1 - nonresponse / 100;
    const effectiveSample = Math.min(POLL_POLICY.maximumEffectiveSample,
      (q.questionSampleSize ?? q.source.sampleSize * decided) / POLL_POLICY.designEffect);
    const weight = effectiveSample * 2 ** (-(day(asOf) - day(q.source.fieldworkEnd)) / POLL_POLICY.halfLifeDays);
    return { questionId: q.id, surveyId: q.independenceGroup, pollster: q.source.pollster, effectiveSample, weight,
      sampleEstimated: q.questionSampleSize === null, reconciliation: reconcilePoll(q, entities, context) };
  });
  const weight = evidence.reduce((n, e) => n + e.weight, 0);
  const rows = baseline.map(b => {
    const share = (POLL_POLICY.historicalPseudoSample * b.share + evidence.reduce((n, e) => n + e.weight * e.reconciliation.shares.find(s => s.listId === b.listId)!.share, 0)) /
      (POLL_POLICY.historicalPseudoSample + weight);
    // Recency and estimated allocations reduce precision; the shared error floor
    // is separate from future movement. Shares are normalized jointly at runtime.
    const samplingVariance = evidence.reduce((n, e) => {
      const p = e.reconciliation.shares.find(s => s.listId === b.listId)!.share;
      const estimated = e.reconciliation.assignments.some(a => a.estimated && a.targets.some(t => t.listId === b.listId));
      return n + e.weight ** 2 * p * (1 - p) / Math.max(1, e.effectiveSample) * (estimated ? 2 : 1);
    }, 0) / (POLL_POLICY.historicalPseudoSample + weight) ** 2;
    const priorVariance = POLL_POLICY.historicalPseudoSample ** 2 * b.share * (1 - b.share) / POLL_POLICY.historicalPseudoSample /
      (POLL_POLICY.historicalPseudoSample + weight) ** 2;
    const sigma = Math.sqrt(samplingVariance + priorVariance) / Math.max(0.01, share * (1 - share));
    return { listId: b.listId, share, sigma: Math.hypot(sigma, POLL_POLICY.sharedErrorLogitSigma) };
  });
  return { policy: POLL_POLICY, asOf, conditionalLabel, rows, evidence,
    excludedQuestionIds: questions.filter(q => !selected.some(s => s.id === q.id)).map(q => q.id),
    applied: evidence.length > 0,
    assumptions: ['Effective samples, recency, prior strength and error floor are assumptions, not calibrated intervals.',
      'One wave per house limits repeated-survey dominance; it does not establish independence between houses.',
      'Nonresponse estimates the decided sample base only; election turnout is unchanged.',
      'Shared measurement draws couple chambers; party normalization induces compositional dependence.'] };
}

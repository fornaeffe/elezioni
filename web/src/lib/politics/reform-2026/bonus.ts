import type { AdmissionResult } from './admission';
import { exactFraction } from './exact';
import { calculatePercentages, sumVoteCounts, type ElectoralFigures, type Percentage } from './figures';
import type { Chamber, ReformScrutinyInput, Subject } from './input';
import type { DrawTrace, ReformWarning, ScrutinyExecutionContext } from './result';
import { resolveTie } from './ties';

export interface BonusChamberInput {
  readonly chamber: Chamber;
  readonly admission: AdmissionResult;
  /** N06 national figures INCLUDING VdA/TAA, never ordinaryAllocation. */
  readonly national: ElectoralFigures;
}
export interface GovernabilityBonusInput {
  readonly simulationId: number;
  /** Declared renewal scope; missing joint data must not imply single renewal. */
  readonly renewedChambers: readonly Chamber[];
  readonly chambers: readonly BonusChamberInput[];
  readonly bonusLinks: ReformScrutinyInput['bonusLinks'];
}
export interface BonusFirstSubject {
  readonly chamber: Chamber;
  readonly subject: Subject;
  readonly votes: number;
  readonly validVotes: number;
  readonly percentage: Percentage;
  readonly reaches42Percent: boolean;
}
export type BonusAbsenceReason =
  | { readonly code: 'single-chamber-renewal'; readonly chamber: Chamber }
  | { readonly code: 'below-42-percent'; readonly chamber: Chamber }
  | { readonly code: 'missing-link' | 'unlinked-first-subjects' | 'non-reciprocal-link' };
export interface BonusBlocker {
  readonly chamber: Chamber;
  readonly reason: 'missing-chamber-data' | 'admission-non-computable' | 'zero-valid-votes' |
    'no-admitted-subjects' | 'missing-subject-figure';
}
interface BonusBase {
  readonly norm: 'N12';
  readonly simulationId: number;
  readonly renewedChambers: readonly Chamber[];
  readonly lawReference: string;
  readonly firstSubjects: readonly BonusFirstSubject[];
  readonly draws: readonly DrawTrace[];
  readonly warnings: readonly ReformWarning[];
}
export type GovernabilityBonusResult = BonusBase & (
  | { readonly status: 'awarded'; readonly awards: readonly [
      { readonly chamber: 'camera'; readonly subject: Subject; readonly seats: 70 },
      { readonly chamber: 'senato'; readonly subject: Subject; readonly seats: 35 }
    ] }
  | { readonly status: 'not-awarded'; readonly awards: readonly []; readonly reasons: readonly BonusAbsenceReason[] }
  | { readonly status: 'non-computable'; readonly awards: readonly []; readonly blockers: readonly BonusBlocker[] }
);

const sides = ['camera', 'senato'] as const;
const key = (s: Subject) => JSON.stringify([s.kind, s.id]);
const same = (a: Subject, b: Subject) => key(a) === key(b);

/**
 * C83(1)(e-ter): "quella che ha ottenuto la maggiore cifra elettorale nazionale";
 * (e-quater): "almeno il 42 per cento"; (e-sexies): "pari a settanta seggi".
 * S16-bis(1)(e-ter): "almeno al 42 per cento";
 * (e-quinquies): "pari a trentacinque seggi".
 *
 * C83(1)(e-ter)-(e-septies), S16-bis(1)(e-bis)-(e-sexies): first among
 * admitted e)(1)/(2) subjects, then at least 42% in EACH chamber and declared
 * cross-chamber linkage. Award 70/35 jointly; do not seek a second-place linked
 * pair. C83(1-sexies)/S16-bis(1-sexies) include special-territory list votes.
 * No further first-place tie criterion is stated: use warned provisional I03.
 * Caps, territorial allocation and candidate availability belong to later stages.
 */
export function determineGovernabilityBonus(
  input: GovernabilityBonusInput, execution: ScrutinyExecutionContext
): GovernabilityBonusResult {
  const renewed = sides.filter(s => input.renewedChambers.includes(s));
  if (!Number.isSafeInteger(input.simulationId) || input.simulationId < 0 ||
      renewed.length === 0 || renewed.length !== input.renewedChambers.length ||
      new Set(input.chambers.map(c => c.chamber)).size !== input.chambers.length ||
      input.chambers.some(c => !renewed.includes(c.chamber) || c.admission.chamber !== c.chamber)) {
    throw new RangeError('Supply unique renewed chambers and matching admission data');
  }
  const base: BonusBase = { norm: 'N12', simulationId: input.simulationId, renewedChambers: renewed,
    lawReference: 'C83(1)(e-ter)-(e-septies),(1-quater); S16-bis(1)(e-bis)-(e-sexies),(1-quater)',
    firstSubjects: [], draws: [], warnings: [] };
  // (1-quater): single renewal takes the no-bonus path without ranking or draws.
  if (renewed.length === 1) return { ...base, status: 'not-awarded', awards: [],
    reasons: [{ code: 'single-chamber-renewal', chamber: renewed[0] }] };

  // Preflight BOTH chambers before consuming any legal draw. I04 failures must
  // remain distinguishable from a computed negative threshold/link decision.
  const blockers: BonusBlocker[] = [];
  const prepared: { chamber: Chamber; validVotes: number; rows: { subject: Subject; votes: number }[] }[] = [];
  for (const chamber of sides) {
    const data = input.chambers.find(c => c.chamber === chamber);
    let reason: BonusBlocker['reason'] | undefined;
    if (!data) reason = 'missing-chamber-data';
    else if (data.admission.status !== 'complete') reason = 'admission-non-computable';
    else {
      sumVoteCounts([data.national.validVotes]);
      const subjects = data.admission.admittedSubjects;
      if (new Set(subjects.map(key)).size !== subjects.length) throw new RangeError('Duplicate admitted subject');
      const rows = subjects.map(subject => ({ subject, votes: subject.kind === 'list'
        ? data.national.lists.find(l => l.listId === subject.id)?.votes
        : data.national.coalitions.find(c => c.coalitionId === subject.id)?.votes }));
      if (data.national.validVotes === 0) reason = 'zero-valid-votes';
      else if (!subjects.length) reason = 'no-admitted-subjects';
      else if (rows.some(r => r.votes === undefined)) reason = 'missing-subject-figure';
      else {
        const complete = rows.map(r => ({ subject: r.subject, votes: r.votes! }));
        sumVoteCounts(complete.map(r => r.votes));
        if (complete.some(r => r.votes > data.national.validVotes)) throw new RangeError('Subject exceeds all valid votes');
        prepared.push({ chamber, validVotes: data.national.validVotes, rows: complete });
      }
    }
    if (reason) blockers.push({ chamber, reason });
  }
  if (blockers.length) return { ...base, status: 'non-computable', awards: [], blockers };

  const draws: DrawTrace[] = [], warnings: ReformWarning[] = [];
  const firstSubjects = prepared.map(({ chamber, validVotes, rows }): BonusFirstSubject => {
    const lawReference = chamber === 'camera' ? 'C83(1)(e-ter)' : 'S16-bis(1)(e-bis)';
    const tie = resolveTie(rows.map(r => ({ id: key(r.subject), values: [exactFraction(r.votes)] })),
      [{ name: 'national-subject-votes', direction: 'descending' }], { simulationId: input.simulationId,
        request: { contextId: JSON.stringify(['N12', input.simulationId, chamber, 'first-subject']),
          chamber, norm: 'N12', rule: 'I03', lawReference } }, execution);
    draws.push(...tie.draws);
    warnings.push(...tie.warnings);
    const first = rows.find(r => key(r.subject) === tie.chosenId)!;
    return { chamber, subject: { ...first.subject }, votes: first.votes, validVotes,
      percentage: calculatePercentages([first.votes], validVotes)[0],
      reaches42Percent: BigInt(first.votes) * 100n >= BigInt(validVotes) * 42n };
  });
  const [camera, senato] = firstSubjects;
  const matching = input.bonusLinks.filter(l => same(l.camera, camera.subject) && same(l.senato, senato.subject));
  const reasons: BonusAbsenceReason[] = firstSubjects.filter(f => !f.reaches42Percent)
    .map(f => ({ code: 'below-42-percent', chamber: f.chamber }));
  if (!matching.length) reasons.push({ code: input.bonusLinks.some(l =>
    same(l.camera, camera.subject) || same(l.senato, senato.subject)) ? 'unlinked-first-subjects' : 'missing-link' });
  else if (!matching.some(l => l.cameraDeclares && l.senatoDeclares)) reasons.push({ code: 'non-reciprocal-link' });
  const evidence = { ...base, firstSubjects, draws, warnings };
  return reasons.length ? { ...evidence, status: 'not-awarded', awards: [], reasons }
    : { ...evidence, status: 'awarded', awards: [
      { chamber: 'camera', subject: { ...camera.subject }, seats: 70 },
      { chamber: 'senato', subject: { ...senato.subject }, seats: 35 }
    ] };
}

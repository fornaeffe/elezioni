import { otherListsInCollege } from './other-list';
import type { ListVotes, OrdinarySlate } from './input';
import { ChamberContext, key } from './validation-context';

/** Validate representations; legal attribution runs in N04/N10. */
export function validateVotes(c: ChamberContext): void {
  const { validation: v, data: d, path: p } = c;
  function lists(rows: readonly ListVotes[], expected: readonly string[], path: string): Map<string, ListVotes> {
    const map = v.index(rows, x => x.listId, path);
    for (const row of rows) {
      v.reference(c.lists, row.listId, path);
      if (!expected.includes(row.listId)) v.add('INVALID_VOTES', path, 'Votes refer to a list not contesting this college.');
    }
    for (const id of expected) if (!map.has(id)) v.add('INVALID_VOTES', path, `Missing vote count for contesting list ${id}; supply explicit zeros.`);
    return map;
  }
  function electorate(total: number, electors: number | null, path: string) {
    if (electors !== null && total > electors) v.add('INVALID_VOTES', path, 'Valid ballots exceed the supplied electorate.', 'K48');
  }
  const ordinary = v.index(d.ordinaryVotes, x => x.collegeId, `${p}.ordinaryVotes`);
  const special = v.index(d.specialVotes, x => x.collegeId, `${p}.specialVotes`);
  for (const college of d.colleges) {
    if (!(college.kind === 'special-uninominal' ? special : ordinary).has(college.id)) v.add('INVALID_VOTES', p, `Missing votes for college ${college.id}.`);
  }
  for (const row of d.ordinaryVotes) {
    const path = `${p}.ordinaryVotes.${row.collegeId}`;
    const college = v.reference(c.colleges, row.collegeId, path);
    if (college?.kind === 'special-uninominal') v.add('INVALID_VOTES', path, 'Uninominal votes require the special representation.');
    const slates = d.ordinarySlates.filter(s => s.collegeId === row.collegeId);
    const votes = row.votes;
    const listRows = votes.kind === 'components' ? votes.directListVotes : votes.listVotes;
    const byList = lists(listRows, [...slates.map(s => s.listId), ...otherListsInCollege(d, row.collegeId)], path);
    let total = v.sum(listRows.map(x => x.votes), path);
    if (votes.kind === 'components') {
      v.index(votes.bonusOnlyVotes, x => key(x.subject.kind, x.subject.id), `${path}.bonusOnlyVotes`);
      for (const bonus of votes.bonusOnlyVotes) {
        const members = c.subject(bonus.subject, path);
        if (!members.some(id => slates.some(s => s.listId === id))) v.add('INVALID_VOTES', path, 'Bonus-only votes require a subject contesting this college.', 'C58/S14');
        if (bonus.votes > 0 && (v.input.renewal.kind === 'single' || college?.kind !== 'ordinary' ||
            !d.bonusSlates.some(s => s.territoryId === college?.territoryId && key(s.subject.kind, s.subject.id) === key(bonus.subject.kind, bonus.subject.id)))) {
          v.add('INVALID_VOTES', path, 'Positive bonus-only votes require a filed bonus slate in a joint renewal.', 'C58/S14');
        }
      }
      total = v.sum([total, ...votes.bonusOnlyVotes.map(x => x.votes)], path);
    }
    electorate(total, row.electorate.electors, path);
    for (const slate of slates) {
      // C93-sexies(3)-(4), C93-septies(1)(d)-(e): TAA preferences accompany
      // the SAME uninominal ballots. The compatibility ordinaryVotes view is
      // not an independent preference budget. Raw candidate-only votes cannot
      // express preferences; attributed representation keeps composition unknown.
      const budget = college?.kind === 'taa-proportional'
        ? v.sum(d.specialVotes.filter(r => c.colleges.get(r.collegeId)?.territoryId === college.territoryId)
          .flatMap(r => (r.votes.kind === 'components' ? r.votes.directListVotes : r.votes.listVotes)
            .filter(l => l.listId === slate.listId).map(l => l.votes)), path)
        : byList.get(slate.listId)?.votes ?? 0;
      validatePreferences(c, slate, budget, path);
    }
  }
  for (const row of d.specialVotes) {
    const path = `${p}.specialVotes.${row.collegeId}`;
    const college = v.reference(c.colleges, row.collegeId, path);
    if (college && college.kind !== 'special-uninominal') v.add('INVALID_VOTES', path, 'Special vote rows require a uninominal college.');
    const contenders = d.specialCandidatures.filter(x => x.collegeId === row.collegeId);
    const expectedLists = [...new Set([...contenders.flatMap(x => x.linkedListIds), ...otherListsInCollege(d, row.collegeId)])];
    const votes = row.votes;
    const listRows = votes.kind === 'components' ? votes.directListVotes : votes.listVotes;
    const byList = lists(listRows, expectedLists, path);
    const totals = v.index(votes.candidateTotals, x => x.candidateId, `${path}.candidateTotals`);
    const only = votes.kind === 'components' ? v.index(votes.candidateOnlyVotes, x => x.candidateId, `${path}.candidateOnlyVotes`) : null;
    for (const records of [totals, only]) if (records) {
      for (const id of records.keys()) if (!contenders.some(x => x.candidateId === id)) v.add('INVALID_VOTES', path, `Votes refer to a non-contesting candidate ${id}.`);
      for (const candidate of contenders) if (!records.has(candidate.candidateId)) v.add('INVALID_VOTES', path, `Missing candidate count for ${candidate.candidateId}.`);
    }
    for (const candidate of contenders) {
      const expected = v.sum([...candidate.linkedListIds.map(id => byList.get(id)?.votes ?? 0), only?.get(candidate.candidateId)?.votes ?? 0], path);
      // C92/C93-sexies; S20(1)(c-ter): candidate-only ballots are included
      // in the candidate total and attributed to its linked lists only once.
      if (totals.get(candidate.candidateId)?.votes !== expected) v.add('INVALID_VOTES', path, 'Candidate totals disagree with their linked lists and candidate-only components.', 'C92/C93-sexies; S20/S21');
    }
    // Candidate totals partition valid ballots: do not add list votes again.
    electorate(v.sum([...votes.candidateTotals.map(x => x.votes), ...listRows.filter(l => otherListsInCollege(d, row.collegeId).includes(l.listId)).map(l => l.votes)], path), row.electorate.electors, path);
  }
}

function validatePreferences(c: ChamberContext, slate: OrdinarySlate, budget: number, parent: string): void {
  const { validation: v } = c;
  const prefs = slate.preferences;
  if (prefs.kind === 'missing') return;
  const path = `${parent}.preferences.${slate.listId}`;
  if (prefs.provenance.kind === 'generated' && prefs.validity.kind !== 'combinations') v.add('INVALID_PREFERENCES', path, 'Generated preferences require a realizable combination certificate (R26-03).');
  const eligible = slate.candidateIds.slice(1);
  const counts = v.index(prefs.counts, x => x.candidateId, `${path}.counts`);
  for (const row of prefs.counts) {
    if (!eligible.includes(row.candidateId)) v.add('INVALID_PREFERENCES', path, 'Preferences must name an eligible non-capolista candidate.', 'C58/C93-sexies; S14');
    if (row.count > budget) v.add('INVALID_PREFERENCES', path, 'Candidate preferences exceed the list ballot budget.');
  }
  for (const id of eligible) if (!counts.has(id)) v.add('INVALID_PREFERENCES', path, `Missing preference count for ${id}; unknown is not zero.`);
  const sum = prefs.counts.reduce((n, x) => n + BigInt(x.count), 0n);
  if (sum > BigInt(budget) * 3n) v.add('INVALID_PREFERENCES', path, 'More than three preferences per list ballot in aggregate.', 'C58/C93-sexies; S14');
  if (prefs.validity.kind === 'adjudicated') return;
  const combinations = prefs.validity.ballots;
  v.index(combinations, x => key(...[...x.candidateIds].sort()), `${path}.validity.ballots`);
  const margins = new Map<string, bigint>();
  for (const ballot of combinations) {
    v.index(ballot.candidateIds, x => x, path);
    if (ballot.candidateIds.length > 3 || ballot.candidateIds.some(id => !eligible.includes(id))) v.add('INVALID_PREFERENCES', path, 'Invalid candidate subset in a preference ballot.', 'C58/C93-sexies; S14');
    // «Nel caso di più preferenze espresse, queste devono riguardare
    // candidati di sesso diverso»: valid combinations use both genders.
    if (ballot.candidateIds.length > 1 && new Set(ballot.candidateIds.map(id => v.people.get(id)?.gender)).size < 2) v.add('INVALID_PREFERENCES', path, 'A multiple-preference combination must contain both genders.', 'C58/C93-sexies; S14');
    for (const id of ballot.candidateIds) margins.set(id, (margins.get(id) ?? 0n) + BigInt(ballot.count));
  }
  // Raw bonus-only ballots cannot carry list preferences; for attributed totals
  // composition is unknown and the chosen synthetic profile remains conditional.
  if (combinations.reduce((n, x) => n + BigInt(x.count), 0n) !== BigInt(budget)) v.add('INVALID_PREFERENCES', path, 'Combination counts must partition the list ballot budget, including empty combinations.');
  for (const id of eligible) if ((margins.get(id) ?? 0n) !== BigInt(counts.get(id)?.count ?? 0)) v.add('INVALID_PREFERENCES', path, 'Preference margins disagree with the ballot combinations.');
}

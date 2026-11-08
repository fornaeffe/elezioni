/** Normalize authored units/identities once, before either statistical model. */
import { chambers, type PoliticsScenario } from '../scenario/schema';
import { activeGenerator } from '../scenario/registry';
import { effectiveFilings, effectiveMembers } from '../scenario/effective';
import type { FastComponent, FastList, FastReference } from '../reform-2026/vote-share/forecast-types';
import type { M01SourceSettings } from '../generation/m01-compiler';
import type { M02Settings } from '../generation/m02';
import type { ExecutionData } from './assets';
import type { SourceSelection } from '../reform-2026/vote-share/source-compiler';
import { pollChoiceId } from '../reform-2026/vote-share/correspondences';

export function commonVoteInputs(s: PoliticsScenario, data: ExecutionData, chamberProfileId: string,
  coverage: M01SourceSettings['coverage']) {
  const model = activeGenerator(s), law = s.selection.scrutiny.id;
  const componentMap = Object.fromEntries(s.history.pooling.flatMap(p => p.members.map(id => [id, p.id])));
  const used = new Set(s.memberships.flatMap(m => chambers.flatMap(ch => [...effectiveMembers(s, m.listId, ch)])));
  // Keep every observed basis component for fitting, including candidate-free residual support.
  const originalIds = data.domain.componentIds.filter(id => !Object.hasOwn(componentMap, id));
  const components: FastComponent[] = [...new Set([...originalIds, ...s.history.pooling.map(p => p.id), ...used])].sort().map(id => {
    const n = s.newComponents.find(n => n.id === id);
    return n ? { id, kind: 'new', provenance: n.provenance, fallback: {
      geographicLogProfile: n.fallback.M02.geographicLogProfile, profileSd: n.fallback.M02.profileSd,
      localInnovationSd: n.fallback.M02.localInnovationSd, nationalInnovationSd: n.fallback.M02.nationalInnovationSd } } : { id, kind: 'fitted', provenance: 'Pinned historical component basis' };
  });
  // An unused owned fallback is retained in the draft, never introduced into the fitted basis.
  const lists = (ch: 'camera' | 'senato'): FastList[] => s.subjects.lists.map(l => {
    const multiplier = s.modelSettings.M02.uncertaintyMultipliers.find(r => r.listId === l.id && r.chamber === ch);
    return { id: l.id, name: l.name, kind: l.kind, coalitionId: l.coalitionId,
      protectedMinority: l.minority ? { regionId: l.minority.regionId, legalReference: l.minority.legalReference ?? 'Rosatellum minority declaration' } : null,
      componentIds: [...effectiveMembers(s, l.id, ch)],
      ...(model.id !== 'M02' ? {} : { nationalUncertaintyMultiplier: multiplier?.national ?? 1, localUncertaintyMultiplier: multiplier?.local ?? 1 }) };
  });
  const scope = (r: PoliticsScenario['references'][number] | PoliticsScenario['nonValidReferences'][number]) =>
    r.scope.kind === 'national' ? { kind: 'national' as const } : { kind: r.scope.kind, code: r.scope.code };
  const references: FastReference[] = s.references.filter(r => r.appliesTo.includes(model.id)).map(r => ({ id: r.id,
    scope: scope(r), channel: r.channel, subject: { ...r.subject }, mean: r.mean,
    // M01 never consumes the SD; absence does not acquire a numerical meaning in the draft.
    sdPercentagePoints: r.uncertainty?.sdPercentagePoints ?? 0 }));
  const edits = new Map(s.history.sourceMemberships.map(r => [r.choiceId, r]));
  if (edits.size !== s.history.sourceMemberships.length)
    throw new Error('SOURCE_ENCODING_CONFLICT: original-column choices require globally unique choice IDs');
  const polls = [...data.source.payload.polls, ...s.history.polls.added].map(p => ({ ...p,
    original: p.original.map(r => ({ ...r })), normalized: p.normalized.map(r => {
      const edit = edits.get(pollChoiceId(p.id, r.label));
      return { ...r, members: [...(edit?.members ?? r.members)] };
    }), reasons: [...p.reasons] }));
  for (const edit of edits.values()) {
    const choice = data.source.payload.choices.find(c => c.id === edit.choiceId);
    if (choice ? choice.eventId !== edit.sourceId : !polls.some(p => p.id === edit.sourceId && p.normalized.some(g => pollChoiceId(p.id, g.label) === edit.choiceId)))
      throw new Error('SOURCE_ENCODING_CONFLICT: source/question/choice does not address the pinned original observations');
  }
  const sourceMemberships = Object.fromEntries([...edits.values()].filter(r => data.source.payload.choices.some(c => c.id === r.choiceId))
    .map(r => [r.choiceId, { members: [...r.members], provenance: r.provenance }]));
  const compositions = model.id === 'M01' ? s.modelSettings.M01.compositions : [];
  if (new Set(compositions.map(c => c.choiceId)).size !== compositions.length) throw new Error('SOURCE_ENCODING_CONFLICT: ambiguous composition choice');
  const generation = { model: model.id, modelVersion: model.version,
    compositions: model.id === 'M01' ? Object.fromEntries(compositions.map(c => [c.choiceId,
      { fractions: { ...c.fractions }, provenance: c.provenance, proxy: c.proxy }])) : {},
    supplementalPolls: model.id === 'M01' ? s.modelSettings.M01.supplementalPolls.map(p => ({ ...p, groups: p.groups.map(g => ({ ...g, members: [...g.members] })) })) : [] };
  const common: M01SourceSettings = { generation, referenceDate: s.referenceDate, electionDate: s.electionDate,
    ...(model.id === 'M01' ? { parameterDate: s.modelSettings.M01.parameterDate } : {}),
    components, componentMap, sourceMemberships, lists: { camera: lists('camera'), senato: lists('senato') }, coverage,
    polls: { selected: s.history.polls.selected.filter(id => !Object.hasOwn(s.history.polls.excluded, id)),
      added: polls.filter(p => s.history.polls.added.some(a => a.id === p.id) || p.normalized.some(g => edits.has(pollChoiceId(p.id, g.label)))),
      excluded: { ...s.history.polls.excluded } },
    references, nonValidReferences: s.nonValidReferences.filter(r => r.appliesTo.includes(model.id)).map(r => ({
      id: r.id, scope: scope(r), channel: r.channel, mean: r.mean, sdPercentagePoints: r.uncertainty?.sdPercentagePoints ?? 0 })),
    renewal: s.renewal, seed: s.execution.simulationSeed, chamberProfileId };
  const m02 = s.modelSettings.M02;
  const fast: M02Settings = { generation, seed: common.seed, electionDate: common.electionDate, renewal: common.renewal,
    coverage, chamberProfileId, referenceDate: common.referenceDate, fitSeed: s.execution.fittingSeed,
    lists: common.lists, components, references, nonValidReferences: common.nonValidReferences,
    models: { statistical: m02.statistical, ...m02.policies, forecast: m02.forecast, forecastScale: m02.forecastScale,
      profileRegionCorrelation: m02.profileRegionCorrelation, forecastReference: m02.forecastReference },
    data: { sourceBundleId: data.source.id, fittedStateId: '', profileStateId: '', geographyVersion: data.geography.version } };
  const selection: SourceSelection = { referenceDate: s.referenceDate, electionIds: [...s.history.electionIds],
    pollIds: common.polls.selected, components: components.filter(c => c.kind === 'fitted').map(c => c.id),
    sourceMemberships, componentMap, targets: data.electorates.map(e => ({ code: e.code, electors: e.camera })), targetContext: 'camera' };
  return { common, fast, selection, filings: (listId: string) => effectiveFilings(s, law, listId) };
}

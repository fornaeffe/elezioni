/** Explicit candidate policy, independent of research inventories/derived exports. */
import type {
  FastGenerationScenario,
  FastReference,
  TargetElectorate,
} from '../../src/lib/politics/reform-2026/vote-share/forecast-types';
import type {
  CompactSource,
  OriginalRow,
} from '../../src/lib/politics/reform-2026/vote-share/source-types';
import type { ReformGenerationScenario } from '../../src/lib/politics/reform-2026/generation-types';
import type { SourceSelection } from '../../src/lib/politics/reform-2026/vote-share/source-compiler';
import { normalizeName } from './formats';
import { sum } from '../../src/lib/politics/reform-2026/vote-share/types';
export interface DefaultSharePolicy {
  schemaVersion: 1;
  id: string;
  referenceDate: string;
  electionDate: string;
  fitSeed: string;
  simulationSeed: string;
  history: string[];
  polls: string[];
  excludedPolls: Record<string, string>;
  paired: { camera: string; senato: string; electoratePolicy: 'equal-only' };
  basisPolicy: string;
  referenceSupportedPolicy: string;
  electoratePolicy: string;
  selectionStatus: string;
  newComponentFallback: {
    geographicLogProfile: number;
    profileSd: number;
    localInnovationSd: number;
    nationalInnovationSd: number;
  };
  specialRegionNonValidAnchors: string[];
}
export function defaultSharePlan(
  source: CompactSource,
  old: Pick<ReformGenerationScenario, 'lists' | 'coverage'>,
  policy: DefaultSharePolicy,
  camera: OriginalRow[],
  senato: OriginalRow[],
) {
  if (
    policy.schemaVersion !== 1 ||
    policy.id !== 'fast-share-default-policy-v1'
  )
    throw new Error('Unsupported default policy');
  const declared = [...policy.polls, ...Object.keys(policy.excludedPolls)];
  if (
    new Set(declared).size !== declared.length ||
    declared.length !== source.payload.polls.length ||
    source.payload.polls.some((p) => !declared.includes(p.id))
  )
    throw new Error(
      'Default poll inventory drift; review the pinned selection',
    );
  const listMembers: {
      camera: Record<string, string[]>;
      senato: Record<string, string[]>;
    } = {
      camera: {},
      senato: {},
    },
    sourceChoices: {
      camera: Record<string, string>;
      senato: Record<string, string>;
    } = {
      camera: {},
      senato: {},
    },
    covered = new Set<string>(),
    pool: Record<string, string> = {},
    futureJoint: Record<string, string> = {};
  const ordinary = new Set(
    source.payload.geography.colleges
      .filter((c) => c.kind === 'ordinary')
      .map((c) => c.id),
  );
  for (const ch of ['camera', 'senato'] as const)
    for (const list of old.lists[ch]) {
      const found = source.payload.choices.filter(
        (c) =>
          c.eventId === ch + '-2022' &&
          normalizeName(c.label) === normalizeName(list.name) &&
          c.members.some((m) => m.startsWith('special-')) ===
            list.id.startsWith('group-'),
      );
      if (found.length !== 1)
        throw new Error(
          'Ambiguous original default ballot ' + ch + '/' + list.id,
        );
      listMembers[ch][list.id] = [...found[0].members];
      sourceChoices[ch][list.id] = found[0].id;
      if (
        ch === 'camera' &&
        old.coverage.some(
          (c) =>
            c.chamber === ch &&
            ordinary.has(c.collegeId) &&
            c.listIds.includes(list.id),
        )
      )
        for (const id of found[0].members)
          if (!id.startsWith('special-')) covered.add(id);
    }
  for (const [list, members] of Object.entries(listMembers.camera))
    if (members.length > 1 && !members.includes('azione'))
      for (const id of members) {
        pool[id] = 'joint:' + list;
        futureJoint[id] = 'joint:' + list;
      }
  const originals = [
    ...new Set([
      ...source.payload.choices.flatMap((c) => c.members),
      ...source.payload.polls.flatMap((p) =>
        p.normalized.flatMap((a) => a.members),
      ),
    ]),
  ].sort();
  for (const id of originals)
    if (!covered.has(id)) pool[id] = 'historical-rest';
  const components = [...new Set(originals.map((id) => pool[id] ?? id))].sort(),
    known = new Set(components),
    newComponents = new Set<string>();
  for (const ch of ['camera', 'senato'] as const)
    for (const [list, members] of Object.entries(listMembers[ch])) {
      listMembers[ch][list] = [
        ...new Set(members.map((id) => futureJoint[id] ?? id)),
      ].sort();
      for (const id of listMembers[ch][list])
        if (!known.has(id)) newComponents.add(id);
    }
  if ([...newComponents].some((id) => known.has(id)))
    throw new Error('Default support basis collision');
  const units = new Map(source.payload.units.map((u) => [u.id, u])),
    matched = camera.filter((r) => units.get(r.unit)?.targetCode),
    byCode = new Map(matched.map((r) => [units.get(r.unit)!.targetCode!, r]));
  if (byCode.size !== matched.length)
    throw new Error('Default electorate crosswalk is not one-to-one');
  const targets = source.payload.geography.targets.map((t) => {
    const r = byCode.get(t.code);
    if (!r)
      throw new Error('Missing conditional original electorate ' + t.code);
    return { code: t.code, electors: r.electors };
  });
  const electorates: TargetElectorate[] = targets.map((t) => ({
    code: t.code,
    date: '2022-09-25',
    camera: t.electors,
    senato: t.electors,
    provenance: policy.electoratePolicy,
  }));
  const sourceMemberships = Object.fromEntries(
    source.payload.choices.map((c) => [
      c.id,
      {
        members: [...c.members],
        provenance: c.method + '; citations: ' + c.citations.join(', '),
      },
    ]),
  );
  const selection: SourceSelection = {
    referenceDate: policy.referenceDate,
    electionIds: [...policy.history],
    pollIds: [...policy.polls],
    components,
    componentMap: pool,
    sourceMemberships,
    targets,
    targetContext: 'camera',
  };
  const references: FastReference[] = [],
    nonValidReferences: FastGenerationScenario['nonValidReferences'] = [];
  for (const [ch, rows] of [
    ['camera', camera],
    ['senato', senato],
  ] as const) {
    for (const list of old.lists[ch]) {
      const members = listMembers[ch][list.id];
      if (!members.some((id) => newComponents.has(id))) continue;
      if (members.length !== 1)
        throw new Error(
          'New grouped support requires an explicit separate policy',
        );
      const filed = new Set(
        old.coverage
          .filter((c) => c.chamber === ch && c.listIds.includes(list.id))
          .map((c) => c.collegeId),
      );
      const fractions = new Map<string, number>();
      for (const cell of source.payload.geography.cells.filter(
        (c) => c.chamber === ch && filed.has(c.collegeId),
      ))
        fractions.set(
          cell.municipality,
          (fractions.get(cell.municipality) ?? 0) +
            cell.population /
              source.payload.geography.targets.find(
                (t) => t.code === cell.municipality,
              )!.population,
        );
      for (const region of policy.specialRegionNonValidAnchors) {
        const selected = rows.filter(
            (r) => units.get(r.unit)!.region === region,
          ),
          valid = sum(selected.map((r) => sum(r.choices.map((c) => c.count))));
        const count = sum(
          selected.map(
            (r) =>
              sum(
                r.choices
                  .filter((c) => c.id === sourceChoices[ch][list.id])
                  .map((c) => c.count),
              ) * (fractions.get(units.get(r.unit)!.targetCode!) ?? 0),
          ),
        );
        if (count > 0)
          references.push({
            id: 'baseline:' + ch + ':' + region + ':' + list.id,
            scope: { kind: 'region', code: region },
            channel: ch,
            subject: { kind: 'component', id: members[0] },
            mean: count / valid,
            sdPercentagePoints: 0,
          });
      }
    }
    for (const region of policy.specialRegionNonValidAnchors) {
      const selected = rows.filter((r) => units.get(r.unit)!.region === region),
        e = sum(selected.map((r) => r.electors)),
        v = sum(selected.map((r) => sum(r.choices.map((c) => c.count))));
      if (!(e > 0 && v > 0 && v <= e))
        throw new Error('Invalid default regional reference denominator');
      nonValidReferences.push({
        id: 'baseline-nonvalid:' + ch + ':' + region,
        scope: { kind: 'region', code: region },
        channel: ch,
        mean: 1 - v / e,
        sdPercentagePoints: 0,
      });
    }
  }
  for (const id of newComponents)
    if (!references.some((r) => r.subject.id === id && r.mean > 0))
      throw new Error(
        'Default reference-supported component has no eligible anchor: ' + id,
      );
  return {
    selection,
    listMembers,
    electorates,
    references,
    nonValidReferences,
    components: [
      ...components.map((id) => ({
        id,
        kind: 'fitted' as const,
        provenance: policy.basisPolicy,
      })),
      ...[...newComponents].sort().map((id) => ({
        id,
        kind: 'new' as const,
        provenance: policy.referenceSupportedPolicy,
        fallback: policy.newComponentFallback,
      })),
    ],
    assumptions: [
      policy.basisPolicy,
      policy.referenceSupportedPolicy,
      policy.electoratePolicy,
      'The candidate preserves the installed 2022-derived filing/list choice set. Poll-only or historical identities not filed in it are removed by the approved all-elector choice-set renormalization; they are not silently assigned to a present list.',
    ],
  };
}

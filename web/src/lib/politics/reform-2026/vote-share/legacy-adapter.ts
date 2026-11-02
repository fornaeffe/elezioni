import { requireGenerationModel } from './model-registry';
/** Unified scenario -> compared legacy generator, from the same original source. */
import type { ReleasedScenario } from './release';
import type { CompactSource, OriginalRow } from './source-types';
import type { FastVoteAssets } from './forecast-types';
import type {
  ReformAssemblyContext,
  ReformGenerationScenario,
  ReformGenerationSnapshot,
  HistoricalMunicipality,
} from '../generation-types';
import { REFORM_MODEL_IDS } from '../generation-types';
import { decodeSourceRows, sourceKey, type AssetBytes } from './source-codec';
import { correspondenceRow } from './correspondences';
import { refreshLegacyPolls, type LegacyPollObservation } from './legacy-polls';
import { apportion, generationRng } from '../generation-math';
import { reformPresentation } from '../presentation-metadata';
export interface CompiledLegacy {
  inputKey: string;
  scenario: ReformGenerationScenario;
  snapshot: ReformGenerationSnapshot;
  audit: {
    baseline: { camera: string; senato: string };
    missingHistory: string[];
    policy: string;
    polls: ReturnType<typeof refreshLegacyPolls>;
  };
}
export const isM01Generation = (s: Pick<ReleasedScenario, 'generation'>) =>
  s.generation?.model === 'M01';
export function legacyInputKey(s: ReleasedScenario) {
  // SDs, fitting seeds, candidate-only profile scales and pooling do not acquire
  // unsupported legacy meanings. They remain in the common editable scenario.
  return sourceKey('unified-legacy-input-v1', {
    source: s.data.sourceBundleId,
    referenceDate: s.referenceDate,
    electionDate: s.electionDate,
    generation: s.generation,
    sourceMemberships: s.sourceMemberships,
    componentMap: s.componentMap,
    polls: s.polls,
    lists: s.lists,
    coverage: s.coverage,
    references: s.references.map(({ sdPercentagePoints, ...r }) => r),
    nonValid: s.nonValidReferences.map(({ sdPercentagePoints, ...r }) => r),
    renewal: s.renewal,
    seed: s.seed,
    candidateGeneration: s.candidateGeneration,
    candidateTemplates: s.candidateTemplates,
    suppliedVotes: s.suppliedVotes,
    suppliedPreferences: s.suppliedPreferences,
    bonusLinks: s.bonusLinks,
    chamberProfileId: s.chamberProfileId,
    preferenceProfileId: s.preferenceProfileId,
  });
}
export function latestLegacyPair(source: CompactSource, cutoff: string) {
  const eligible = source.payload.events.filter(
    (e) => e.date <= cutoff && e.availableDate <= cutoff && e.minimumAge === 18,
  );
  const pairs = eligible
    .filter((e) => e.context === 'camera')
    .flatMap((camera) =>
      eligible
        .filter((e) => e.context === 'senato' && e.date === camera.date)
        .map((senato) => ({ camera, senato })),
    )
    .sort(
      (a, b) =>
        b.camera.date.localeCompare(a.camera.date) ||
        a.camera.id.localeCompare(b.camera.id),
    );
  if (!pairs.length)
    throw new Error(
      'No eligible common-18+ Camera/Senato pair at this reference date',
    );
  return pairs[0];
}
export async function compileLegacy(
  s: ReleasedScenario,
  source: CompactSource,
  read: (path: string) => Promise<AssetBytes>,
  assets: FastVoteAssets,
  context: ReformAssemblyContext,
): Promise<CompiledLegacy> {
  requireGenerationModel(s.generation);
  if (!s.generation) throw new Error('Unified generation settings required');
  const pair = latestLegacyPair(source, s.referenceDate),
    inputKey = await legacyInputKey(s),
    choices = new Map(source.payload.choices.map((c) => [c.id, c])),
    units = new Map(source.payload.units.map((u) => [u.id, u])),
    ids = new Set(s.components.map((c) => c.id)),
    canonical = (id: string) => (ids.has(id) ? id : (s.componentMap[id] ?? id)),
    missingHistory: string[] = [],
    tables = new Map<string, Map<string, HistoricalMunicipality['camera']>>();
  const historicalLists = source.payload.choices
    .filter((c) => c.eventId === pair.camera.id || c.eventId === pair.senato.id)
    .map((c) => ({
      id: c.id,
      name: c.label,
      kind: c.opaque ? ('special-ballot-group' as const) : ('list' as const),
    }));
  const correspondences: ReformGenerationScenario['correspondences'] = [];
  for (const ch of ['camera', 'senato'] as const) {
    const event = pair[ch],
      rows = await decodeSourceRows(
        source,
        event.id,
        await read(event.chunk.path),
      );
    const exact = new Map<string, OriginalRow>(),
      regions = new Map<
        string,
        { electors: number; votes: Map<string, number> }
      >();
    for (const row of rows) {
      const unit = units.get(row.unit)!;
      const members = row.choices.flatMap(
        (c) => s.sourceMemberships[c.id]?.members ?? choices.get(c.id)!.members,
      );
      if (new Set(members).size !== members.length)
        throw new Error(
          'Component competes in multiple original source choices: ' + row.unit,
        );
      for (const c of row.choices) {
        const choice = choices.get(c.id)!,
          composition = s.generation.compositions[c.id];
        if (composition?.proxy && !choice.opaque)
          throw new Error(
            'Legacy proxy is reserved for explicitly opaque source groups',
          );
      }
      const valid = row.choices.reduce((n, c) => n + c.count, 0);
      if (
        unit.match === 'unlocated' ||
        row.electors <= 0 ||
        valid > row.electors
      )
        continue;
      const r = regions.get(unit.region) ?? {
        electors: 0,
        votes: new Map<string, number>(),
      };
      r.electors += row.electors;
      for (const c of row.choices)
        r.votes.set(c.id, (r.votes.get(c.id) ?? 0) + c.count);
      regions.set(unit.region, r);
      if (unit.match === 'same-place' && unit.targetCode) {
        if (exact.has(unit.targetCode))
          throw new Error('Duplicate legacy geographic baseline');
        exact.set(unit.targetCode, row);
      }
    }
    const table = new Map<string, HistoricalMunicipality['camera']>();
    for (const target of assets.electorates) {
      const geo = assets.geography.targets.find((g) => g.code === target.code)!,
        row = exact.get(target.code),
        regional = regions.get(geo.region);
      if (!row && !regional)
        throw new Error(
          'No original regional history for ' + ch + '/' + target.code,
        );
      const values = row
          ? row.choices.map((c) => ({ listId: c.id, votes: c.count }))
          : [...regional!.votes].map(([listId, votes]) => ({ listId, votes })),
        electorate = row?.electors ?? regional!.electors,
        total = values.reduce((n, c) => n + c.votes, 0),
        counts = apportion(
          target[ch],
          [...values.map((v) => v.votes), electorate - total],
          generationRng('unified-legacy-baseline-v1', 0, ch, target.code),
        );
      if (!row) missingHistory.push(ch + '/' + target.code);
      table.set(target.code, {
        electors: target[ch],
        votes: values.map((v, i) => ({ ...v, votes: counts[i] })),
      });
    }
    tables.set(ch, table);
    for (const choice of choices.values())
      if (choice.eventId === event.id) {
        const members =
            s.sourceMemberships[choice.id]?.members ?? choice.members,
          composition = s.generation.compositions[choice.id];
        for (const link of correspondenceRow(
          members,
          composition,
          s.lists[ch],
          canonical,
        ))
          correspondences.push({ chamber: ch, pastListId: choice.id, ...link });
      }
  }
  const snapshot: ReformGenerationSnapshot = {
    ...structuredClone(context),
    schemaVersion: 1,
    kind: 'politics-reform-generation',
    version: inputKey,
    historicalDate: pair.camera.date,
    historicalLists,
    historicalCoalitions: [],
    memberships: assets.geography.cells.map((c) => ({
      chamber: c.chamber,
      municipalityCode: c.municipality,
      collegeId: c.collegeId,
      population: c.population,
    })),
    municipalities: assets.electorates.map((t) => {
      const g = assets.geography.targets.find((g) => g.code === t.code)!;
      return {
        code: t.code,
        name: g.name,
        regionId: g.region,
        provinceId: g.province,
        camera: tables.get('camera')!.get(t.code)!,
        senato: tables.get('senato')!.get(t.code)!,
      };
    }),
  };
  for (const proportional of context.seats.geography.colleges.filter(
    (c) =>
      c.kind === 'taa-proportional' &&
      !assets.geography.cells.some((cell) => cell.collegeId === c.id),
  ))
    for (const cell of assets.geography.cells.filter(
      (c) =>
        c.chamber === proportional.chamber &&
        context.seats.geography.colleges.some(
          (g) =>
            g.id === c.collegeId && g.territoryId === proportional.territoryId,
        ),
    ))
      snapshot.memberships.push({
        chamber: cell.chamber,
        municipalityCode: cell.municipality,
        collegeId: proportional.id,
        population: cell.population,
      });
  const groupedMemberships = new Map<
    string,
    (typeof snapshot.memberships)[number]
  >();
  for (const m of snapshot.memberships) {
    const key = JSON.stringify([m.chamber, m.municipalityCode, m.collegeId]),
      old = groupedMemberships.get(key);
    groupedMemberships.set(
      key,
      old ? { ...m, population: old.population + m.population } : m,
    );
  }
  snapshot.memberships = [...groupedMemberships.values()];
  snapshot.assumptions.push(
    'Original latest eligible common-18+ Camera/Senato pair; target electorates are conditional, missing municipal history uses labelled original regional proportions.',
    'Political mapping/filing is conditioned on available choices, preserving historical turnout; post-FV-08 policy audit remains planned.',
  );
  const scenario: ReformGenerationScenario = {
    schemaVersion: 2,
    kind: s.kind,
    algorithmId: s.algorithmId,
    snapshotVersion: snapshot.version,
    modelVersion: context.models.version,
    modelIds: REFORM_MODEL_IDS,
    preferenceProfileId: s.preferenceProfileId,
    chamberProfileId: s.chamberProfileId,
    seed: s.seed,
    electionDate: s.electionDate,
    projectionDate: s.referenceDate,
    mode: 'mean',
    renewal: s.renewal,
    lists: {
      camera: s.lists.camera.map(
        ({
          componentIds,
          nationalUncertaintyMultiplier,
          localUncertaintyMultiplier,
          ...l
        }) => l,
      ),
      senato: s.lists.senato.map(
        ({
          componentIds,
          nationalUncertaintyMultiplier,
          localUncertaintyMultiplier,
          ...l
        }) => l,
      ),
    },
    coverage: s.coverage,
    correspondences,
    shares: [],
    abstention: [],
    candidateGeneration: s.candidateGeneration,
    candidateTemplates: s.candidateTemplates,
    suppliedPreferences: s.suppliedPreferences,
    suppliedVotes: s.suppliedVotes,
    bonusLinks: s.bonusLinks,
    conversion: null,
    projectionPolicy: 'preserve-historical-turnout-v1',
    presentation: reformPresentation(s),
  };
  const added = new Map(s.polls.added.map((p) => [p.id, p])),
    polls: LegacyPollObservation[] = s.polls.selected
      .map(
        (id) => added.get(id) ?? source.payload.polls.find((p) => p.id === id),
      )
      .map((p) => {
        if (!p) throw new Error('Unknown selected legacy poll');
        return {
          id: p.id,
          date: p.date,
          end: p.fieldworkEnd,
          available: p.availableDate,
          context: p.context,
          source: p.citation,
          sample: p.effectiveSample,
          enabled: p.status === 'supported',
          groups: p.normalized
            .filter((a) => !a.other)
            .map((a) => ({
              label: a.label,
              members: a.members,
              share: a.share,
            })),
        };
      });
  // The compared partial named observations stay explicit and editable. They do
  // not pretend to be complete candidate observations or create fitted support.
  polls.push(...s.generation.supplementalPolls);
  if (
    polls.some(
      (p) =>
        !Number.isFinite(p.sample) ||
        p.sample <= 0 ||
        p.groups.some(
          (g) =>
            !g.members.length ||
            !Number.isFinite(g.share) ||
            g.share < 0 ||
            g.share > 1,
        ),
    )
  )
    throw new Error('Invalid legacy named poll observation');
  const lists = [
    ...new Map(
      [...s.lists.camera, ...s.lists.senato].map((l) => [l.id, l]),
    ).values(),
  ];
  const pollAudit = refreshLegacyPolls(
    scenario,
    snapshot,
    polls,
    s.referenceDate,
    lists,
    s.generation.compositions,
    canonical,
    s.lists,
  );
  // Manual point references take precedence. Unsupported SDs and incomplete
  // component-only interventions remain candidate-only, visibly in the editor.
  const refs = new Map<string, ReformGenerationScenario['shares'][number]>();
  for (const r of s.references) for (const ch of s.renewal === 'joint' ? (['camera','senato'] as const) : [s.renewal]) {
    if (r.channel !== 'common' && r.channel !== ch) continue;
    const chamberLists=s.lists[ch];
    const matches =
      r.subject.kind === 'list'
        ? chamberLists.filter((l) => l.id === r.subject.id)
        : chamberLists.filter(
            (l) =>
              l.componentIds.length === 1 && l.componentIds[0] === r.subject.id,
          );
    for (const list of matches) {
      const key = JSON.stringify([r.scope, list.id]),
        row = refs.get(key) ?? {
          scope: r.scope,
          listId: list.id,
          common: null,
          camera: null,
          senato: null,
        };
      row[r.channel] = r.mean;
      refs.set(key, row);
    }
  }
  scenario.shares.push(...refs.values());
  const turnout = new Map<
    string,
    ReformGenerationScenario['abstention'][number]
  >();
  for (const r of s.nonValidReferences) {
    const key = JSON.stringify(r.scope),
      row = turnout.get(key) ?? {
        scope: r.scope,
        common: null,
        camera: null,
        senato: null,
      };
    row[r.channel] = r.mean;
    turnout.set(key, row);
  }
  scenario.abstention = [...turnout.values()];
  if (scenario.pollReferences)
    scenario.pollReferences.archiveVersion = inputKey;
  return {
    inputKey,
    scenario,
    snapshot,
    audit: {
      baseline: { camera: pair.camera.id, senato: pair.senato.id },
      missingHistory,
      policy: 'condition-political-support-preserve-turnout-v1',
      polls: pollAudit,
    },
  };
}

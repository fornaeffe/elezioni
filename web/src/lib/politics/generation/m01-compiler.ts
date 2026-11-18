/** Original-source compiler shared by both law adapters. No legal scenario is constructed. */
import { requireGenerationModel } from '../reform-2026/vote-share/model-registry';
import type { ReleasedScenario } from '../reform-2026/vote-share/release';
import type { CompactSource, OriginalRow } from '../reform-2026/vote-share/source-types';
import type { FastVoteAssets } from '../reform-2026/vote-share/forecast-types';
import type { HistoricalMunicipality } from '../reform-2026/generation-types';
import type { M01Settings, M01Data, VoteGeography } from './types';
import { decodeSourceRows, type AssetBytes } from '../reform-2026/vote-share/source-codec';
import { correspondenceRow } from '../reform-2026/vote-share/correspondences';
import { refreshM01Polls, type LegacyPollObservation } from './m01-polls';
import { apportion, generationRng } from '../reform-2026/generation-math';
export type M01SourceSettings = Pick<ReleasedScenario, 'generation' | 'referenceDate' | 'electionDate' |
  'components' | 'componentMap' | 'sourceMemberships' | 'lists' | 'coverage' | 'polls' | 'references' |
  'nonValidReferences' | 'renewal' | 'seed' | 'chamberProfileId'> & { parameterDate?: string };
export interface M01Context { geography: VoteGeography; models: M01Data['models'] }
export interface CompiledM01 { inputKey: string; scenario: M01Settings; snapshot: M01Data;
  audit: { baseline: { camera: string; senato: string }; missingHistory: string[]; policy: string; polls: ReturnType<typeof refreshM01Polls> } }
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
export async function compileM01(
  s: M01SourceSettings,
  source: CompactSource,
  read: (path: string) => Promise<AssetBytes>,
  assets: Pick<FastVoteAssets, 'electorates' | 'geography'>,
  context: M01Context,
  inputKey: string,
): Promise<CompiledM01> {
  requireGenerationModel(s.generation);
  if (!s.generation) throw new Error('Unified generation settings required');
  const pair = latestLegacyPair(source, s.referenceDate),
    choices = new Map(source.payload.choices.map((c) => [c.id, c])),
    units = new Map(source.payload.units.map((u) => [u.id, u])),
    ids = new Set(s.components.map((c) => c.id)),
    canonical = (id: string) => (ids.has(id) ? id : (s.componentMap[id] ?? id)),
    missingHistory: string[] = [],
    tables = new Map<string, Map<string, HistoricalMunicipality['camera']>>();
  const correspondences: M01Settings['correspondences'] = [];
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
  const snapshot: M01Data = {
    geography: structuredClone(context.geography),
    models: structuredClone(context.models),
    version: inputKey,
    historicalDate: pair.camera.date,
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
  for (const proportional of context.geography.colleges.filter(
    (c) =>
      c.kind === 'taa-proportional' &&
      !assets.geography.cells.some((cell) => cell.collegeId === c.id),
  ))
    for (const cell of assets.geography.cells.filter(
      (c) =>
        c.chamber === proportional.chamber &&
        context.geography.colleges.some(
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
  const scenario: M01Settings = {
    chamberProfileId: s.chamberProfileId,
    seed: s.seed,
    electionDate: s.electionDate,
    projectionDate: s.referenceDate,
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
    projectionPolicy: 'preserve-historical-turnout-v1',
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
  const pollAudit = refreshM01Polls(
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
  const refs = new Map<string, M01Settings['shares'][number]>();
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
    M01Settings['abstention'][number]
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
  // The common scenario persists this distinct projection/parameter date.
  // Retained reform callers omit it and keep their existing poll-derived date.
  if (s.parameterDate !== undefined) scenario.projectionDate = s.parameterDate;
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

/** Constructed original observations on the existing complete reduced legal geography. */
import { fastReleaseFixture } from './fast-release-input';
import { originalShareFixture } from './fast-share-source-input';
import { reducedShareInput } from './fast-share-input';
import {
  encodeSource,
  compressAsset,
  canonicalJson,
  sourceKey,
} from '../../src/lib/politics/reform-2026/vote-share/source-codec';
import { compileSource } from '../../src/lib/politics/reform-2026/vote-share/source-compiler';
import { compilePairedHistory } from '../../src/lib/politics/reform-2026/vote-share/paired-source';
import { fitReference } from '../../src/lib/politics/reform-2026/vote-share/information';
import { fitProfiles } from '../../src/lib/politics/reform-2026/vote-share/profiles';
import { prepareSenateAdapter } from '../../src/lib/politics/reform-2026/vote-share/senate-adapter';
import {
  runFitTask,
  evidenceKey,
} from '../../src/lib/politics/reform-2026/vote-share/fit-service';
import { generationKeys } from '../../src/lib/politics/reform-2026/vote-share/release';
import type { FitConfiguration } from '../../src/lib/politics/reform-2026/vote-share/fit-types';
import type {
  OriginalSource,
  SourcePoll,
} from '../../src/lib/politics/reform-2026/vote-share/source-types';
export async function loadedFitFixture() {
  const f = await fastReleaseFixture(),
    configuration: FitConfiguration = {
      base: '',
      pointer: f.pointer,
      implementations: {
        generation: f.implementation,
        fitting: f.implementation,
        profiles: f.implementation,
      },
    };
  const result = await runFitTask({ action: 'load', configuration }, f.read);
  return { ...f, configuration, result, accepted: result.accepted! };
}
export async function originalFitFixture() {
  const base = await loadedFitFixture(),
    accepted = base.accepted,
    ids = ['a', 'b', 'c', 'd'],
    geography = accepted.assets.geography,
    dates = [
      ...reducedShareInput().national.elections.map((e) => ({
        id: e.id,
        date: e.date,
        context: e.context,
      })),
      { id: 'c22', date: '2022-09-25', context: 'camera' as const },
      { id: 's22', date: '2022-09-25', context: 'senato' as const },
    ];
  const original: OriginalSource = {
    sourceVersions: ['fv06-constructed-source'],
    methodVersions: ['fv06-test-only'],
    assumptions: ['Constructed refit boundary'],
    correspondencePolicy: { id: 'fixture', status: 'test-only', warnings: [] },
    citations: [{ id: 'fixture', url: 'https://example.org/fv06' }],
    geography,
    units: geography.targets.map((t) => ({
      id: t.code,
      name: t.name,
      region: t.region,
      province: t.province,
      targetCode: t.code,
      match: 'same-place',
    })),
    choices: dates.flatMap((e) =>
      ids.map((id, j) => ({
        id: e.id + ':' + j,
        eventId: e.id,
        label: id,
        members: [id],
        method: 'Constructed disjoint choice',
        citations: ['fixture'],
        opaque: false,
      })),
    ),
    events: dates.map((e, i) => ({
      ...e,
      availableDate: e.date.slice(0, 8) + (e.date.endsWith('25') ? '26' : '02'),
      minimumAge: 18,
      role: e.context === 'senato' ? 'paired-adaptation' : 'reference',
      sourceId: e.id,
      sourceUrl: 'https://example.org/' + e.id,
      sourceSha256: 'a'.repeat(64),
      availabilityBasis: 'Explicit constructed date',
      rows: geography.targets.map((t) => ({
        unit: t.code,
        electors: 100000,
        voters: 80000,
        choices: ids.map((_, j) => ({
          id: e.id + ':' + j,
          count:
            e.id === 's22'
              ? [18000, 18000, 17000, 17000][j]
              : [15000 + i * 200, 20000 - i * 200, 20000, 15000][j],
          direct: 0,
        })),
      })),
    })),
    polls: [
      {
        ...originalShareFixture().polls[0],
        id: 'poll',
        surveyId: 'poll-survey',
        date: '2026-10-15',
        fieldworkStart: '2026-10-14',
        fieldworkEnd: '2026-10-16',
        availableDate: '2026-10-17',
        original: ids.map((label) => ({ label, percentage: 25, kind: 'list' })),
        normalized: ids.map((id) => ({
          label: id,
          members: [id],
          share: 0.25,
          other: false,
        })),
      },
    ],
  };
  const encoded = await encodeSource(original),
    metadata = await compressAsset(
      'fv06-source',
      new TextEncoder().encode(canonicalJson(encoded.envelope)),
    ),
    files = new Map([
      ...encoded.assets,
      [metadata.descriptor.path, metadata.compressed],
    ]),
    read = async (path: string) => {
      const b = files.get(path);
      if (!b) throw new Error('Missing fixture ' + path);
      return b;
    };
  const scenario = accepted.scenario;
  scenario.data.sourceBundleId = encoded.envelope.id;
  scenario.historySelection = {
    electionIds: dates.filter((e) => e.context !== 'senato').map((e) => e.id),
    pairedCamera: 'c22',
    pairedSenato: 's22',
  };
  scenario.sourceMemberships = Object.fromEntries(
    original.choices.map((c) => [
      c.id,
      { members: c.members, provenance: 'Constructed correspondence' },
    ]),
  );
  scenario.polls.selected = ['poll'];
  const selection = {
    ...accepted.selection,
    referenceDate: scenario.referenceDate,
    electionIds: scenario.historySelection.electionIds,
    pollIds: scenario.polls.selected,
    sourceMemberships: scenario.sourceMemberships,
    componentMap: {},
  };
  const compiled = await compileSource(encoded.envelope, read, selection),
    paired = await compilePairedHistory(encoded.envelope, read, selection, {
      camera: 'c22',
      senato: 's22',
      electoratePolicy: 'equal-only',
    }),
    fit = fitReference(compiled.national, scenario.fitSeed, {
      solver: 'dense',
    }),
    cameraProfile = fitProfiles(compiled.profiles),
    senateAdapter = prepareSenateAdapter(
      cameraProfile,
      scenario.referenceDate,
      paired.history,
    );
  accepted.assets = {
    ...accepted.assets,
    sourceBundleId: encoded.envelope.id,
    fit,
    cameraProfile,
    senateAdapter,
  };
  accepted.selection = selection;
  accepted.nationalInputKey = compiled.nationalInputKey;
  accepted.profileInputKey = compiled.profileInputKey;
  accepted.pairedInputKey = await sourceKey('paired-input-v1', {
    date: paired.history.date,
    availableDate: paired.history.availableDate,
    minimumAge: 18,
    rows: paired.history.rows,
  });
  accepted.sourceCatalog = {
    ...accepted.sourceCatalog,
    sourceId: encoded.envelope.id,
    sourceChunks: encoded.envelope.payload.events.map((e) => e.chunk),
    assets: { ...accepted.sourceCatalog.assets, source: metadata.descriptor },
  };
  accepted.evidenceKey = await evidenceKey(scenario, selection);
  accepted.keys = await generationKeys(
    scenario,
    accepted.context,
    accepted.electorateKey,
  );
  const customPoll: SourcePoll = {
    ...structuredClone(original.polls[0]),
    id: 'user-poll',
    surveyId: 'user-survey',
    citation: 'User-provided source',
    method: 'Explicit hypothetical manual poll',
    original: ids.map((label, i) => ({
      label,
      percentage: [30, 20, 25, 25][i],
      kind: 'list',
    })),
    normalized: ids.map((id, i) => ({
      label: id,
      members: [id],
      share: [0.3, 0.2, 0.25, 0.25][i],
      other: false,
    })),
  };
  return {
    ...base,
    accepted,
    original,
    source: encoded.envelope,
    files,
    read,
    customPoll,
  };
}

/** Pinned input verification and caches, without any installed-default writes. */
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { acquireSource, readSource } from './sources';
import { shareSourceManifests } from './fast-share-history';
import { prepareFastShareSource } from './fast-share-preparation';
import { readReformManifest, prepareReformModels } from './reform-models';
import {
  sourceKey,
  decodeAsset,
  validateSourceEnvelope,
  type ByteAsset,
} from '../../src/lib/politics/reform-2026/vote-share/source-codec';
import { encodeState } from '../../src/lib/politics/reform-2026/vote-share/state-codec';
import type { CompactSource } from '../../src/lib/politics/reform-2026/vote-share/source-types';
import type {
  ReformAssemblyContext,
  ReformGenerationScenario,
} from '../../src/lib/politics/reform-2026/generation-types';
import {
  implementationKey,
  immutableJson,
  writeAssets,
  readBytes,
  readCachedState,
} from './fast-share-release-io';
export interface LegacyReleaseInputs {
  context: ReformAssemblyContext;
  defaults: ReformGenerationScenario;
  preferenceAudit: unknown;
  modelsVersion: string;
  snapshotVersion: string;
}
const reviewed = [
  'data/source-manifest.json',
  'data/reform-model-source-manifest.json',
  'data/fast-share-source-manifest.json',
  'data/sources/polls/history/elections-manifest.json',
  'data/sources/polls/archive.json',
  'data/sources/polls/entities.json',
  'data/sources/polls/manifest.json',
  'data/sources/fast-share/memberships-policy.json',
  'data/sources/fast-share/membership-sources.json',
  'data/sources/fast-share/poll-refinements.json',
];
export async function releaseInputs(
  root: string,
  sources: string,
  cache: string,
  download = false,
) {
  const share = await shareSourceManifests(root),
    reform = await readReformManifest(root),
    entries = new Map<string, (typeof reform.sources)[number]>();
  for (const s of [...share.sources, ...reform.sources]) {
    const old = entries.get(s.path);
    if (old && (old.sha256 !== s.sha256 || old.bytes !== s.bytes))
      throw new Error('Conflicting pinned source path ' + s.path);
    entries.set(s.path, s);
  }
  for (const s of entries.values()) {
    if (download) await acquireSource(sources, s);
    else await readSource(sources, s);
  }
  const review = Object.fromEntries(
    await Promise.all(
      reviewed.map(async (p) => [
        p,
        JSON.parse(await readFile(resolve(root, p), 'utf8')),
      ]),
    ),
  );
  const sourceImplementation = await implementationKey(
      root,
      'web/scripts/data-preparation/fast-share-preparation.ts',
    ),
    legalImplementation = await implementationKey(
      root,
      'web/scripts/data-preparation/reform-models.ts',
    );
  const sourceKeyId = await sourceKey('source-preparation-cache-v1', {
      review,
      sourceImplementation,
    }),
    legalKey = await sourceKey('legal-preparation-cache-v1', {
      sourceManifest: reform,
      geography: review['data/source-manifest.json'],
      legalImplementation,
    });
  type SourceCache = { parents: string; source: ByteAsset; sourceId: string };
  let saved: SourceCache | undefined;
  try {
    saved = JSON.parse(
      await readFile(resolve(cache, sourceKeyId + '.json'), 'utf8'),
    );
  } catch (e) {
    if ((e as NodeJS.ErrnoException).code !== 'ENOENT') throw e;
  }
  const hits = { source: Boolean(saved), legal: false };
  let source: CompactSource, descriptor: ByteAsset;
  if (saved) {
    if (saved.parents !== sourceKeyId)
      throw new Error('Source cache parent mismatch');
    descriptor = saved.source;
    source = JSON.parse(
      new TextDecoder().decode(
        await decodeAsset(descriptor, await readBytes(cache)(descriptor.path)),
      ),
    );
    await validateSourceEnvelope(source);
    if (source.id !== saved.sourceId)
      throw new Error('Source cache identity mismatch');
    for (const e of source.payload.events)
      await decodeAsset(e.chunk, await readBytes(cache)(e.chunk.path));
  } else {
    const prepared = await prepareFastShareSource(root, sources);
    source = prepared.envelope;
    descriptor = prepared.descriptor;
    await writeAssets(cache, prepared.assets);
    await immutableJson(resolve(cache, sourceKeyId + '.json'), {
      parents: sourceKeyId,
      source: descriptor,
      sourceId: source.id,
    });
  }
  const old = await readCachedState<{
    parents: string;
    legacy: LegacyReleaseInputs;
  }>(cache, legalKey, 'fast-share-runtime');
  let legacy: LegacyReleaseInputs;
  if (old) {
    if (old.state.parents !== legalKey)
      throw new Error('Legal cache parent mismatch');
    legacy = old.state.legacy;
    hits.legal = true;
  } else {
    const prepared = await prepareReformModels(root, sources),
      s = prepared.snapshot;
    legacy = {
      context: {
        version: s.version,
        sourceVersions: s.sourceVersions,
        assumptions: s.assumptions,
        seats: s.seats,
        models: s.models,
      },
      defaults: prepared.defaults,
      preferenceAudit: prepared.audit.preferenceFit,
      modelsVersion: prepared.models.version,
      snapshotVersion: s.version,
    };
    const encoded = await encodeState(
      'fast-share-runtime',
      { parents: legalKey, legacy },
      {
        sourceVersions: legacy.context.sourceVersions,
        methodVersions: [legalImplementation],
        assumptions: [
          'Unchanged preference/seat preparation cache; not an installed generation bundle.',
        ],
      },
    );
    await writeAssets(cache, encoded.assets);
    await immutableJson(resolve(cache, legalKey + '.json'), {
      id: encoded.envelope.id,
      descriptor: encoded.descriptor,
    });
  }
  return {
    source,
    descriptor,
    legacy,
    hits,
    sourceImplementation,
    legalImplementation,
    sourceKey: sourceKeyId,
    legalKey,
    read: readBytes(cache),
  };
}

/** Read-only CP-01 default/asset inventory. Run from web; prints JSON to stdout. */
import { readFile } from 'node:fs/promises';
import { createHash } from 'node:crypto';
import { gunzipSync } from 'node:zlib';
import { generatedDefaultPoliticsScenario as previous } from '../../src/lib/scenario/politics-defaults.generated';
import type { ReleasedScenario, ReleaseCatalog } from '../../src/lib/politics/reform-2026/vote-share/release';
import type { ByteAsset } from '../../src/lib/politics/reform-2026/vote-share/source-codec';

const root = new URL('../../', import.meta.url);
const assetRoot = new URL('static/data/reform-2026/fast-share/', root);
const digest = (b: Uint8Array) => createHash('sha256').update(b).digest('hex');
async function readState<T>(asset: ByteAsset): Promise<T> {
  const bytes = await readFile(new URL(asset.path, assetRoot));
  const decoded = gunzipSync(bytes);
  if (bytes.length !== asset.compressedBytes || decoded.length !== asset.decodedBytes ||
    digest(bytes) !== asset.sha256 || digest(decoded) !== asset.decodedSha256)
    throw new Error(`Inventory asset checksum mismatch: ${asset.path}`);
  return JSON.parse(decoded.toString()).payload.state as T;
}
const pointer = JSON.parse(await readFile(new URL('candidate.json', assetRoot), 'utf8'));
const catalog = await readState<ReleaseCatalog>(pointer.catalog);
const state = await readState<{ scenario: ReleasedScenario }>(catalog.assets.scenario);
const s = state.scenario;
const authoredHashes = await Promise.all(['fast-share-future-scenario.json', 'fast-share-default-policy.json',
  'legacy-comparison-policy.json', 'legacy-comparison-future-polls.json',
  'sources/fast-share/memberships-policy.json', 'source-manifest.json', 'workspace-source-manifest.json',
  'fast-share-source-manifest.json', 'reform-model-source-manifest.json'].map(async path => ({ path: `data/${path}`,
    sha256: digest(await readFile(new URL(`../data/${path}`, root))) })));
process.stdout.write(JSON.stringify({
  purpose: 'CP-01 observed defaults and pins, 2026-11-07; generated report, not an authored default',
  authoredHashes, pointer, assets: catalog.assets, electorateKey: catalog.electorateKey,
  previous: { id: previous.id, name: previous.name, electionDate: previous.electionDate,
    defaultSource: previous.defaultSource, globalShareMode: previous.globalShareMode,
    abstentionShare: previous.abstentionShare, abstentionOverride: previous.abstentionOverride,
    candidateGeneration: previous.candidateGeneration, listCount: previous.lists.length,
    coalitions: previous.coalitions, lists: previous.lists, localShareOverrides: previous.localShareOverrides,
    correspondenceCount: previous.listCorrespondences.length, candidateTemplates: previous.candidateTemplates },
  reform: { schemaVersion: s.schemaVersion, generation: { model: s.generation?.model,
    modelVersion: s.generation?.modelVersion, compositionCount: Object.keys(s.generation?.compositions ?? {}).length,
    supplementalPollCount: s.generation?.supplementalPolls.length },
    electionDate: s.electionDate, referenceDate: s.referenceDate, seed: s.seed, fitSeed: s.fitSeed,
    data: s.data, models: s.models, renewal: s.renewal, historySelection: s.historySelection, polls: s.polls,
    componentMapCount: Object.keys(s.componentMap).length,
    pooledToHistoricalRest: Object.values(s.componentMap).filter(id => id === 'historical-rest').length,
    components: s.components, lists: s.lists, references: s.references, nonValidReferences: s.nonValidReferences,
    candidateGeneration: s.candidateGeneration, candidateTemplates: s.candidateTemplates,
    suppliedVotes: s.suppliedVotes, suppliedPreferences: s.suppliedPreferences, bonusLinks: s.bonusLinks },
}, null, 2) + '\n');

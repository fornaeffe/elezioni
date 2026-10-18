import { fileURLToPath } from 'node:url';
import { resolve } from 'node:path';
import { acquireSource, sha256, writeVersion } from './sources';
import { prepareTerritoryCatalog, readManifest } from './normalize';
import { buildProvisionalSeatTables, seatTableMessages } from '../../src/lib/politics/reform-2026/endowments';
import { createSeededDrawContext } from '../../src/lib/politics/reform-2026/ties';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const [command, ...args] = process.argv.slice(2);
const option = (name: string, fallback: string) => args.includes(name) ? args[args.indexOf(name) + 1] ?? fallback : fallback;
const manifest = await readManifest(root);
const sources = resolve(option('--sources', resolve(root, 'data/sources')));
if (command === 'download') {
  const only = option('--only', '');
  const selected = manifest.sources.filter(s => !only || s.id === only);
  if (!selected.length) throw new Error(`Unknown source ${only}`);
  for (const source of selected) { await acquireSource(sources, source); console.log(`Verified ${source.id}`); }
} else if (command === 'import') {
  const entry = manifest.sources.find(s => s.id === args[0]);
  if (!entry || !args[1]) throw new Error('Usage: data:import -- SOURCE_ID LOCAL_FILE [--sources DIRECTORY]');
  await acquireSource(sources, entry, args[1]);
} else if (command === 'generate') {
  const { catalog, audit } = await prepareTerritoryCatalog(root, manifest, sources);
  const { municipalities: _municipalities, memberships: _memberships, ...geography } = catalog;
  const seed = 'r26-seat-tables-2021-v1';
  const tableVersion = `r26-seats-census-2021-i08-v1-${sha256(JSON.stringify({ geography, seed })).slice(0, 12)}`;
  const bundle = buildProvisionalSeatTables(geography, tableVersion, seed, createSeededDrawContext(seed, 'N03-seat-tables-v1'));
  const output = resolve(option('--output', resolve(root, 'data/derived')));
  await writeVersion(resolve(output, `${catalog.version}.json`), catalog);
  await writeVersion(resolve(output, `${catalog.version}-audit.json`), audit);
  await writeVersion(resolve(output, `${bundle.version}.json`), bundle);
  // Explicit publication only: independent of application scenario defaults and old-law snapshots.
  if (args.includes('--publish')) await writeVersion(resolve(root, `web/src/lib/politics/reform-2026/data/${bundle.version}.json`), bundle);
  console.log(JSON.stringify({ audit, tableVersion: bundle.version, sha256: sha256(JSON.stringify(bundle)),
    messages: seatTableMessages(bundle) }, null, 2));
} else throw new Error('Usage: data:download | data:import -- ID FILE | data:generate -- [--sources DIR] [--output DIR] [--publish]');

import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { prepareSelectedPolls } from './polls';
import { sha256, writeVersion } from './sources';
import { reconcilePoll } from '../../src/lib/polls/reconciliation';
import { getBundledReformSeatTables } from '../../src/lib/politics/reform-2026/bundled-data';
import { requireReformScenario } from '../../src/lib/politics/reform-2026/generation-scenario';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const [command, ...args] = process.argv.slice(2);
if (!['generate', 'check'].includes(command)) throw new Error('Usage: data:polls:generate | data:polls:check [--sources DIR] [--output DIR] [--scenario REFORM_JSON]');
const options = new Map<string, string>();
for (let i = 0; i < args.length; i += 2) {
  if (!['--sources', '--output', '--scenario'].includes(args[i]) || !args[i + 1] || args[i + 1].startsWith('--') || options.has(args[i])) throw new Error(`Invalid poll option ${args[i]}`);
  options.set(args[i], args[i + 1]);
}
const prepared = await prepareSelectedPolls(root, options.has('--sources') ? resolve(options.get('--sources')!) : undefined);
const dataDirectory = resolve(root, 'web/src/lib/politics/reform-2026/data');
const catalog = JSON.parse(await readFile(resolve(dataDirectory, 'generation-catalog.generated.json'), 'utf8'));
const defaults = requireReformScenario(JSON.parse(await readFile(resolve(dataDirectory, catalog.defaultsPath), 'utf8')));
const scenario = options.has('--scenario') ? requireReformScenario(JSON.parse(await readFile(resolve(options.get('--scenario')!), 'utf8'))) : defaults;
const geography = getBundledReformSeatTables().geography;
const historicalLists = [...new Map([...defaults.lists.camera, ...defaults.lists.senato].map(l => [l.id, { id: l.id, name: l.name }])).values()];
const reconciliations = (scenario.renewal === 'joint' ? ['camera', 'senato'] as const : [scenario.renewal]).flatMap(chamber => {
  const ordinaryIds = new Set(geography.colleges.filter(c => c.chamber === chamber && c.kind === 'ordinary').map(c => c.id));
  const eligible = new Set(scenario.coverage.filter(c => c.chamber === chamber && ordinaryIds.has(c.collegeId)).flatMap(c => c.listIds));
  const lists = scenario.lists[chamber].filter(l => eligible.has(l.id)).map(l => ({ id: l.id, name: l.name }));
  const correspondences = scenario.correspondences.filter(c => c.chamber === chamber && eligible.has(c.futureListId) && c.factor > 0);
  // The small tracked defaults are enough to demonstrate historical identity
  // reuse. No ignored production snapshot/cache or fitting is required. Unknown
  // historical IDs still require an explicit name in a future richer adapter.
  for (const c of correspondences) if (!historicalLists.some(l => l.id === c.pastListId)) throw new Error(`Unknown historical list name: ${c.pastListId}`);
  return prepared.bundle.questions.map(q => ({ chamber, ...reconcilePoll(q, prepared.bundle.entityCatalog, { lists, historicalLists, correspondences }) }));
});
const body = { schemaVersion: 1, kind: 'poll-reconciliation-preview', archiveVersion: prepared.bundle.version,
  scenarioSha256: sha256(JSON.stringify(scenario)), scope: 'ordinary-filed-lists', reconciliations,
  assumptions: ['Preparation preview only: scenario overrides, turnout, special-territory votes and live generation are unchanged.',
    'No historical vote totals are needed by this clean-clone pilot; missing residual/split weights use labelled equal estimates.',
    'Territorial prior recipes are recorded for substep 2 and are not municipal observations.',
    'Conditional questions are separate alternatives; this preview does not select or average them.'] };
const preview = { ...body, version: `poll-reconciliation-v1-${sha256(JSON.stringify(body)).slice(0, 12)}` };
if (command === 'generate') {
  const output = resolve(options.get('--output') ?? resolve(root, 'data/derived/polls'));
  await writeVersion(resolve(output, `${prepared.bundle.version}.json`), prepared.bundle);
  await writeVersion(resolve(output, `${prepared.bundle.version}-audit.json`), prepared.audit);
  await writeVersion(resolve(output, `${preview.version}.json`), preview);
}
console.log(JSON.stringify({ ...prepared.audit, previewVersion: preview.version, reconciliations: reconciliations.length,
  estimatedAssignments: reconciliations.reduce((n, r) => n + r.assignments.filter(a => a.estimated).length, 0),
  liveReferencesApplied: false, generated: command === 'generate' }, null, 2));

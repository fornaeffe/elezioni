/** Offline source inventory and reviewable membership preparation. */
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { loadGenerationHistory } from './generation-history';
import { loadMembershipPolicy, buildMembershipTable } from './party-drift-memberships';
import { sha256 } from './sources';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const command = process.argv[2];
if (!['inventory', 'generate'].includes(command)) throw new Error('Usage: party-drift-memberships-cli inventory|generate');
const history = await loadGenerationHistory(root);
if (command === 'generate') {
  const inputs = await loadMembershipPolicy(root), table = buildMembershipTable(history, inputs.policy);
  const version = 'membership-table-' + sha256(JSON.stringify({ table, versions: [...history.sourceVersions, ...inputs.versions] })).slice(0, 12);
  const dir = resolve(root, 'data/derived/party-drift', version); await mkdir(dir, { recursive: true });
  await writeFile(resolve(dir, 'memberships.json'), JSON.stringify({ version, versions: [...history.sourceVersions, ...inputs.versions], ...table }, null, 2) + '\n');
  const lines = ['# Party-drift membership table for review', '', 'Generated from the pinned electoral archives and the curated research policy. Status: **provisional**. No fixed vote fractions, inferred counts or production defaults are written.', '',
    `Version: \`${version}\`. ${table.rows.length} source-list/election rows; ${table.components.length} statistical components in the common research domain.`, '',
    'Edit `data/sources/party-drift/memberships-policy.json` to revise the proposal, then regenerate. Rows marked atomic deliberately retain an opaque original list. Associated organizational continuities are modelling conventions, not official numerical decompositions.', '',
    '## Review assumptions', '', ...table.reviewWarnings.map(w => '- ' + w), '',
    '## Structural resolution', '', `Full-history membership rank: ${table.resolution.rank}/${table.resolution.dimensions}. This is a structural screen, not proof of statistical identification across time.`, '',
    ...table.resolution.nullDirections.map(d => '- Unresolved contrast: ' + d.map(x => x.coefficient + ' × ' + x.id).join(' + ')), '',
    '| Event | Rank | Components |', '| --- | ---: | ---: |', ...table.eventAudits.map(a => `| ${a.event} | ${a.rank} | ${a.dimensions} |`), '',
    '## Complete election memberships', '', '| Election | Original source list | Component membership | Evidence / convention |', '| --- | --- | --- | --- |'];
  for (const r of table.rows) lines.push(`| ${r.event} | ${r.label.replaceAll('|', '\\|')} | ${r.members.join(' + ')} | ${r.sources.map(id => { const s = inputs.sources.sources.find(s => s.id === id)!; return `[${id}](${s.url})`; }).join(', ')}${r.note ? '; ' + r.note : ''}${!r.usedInCommonDomain ? '; outside common research domain' : ''} |`);
  lines.push('', 'Original counts stay in the pinned archives. The derived JSON retains archive IDs/hashes, input hashes, support-date conventions, per-event resolution audits and conservation totals.', '');
  await writeFile(resolve(root, 'docs/vote-generation-party-memberships.md'), lines.join('\n'));
  console.log(JSON.stringify({ version, path: resolve(dir, 'memberships.json'), rows: table.rows.length, components: table.components.length, rank: table.resolution.rank, nullDirections: table.resolution.nullDirections, conservation: table.conservation }));
  process.exit(0);
}
const events = history.events.map(e => ({ id: e.id, date: e.date, sourceId: e.sourceId,
  sourceSha256: e.sourceSha256, lists: [...new Set(e.municipalities.flatMap(m => m.lists.map(l => l.name)))].sort() }));
await mkdir(resolve(root, 'data/derived/party-drift'), { recursive: true });
await writeFile(resolve(root, 'data/derived/party-drift/inventory.json'), JSON.stringify({ version: history.version, sourceVersions: history.sourceVersions, events }, null, 2) + '\n');
for (const e of events) console.log(JSON.stringify({ id: e.id, date: e.date, lists: e.lists }));

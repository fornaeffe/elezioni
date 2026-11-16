/** Full-component observation gate for the six-family endpoint candidates. */
import { readFile, mkdir, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createHash } from 'node:crypto';
import { readAuthoredPolitics } from '../workspaces/politics-default';
import { loadExecutionData } from '../../src/lib/politics/execution/assets';
import { commonVoteInputs } from '../../src/lib/politics/execution/inputs';
import { reformCoverage } from '../../src/lib/politics/execution/reform';
import { compileSource } from '../../src/lib/politics/reform-2026/vote-share/source-compiler';
import type { ReformAssemblyContext } from '../../src/lib/politics/reform-2026/generation-types';
import type { SeatTableBundle } from '../../src/lib/politics/reform-2026/endowments';
const root = fileURLToPath(new URL('../../../', import.meta.url));
const scenario = await readAuthoredPolitics(root);
const read = async (path: string) => new Uint8Array(await readFile(resolve(root, 'web/static', path)));
const data = await loadExecutionData(scenario, { implementationId: 'panel-acceptance', asset: p => read(p.uri), sourceChunk: read });
const context: ReformAssemblyContext = { version: '', sourceVersions: [], assumptions: [], seats: data.seats as SeatTableBundle, models: data.parameters.models };
const inputs = commonVoteInputs(scenario, data, scenario.lawSettings['politiche-reform-2026-v1'].chamberProfileId, reformCoverage(scenario, context));
const compiled = await compileSource(data.source, path => read('data/reform-2026/fast-share/' + path), inputs.selection, { polls: inputs.common.polls.added });
const events = compiled.profiles.elections.map(e => {
  const n = compiled.national.elections.find(n => n.id === e.id)!;
  const singletons = new Set(n.groups.filter(g => g.length === 1).flat());
  return { id: e.id, date: e.date, context: e.context, municipalities: e.rows.length,
    jointNationalGroups: n.groups.filter(g => g.length > 1),
    componentsWithoutSeparateNationalCount: compiled.national.components.filter(id => !singletons.has(id)),
    municipalJointGroups: e.rows.reduce((sum, r) => sum + r.groups.filter(g => g.length > 1).length, 0),
    municipalIncompleteRows: e.rows.filter(r => r.groups.length !== compiled.national.components.length || r.groups.some(g => g.length !== 1)).length };
});
const compatible = events.every(e => e.componentsWithoutSeparateNationalCount.length === 0 && e.municipalIncompleteRows === 0);
const report = { schemaVersion: 1, date: '2026-11-16', source: scenario.data.historical,
  components: compiled.national.components, events,
  M06: { status: compatible ? 'eligible-for-implementation' : 'rejected-at-full-component-law-gate',
    scope: 'The published research-v1 endpoint law requires separately observed component vectors. Six-family accuracy scores do not define a production law.',
    reason: compatible ? null : 'Joint and absent historical components prevent the declared full-component endpoint errors/covariances and local states from being calculated. A grouped-data endpoint law and new-component rule must be specified and validated before browser acceptance; fabricated historical splits are not permitted.',
    browserResourceGate: 'not-entered', offeredInUI: false,
    researchDefinitionSha256: createHash('sha256').update(await readFile(resolve(root, 'research/vote-generation/campaign-2026-11/models.md'))).digest('hex') } };
const output = resolve(root, 'data/derived/model-acceptance-2026-11'); await mkdir(output, { recursive: true });
await writeFile(resolve(output, 'full-panel.json'), JSON.stringify(report, null, 2) + '\n');
console.log(JSON.stringify({ components: report.components.length, events: events.map(e => ({ id: e.id, jointNationalGroups: e.jointNationalGroups.length, incompleteMunicipalRows: e.municipalIncompleteRows })), M06: report.M06 }));

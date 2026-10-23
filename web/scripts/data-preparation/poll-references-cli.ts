import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { gunzipSync } from 'fflate';
import { prepareSelectedPolls } from './polls';
import { sha256, writeVersion } from './sources';
import { prepareReformGeneration, generateReformSimulation } from '../../src/lib/politics/reform-2026/generation';
import { runPoliticsReformScrutiny } from '../../src/lib/politics/reform-2026/scrutiny';
import { createPollInformedScenario } from './poll-references';
import { requireReformScenario } from '../../src/lib/politics/reform-2026/generation-scenario';
import type { ReformGenerationSnapshot } from '../../src/lib/politics/reform-2026/generation-types';
import { preparePollCalibration } from './poll-calibration';
const root = fileURLToPath(new URL('../../../', import.meta.url));
const args = process.argv.slice(2), options = new Map<string, string>();
for (let i = 0; i < args.length; i += 2) {
  if (!['--scenario', '--date', '--ballot', '--output'].includes(args[i]) || !args[i + 1] || options.has(args[i])) throw new Error('Usage: data:polls:references [--scenario JSON] [--date YYYY-MM-DD] [--ballot LABEL] [--output DIR]');
  options.set(args[i], args[i + 1]);
}
const data = resolve(root, 'web/src/lib/politics/reform-2026/data');
const catalog = JSON.parse(await readFile(resolve(data, 'generation-catalog.generated.json'), 'utf8'));
const bytes = await readFile(resolve(root, 'web/static', catalog.path));
if (sha256(bytes) !== catalog.sha256) throw new Error('Production snapshot checksum mismatch');
const raw = gunzipSync(bytes);
if (sha256(raw) !== catalog.uncompressedSha256) throw new Error('Production snapshot content checksum mismatch');
const snapshot: ReformGenerationSnapshot = JSON.parse(new TextDecoder().decode(raw));
const scenario = requireReformScenario(JSON.parse(await readFile(options.has('--scenario') ? resolve(options.get('--scenario')!) : resolve(data, catalog.defaultsPath), 'utf8')), snapshot);
const { bundle } = await prepareSelectedPolls(root);
const calibration = await preparePollCalibration(root);
const result = createPollInformedScenario(scenario, snapshot, bundle, options.get('--date') ?? scenario.projectionDate, options.get('--ballot') ?? null,
  { version: calibration.version, accepted: calibration.gate.accepted, reasons: calibration.gate.reasons });
const generated = generateReformSimulation(prepareReformGeneration(result.scenario, snapshot), 1);
const scrutiny = runPoliticsReformScrutiny(generated.input);
if (scrutiny.stages.some(s => s.status === 'non-computable')) throw new Error('Poll-informed production vertical slice is non-computable');
const output = resolve(options.get('--output') ?? resolve(root, 'data/derived/polls'));
await writeVersion(resolve(output, `${result.report.version}-report.json`), result.report);
await writeVersion(resolve(output, `${calibration.version}.json`), calibration);
const scenarioVersion = sha256(JSON.stringify(result.scenario)).slice(0, 12);
const path = resolve(output, `${result.report.version}-scenario-${scenarioVersion}.json`);
await writeVersion(path, result.scenario);
console.log(JSON.stringify({ version: result.report.version, applied: result.report.applied, scenarioPath: path,
  selected: result.scenario.pollReferences?.sourceQuestionIds ?? [], calibrated: calibration.gate.accepted,
  calibrationVersion: calibration.version, calibrationReasons: calibration.gate.reasons }, null, 2));

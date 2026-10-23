import { readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { downloadCalibrationSources, generateCalibrationResults } from './poll-calibration-sources';
import { preparePollCalibration } from './poll-calibration';
import { sha256, writeVersion } from './sources';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const [command, ...args] = process.argv.slice(2);
if (args.length || !['check', 'generate', 'download', 'verify-results', 'freeze-results'].includes(command)) throw new Error('Usage: poll-calibration-cli check|generate|download|verify-results|freeze-results');
if (command === 'download') await downloadCalibrationSources(root);
else if (command === 'freeze-results' || command === 'verify-results') {
  const value = await generateCalibrationResults(root), directory = resolve(root, 'data/sources/polls/history');
  const text = `${JSON.stringify(value, null, 2)}\n`;
  if (command === 'freeze-results') {
    // Deliberate maintainer-only refresh of a reviewed source table; ordinary
    // generation never changes tracked data or contacts remote services.
    await writeFile(resolve(directory, 'results.json'), text);
    await writeFile(resolve(directory, 'results-pin.json'), `${JSON.stringify({ id: 'camera-european-calibration-results-v1', path: 'results.json',
      url: 'https://dait.interno.gov.it/elezioni/open-data', headers: {}, sha256: sha256(text), bytes: Buffer.byteLength(text),
      manifestSha256: sha256(await readFile(resolve(directory, 'elections-manifest.json'))) }, null, 2)}\n`);
  } else if (await readFile(resolve(directory, 'results.json'), 'utf8') !== text) throw new Error('Frozen election targets differ from official-source regeneration');
  console.log(JSON.stringify({ command, sources: value.results.length, verified: true }));
} else {
  const report = await preparePollCalibration(root);
  if (command === 'generate') await writeVersion(resolve(root, `data/derived/polls/${report.version}.json`), report);
  console.log(JSON.stringify({ version: report.version, elections: report.elections.length, surveys: report.elections.reduce((n, e) => n + e.sources.length, 0),
    archiveBytes: report.archiveBytes, metrics: report.metrics, cameraMetrics: report.byKind.camera, gate: report.gate }, null, 2));
}

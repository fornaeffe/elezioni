import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { loadGenerationHistory } from './generation-history';
import { writeVersion } from './sources';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const command = process.argv[2];
if (!['check', 'generate'].includes(command) || process.argv.length !== 3) throw new Error('Usage: generation-history-cli check|generate');
const history = await loadGenerationHistory(root);
const output = resolve(root, `data/derived/generation/${history.version}.json`);
if (command === 'generate') await writeVersion(output, history);
console.log(JSON.stringify({ version: history.version, ...(command === 'generate' ? { output } : {}),
  events: history.events.map(e => ({ id: e.id, date: e.date, municipalities: e.municipalities.length,
    national: e.national, unmatchable: e.audit.unmatchable, excluded: e.audit.excluded })),
  transitions: history.transitions.map(t => ({ from: t.from, to: t.to, matched: t.matched,
    baselineValidVoteFraction: t.baselineValidVoteFraction, targetValidVoteFraction: t.targetValidVoteFraction })) }, null, 2));

/** Replay the frozen legacy exceptions after the reviewed TAA admission change.
 * Keep the original benchmark/diagnostic receipts immutable. Run from web/. */
import { createHash } from 'node:crypto';
import { readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { prepareReformGeneration, generateReformSimulation } from '../../src/lib/politics/reform-2026/generation';
import { runPoliticsReformScrutiny } from '../../src/lib/politics/reform-2026/scrutiny';

interface FrozenVariant {
  variant: 'polls' | 'no-polls';
  inputSha256: string;
  result: { ledger: { id: number; error?: string }[] };
}
const root = resolve('..');
const dir = resolve(root, 'data/derived/legacy-comparison');
const evidencePath = 'test/fixtures/benchmarks/vote_share_legacy_comparison.json';
const baselineBytes = await readFile(resolve(root, evidencePath));
const baseline = JSON.parse(baselineBytes.toString()) as {
  future: FrozenVariant[];
  failureDiagnostics: { generatedInputSha256: string; nonComputableReplay: { generatedInputSha256: string } };
};
const digest = (bytes: string | Uint8Array) => createHash('sha256').update(bytes).digest('hex');
const codePaths = [
  'web/src/lib/politics/reform-2026/taa-allocation.ts',
  'web/src/lib/politics/reform-2026/recovery-communications.ts',
  'web/src/lib/politics/reform-2026/recovery.ts',
  'web/src/lib/politics/reform-2026/scrutiny.ts',
  'web/src/lib/politics/reform-2026/generation.ts',
  'web/src/lib/politics/reform-2026/paired-vote-generation.ts',
  'web/scripts/acceptance/taa-admission-replay.ts'
];
const codeHashes = Object.fromEntries(await Promise.all(codePaths.map(async path =>
  [path, digest(await readFile(resolve(root, path)))])));
const exceptions = baseline.future.flatMap(v => v.result.ledger.filter(r => r.error).map(r => ({ variant: v.variant, id: r.id })));
if (exceptions.length !== 20 || baseline.future.some(v => v.result.ledger.some(r =>
  r.error && r.error !== 'RangeError: Invalid recovery seat or retained election'))) {
  throw new Error('Expected the original 20 recovery exceptions');
}
const rows = [];
let failures = 0;
for (const variant of baseline.future) {
  const bytes = await readFile(resolve(dir, `future-${variant.variant}.json`));
  if (digest(bytes) !== variant.inputSha256) throw new Error(`Frozen input changed: ${variant.variant}`);
  const payload = JSON.parse(bytes.toString());
  const prepared = prepareReformGeneration(payload.scenario, payload.snapshot);
  const ids = exceptions.filter(r => r.variant === variant.variant).map(r => r.id);
  // Separate control: preserve the known Senate N21 failure, not a TAA exception.
  if (variant.variant === 'polls') ids.push(135);
  for (const id of ids) {
    const control = id === 135 && variant.variant === 'polls';
    try {
      const generated = generateReformSimulation(prepared, id);
      const generatedInputSha256 = digest(JSON.stringify(generated.input));
      const previousHash = variant.variant !== 'polls' ? undefined : id === 23
        ? baseline.failureDiagnostics.generatedInputSha256 : control
          ? baseline.failureDiagnostics.nonComputableReplay.generatedInputSha256 : undefined;
      if (previousHash && generatedInputSha256 !== previousHash) throw new Error('Generated input changed');
      const result = runPoliticsReformScrutiny(generated.input);
      const filled = result.seats.filter(s => s.status === 'filled');
      const uniqueCandidates = new Set(filled.map(s => s.candidateId)).size;
      const chamberSeats = result.chambers.map(c => ({ chamber: c.chamber,
        seats: result.seats.filter(s => s.entitlement.chamber === c.chamber).length }));
      const camera = result.chambers.find(c => c.chamber === 'camera')!;
      const taa = camera.taa;
      const admission = camera.admission;
      const eligibilityConsistent = taa && taa.status !== 'non-computable' && admission?.status === 'complete' &&
        taa.listEntitlements.every(t => admission.lists.some(l => l.listId === t.listId && l.admitted) ||
          admission.taaLocalExceptions.some(e => e.territoryId === taa.territoryId && e.listId === t.listId));
      const nonComputableStages = result.stages.filter(s => s.status === 'non-computable');
      const passed = Boolean(eligibilityConsistent && (control
        ? result.status === 'non-computable' && filled.length === 399 &&
          nonComputableStages.some(s => s.status === 'non-computable' && s.norm === 'N21' && s.chamber === 'senato' && s.reason === 'impossible-residual')
        : result.status === 'complete' && result.seats.length === 588 && filled.length === 588 && uniqueCandidates === 588 &&
          chamberSeats.some(c => c.chamber === 'camera' && c.seats === 392) &&
          chamberSeats.some(c => c.chamber === 'senato' && c.seats === 196)));
      const row = { variant: variant.variant, id, control, inputSha256: variant.inputSha256, generatedInputSha256,
        matchedPreviousGeneratedHash: previousHash ? true : null, passed, status: result.status,
        seats: result.seats.length, filled: filled.length, uniqueCandidates, chamberSeats, eligibilityConsistent,
        nonComputableStages,
        taaEntitlements: taa && taa.status !== 'non-computable'
          ? taa.listEntitlements.map(({ listId, votes, seats }) => ({ listId, votes, seats })) : [] };
      rows.push(row);
      if (!passed) failures++;
      console.log(`${variant.variant} ${id}${control ? ' (Senate control)' : ''}: ${result.status}, ${filled.length} filled, ${passed ? 'PASS' : 'FAIL'}`);
    } catch (error) {
      failures++;
      rows.push({ variant: variant.variant, id, control, inputSha256: variant.inputSha256, passed: false, error: String(error) });
      console.log(`${variant.variant} ${id}: FAIL ${String(error)}`);
    }
  }
}
const report = { schemaVersion: 1, scope: 'TAA admitted-member interpretation adopted 2026-10-30',
  method: 'Node replay from frozen scenarios/snapshots with production generation and scrutiny; original receipts preserved.',
  baseline: { path: evidencePath, sha256: digest(baselineBytes) }, codeHashes,
  exceptions: exceptions.length, passedExceptions: rows.filter(r => !r.control && r.passed).length,
  failures, rows };
await writeFile(resolve(root, 'test/fixtures/benchmarks/taa_admission_replay.json'), JSON.stringify(report, null, 2) + '\n');
if (failures) process.exitCode = 1;

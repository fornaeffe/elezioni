/** Read-only default-scenario audit; stdout is a small reproducible report.
 * Run from web/: npx tsx --tsconfig tsconfig.node.json scripts/acceptance/correspondence-policy-audit.ts
 * Required production assets fail explicitly when absent. No acceptance receipt is rewritten.
 */
import { readFile } from 'node:fs/promises';
import { loadCandidate, loadCandidateSource } from '../../src/lib/politics/reform-2026/vote-share/release';
import { compileLegacy } from './archive/legacy-adapter-v1';
import { preparePairedVoteReferences } from '../../src/lib/politics/reform-2026/paired-vote-generation';
import { aggregateReformCollegeVotes } from '../../src/lib/politics/reform-2026/generation';
import { apportion, generationRng } from '../../src/lib/politics/reform-2026/generation-math';
import { readBytes } from '../data-preparation/fast-share-release-io';

const pointer = JSON.parse(await readFile('static/data/reform-2026/fast-share/candidate.json', 'utf8'));
const read = readBytes('static/data/reform-2026/fast-share');
const loaded = await loadCandidate(pointer, read, { validateGeneration: false });
const source = await loadCandidateSource(loaded.catalog, read);
const s = structuredClone(loaded.scenario);
if (!s.generation) throw new Error('v5 generation settings required');
s.generation.model = 'legacy-poll-informed-v1';
const legacy = await compileLegacy(s, source, read, loaded.assets, loaded.context);
const baseline = structuredClone(legacy.scenario);
baseline.shares = [];
baseline.abstention = [];
delete baseline.pollReferences;
const prepared = preparePairedVoteReferences(baseline, legacy.snapshot);
const original = structuredClone(baseline);
delete original.projectionPolicy;
const archived = preparePairedVoteReferences(original, legacy.snapshot);
const chambers = (['camera', 'senato'] as const).map(ch => {
  const filing = new Map<string, Set<string>>();
  for (const c of baseline.coverage.filter(c => c.chamber === ch))
    for (const m of legacy.snapshot.memberships.filter(m => m.chamber === ch && m.collegeId === c.collegeId))
      filing.set(m.municipalityCode, new Set([...(filing.get(m.municipalityCode) ?? []), ...c.listIds]));
  const units = prepared.units;
  let valid = 0, retained = 0, duplicatedMunicipalities = 0, incompleteMunicipalities = 0;
  const losses: { code: string; lost: number; valid: number }[] = [];
  for (const u of units) {
    const v = u.municipality[ch].votes.reduce((n, v) => n + v.votes, 0);
    const kept = u.municipality[ch].votes.reduce((n, v) => n + v.votes * baseline.correspondences
      .filter(c => c.chamber === ch && c.pastListId === v.listId && filing.get(u.municipality.code)?.has(c.futureListId))
      .reduce((n, c) => n + c.factor, 0), 0);
    valid += v; retained += kept;
    if (kept > v + 1e-6) duplicatedMunicipalities++;
    if (kept < v - 1e-6) { incompleteMunicipalities++; losses.push({ code: u.municipality.code, lost: v - kept, valid: v }); }
  }
  const totals = (field: typeof units) => baseline.lists[ch].map(l => ({ id: l.id,
    votes: field.reduce((n, u) => n + u.municipality[ch].electors * (1 - u[ch].abstention) * (u[ch].shares.get(l.id) ?? 0), 0) }));
  const before = new Map(totals(archived.units).map(r => [r.id, r.votes]));
  const after = totals(units).map(r => ({ ...r, deltaFromOriginalArchivePolicy: r.votes - before.get(r.id)! }));
  const electors = units.reduce((n, u) => n + u.municipality[ch].electors, 0);
  const oldValid = archived.units.reduce((n, u) => n + u.municipality[ch].electors * (1 - u[ch].abstention), 0);
  const collegeCounts = (field: typeof units) => aggregateReformCollegeVotes(baseline, legacy.snapshot,
    field.map(u => {
      const electors = u.municipality[ch].electors;
      const shares = [...u[ch].shares];
      const counts = apportion(electors, [...shares.map(([, p]) => (1 - u[ch].abstention) * p), u[ch].abstention].map(p => Math.round(p * 1e12)),
        generationRng('correspondence-audit-rounding', 0, ch, u.municipality.code));
      return { chamber: ch, municipalityCode: u.municipality.code, electors,
        abstention: counts.at(-1)!, votes: shares.map(([listId], i) => ({ listId, votes: counts[i] })) };
    }), 0);
  const oldColleges = collegeCounts(archived.units), newColleges = collegeCounts(units);
  const collegeDeltas = [...newColleges].flatMap(([id, c]) => baseline.lists[ch].map(l => ({
    collegeId: id, listId: l.id, delta: (c.votes.get(l.id) ?? 0) - (oldColleges.get(id)?.votes.get(l.id) ?? 0),
  }))).sort((a, b) => Math.abs(b.delta) - Math.abs(a.delta)).slice(0, 8);
  // Count actual physical split municipalities and differing filed ticket sets.
  const physical = new Set(legacy.snapshot.seats.geography.colleges.filter(c => c.chamber === ch && c.kind !== 'taa-proportional').map(c => c.id));
  let splitMunicipalities = 0, varyingFilingMunicipalities = 0;
  for (const u of units) {
    const colleges = [...new Set(legacy.snapshot.memberships.filter(m => m.chamber === ch && m.municipalityCode === u.municipality.code && physical.has(m.collegeId)).map(m => m.collegeId))];
    if (colleges.length > 1) splitMunicipalities++;
    const tickets = new Set(colleges.map(id => JSON.stringify([...(baseline.coverage.find(c => c.chamber === ch && c.collegeId === id)?.listIds ?? [])].sort())));
    if (tickets.size > 1) varyingFilingMunicipalities++;
  }
  return { chamber: ch, municipalities: units.length, electors, historicalValid: valid,
    rawMappedFiledMass: retained, rawMissingMass: valid - retained, incompleteMunicipalities,
    duplicatedMunicipalities, splitMunicipalities, varyingFilingMunicipalities,
    historicalNonValid: 1 - valid / electors, originalPolicyNonValid: 1 - oldValid / electors,
    fallbackMunicipalities: prepared.trace.warnings.filter(w => w.code === 'LEGACY_UNMAPPED_POLITICAL_PROXY' && w.message.startsWith(ch + '/')).length,
    equalFallbackMunicipalities: prepared.trace.warnings.filter(w => w.code === 'LEGACY_UNMAPPED_POLITICAL_PROXY' && w.message.startsWith(ch + '/') && w.message.includes('uniforme')).length,
    largestLosses: losses.sort((a, b) => b.lost - a.lost).slice(0, 5), lists: after,
    largestCollegeDeltas: collegeDeltas };
});
console.log(JSON.stringify({ kind: 'post-fv08-correspondence-policy-audit-v1', catalogId: pointer.catalogId,
  sourceId: source.id, legacyInputKey: legacy.inputKey,
  scope: 'Deterministic historical preparation before polls/manual references; not a simulation or acceptance campaign.',
  baseline: legacy.audit.baseline, missingHistory: legacy.audit.missingHistory.length, chambers }, null, 2));

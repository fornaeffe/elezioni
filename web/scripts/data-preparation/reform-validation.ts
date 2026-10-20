import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { resolve } from 'node:path';
import { gunzipSync } from 'fflate';
import { sha256, writeVersion } from './sources';
import { generationFixture } from '../../src/lib/politics/reform-2026/generation.fixture';
import { preparePairedVoteReferences, generatePairedMunicipalVotes, pairedStandardShock } from '../../src/lib/politics/reform-2026/paired-vote-generation';
import { generateSyntheticPreferences, preferenceParametersForRegion } from '../../src/lib/politics/reform-2026/preference-generation';
import { runPoliticsReformScrutiny } from '../../src/lib/politics/reform-2026/scrutiny';
import { createDefaultReformScenario } from '../../src/lib/politics/reform-2026/generation-scenario';
import { mean, sum, variance } from '../../src/lib/politics/reform-2026/generation-math';
import type { ReformGenerationSnapshot } from '../../src/lib/politics/reform-2026/generation-types';
import type { ChamberInput, ReformScrutinyInput } from '../../src/lib/politics/reform-2026/input';
import { completeScrutinyFixture } from '../../src/lib/politics/reform-2026/scrutiny.fixture';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const outputIndex = process.argv.indexOf('--output');
if (outputIndex >= 0 && !process.argv[outputIndex + 1]) throw new Error('--output requires a directory');
const outputDirectory = outputIndex < 0 ? resolve(root, 'data/derived') : resolve(process.argv[outputIndex + 1]);
const catalog = JSON.parse(await readFile(resolve(root, 'web/src/lib/politics/reform-2026/data/generation-catalog.generated.json'), 'utf8'));
const bytes = await readFile(resolve(root, 'web/static', catalog.path));
if (sha256(bytes) !== catalog.sha256) throw new Error('Reform runtime checksum mismatch');
const production: ReformGenerationSnapshot = JSON.parse(new TextDecoder().decode(gunzipSync(bytes)));
const { snapshot } = generationFixture(); snapshot.models = structuredClone(production.models);
const scenario = createDefaultReformScenario(snapshot, '2026-10-19', '2027-09-25', 'r26-19-validation-0');
// Vote/candidate conditioning is fixed for all preference profiles and seeds.
const fixed = completeScrutinyFixture(45);
const people = new Map(fixed.candidates.map(p => [p.id, p]));
function withPreferences(profileId: string, seed: string): ReformScrutinyInput {
  const profile = snapshot.models.preferences.profiles.find(p => p.id === profileId)!;
  function chamber(c: ChamberInput): ChamberInput {
    return { ...c, ordinarySlates: c.ordinarySlates.map(slate => {
      const row = c.ordinaryVotes.find(r => r.collegeId === slate.collegeId)!;
      const college = c.colleges.find(p => p.id === slate.collegeId)!;
      const budget = college.kind === 'taa-proportional' ? sum(c.specialVotes.filter(r => c.colleges.find(p => p.id === r.collegeId)?.territoryId === college.territoryId)
        .flatMap(r => (r.votes.kind === 'attributed' ? r.votes.listVotes : r.votes.directListVotes).filter(l => l.listId === slate.listId).map(l => l.votes)))
        : (row.votes.kind === 'attributed' ? row.votes.listVotes : row.votes.directListVotes).find(r => r.listId === slate.listId)!.votes;
      const territory = c.territories.find(t => t.id === c.colleges.find(p => p.id === slate.collegeId)!.territoryId)!;
      return { ...slate, preferences: generateSyntheticPreferences({ seed, simulationId: 1, chamber: c.chamber, collegeId: slate.collegeId,
        listId: slate.listId, eligible: slate.candidateIds.slice(1).map(id => people.get(id)!), ballotBudget: budget,
        parameters: preferenceParametersForRegion(snapshot.models, territory.regionId), profile,
        provenance: { kind: 'generated', modelId: snapshot.models.modelIds.preferences, parameterVersion: snapshot.models.preferences.parameterVersion,
          profileId, sourceVersions: snapshot.models.sourceVersions, seed, assumptions: snapshot.models.assumptions } }) };
    }) };
  }
  return { ...fixed, randomness: { ...fixed.randomness, seed }, renewal: fixed.renewal.kind === 'joint'
    ? { kind: 'joint', camera: chamber(fixed.renewal.camera), senato: chamber(fixed.renewal.senato) }
    : { kind: 'single', chamber: chamber(fixed.renewal.chamber) } };
}
const profiles = snapshot.models.preferences.profiles;
const preferenceRows: Record<string, { filled: number; unfilled: number; unresolved: number; recoveries: number; candidateChanges: number; entitlementChanges: number; nonComputable: number }[]> = Object.fromEntries(profiles.map(p => [p.id, []]));
for (let i = 0; i < 100; i++) {
  const seed = `r26-19-validation-${i}`;
  let reference: ReturnType<typeof runPoliticsReformScrutiny> | undefined;
  for (const profile of profiles) {
    const result = runPoliticsReformScrutiny(withPreferences(profile.id, seed));
    if (result.stages.some(s => s.status === 'non-computable') || result.seats.length !== 588) throw new Error('Preference conditioning fixture must execute the complete scrutiny');
    reference ??= result;
    const baseline = new Map(reference.seats.map(s => [s.seatId, s]));
    const candidateChanges = result.seats.filter(s => {
      const base = baseline.get(s.seatId);
      return s.status === 'filled' && base?.status === 'filled' && s.candidateId !== base.candidateId;
    }).length;
    const entitlementChanges = result.seats.filter(s => JSON.stringify(s.entitlement) !== JSON.stringify(baseline.get(s.seatId)?.entitlement)).length;
    if (entitlementChanges) throw new Error('Preferences changed fixed vote-based seat entitlements');
    preferenceRows[profile.id].push({ filled: result.seats.filter(s => s.status === 'filled').length,
      unfilled: result.seats.filter(s => s.status === 'unfilled').length, unresolved: result.seats.filter(s => s.status === 'unresolved').length,
      recoveries: result.recoveries.length, candidateChanges, entitlementChanges, nonComputable: result.stages.filter(s => s.status === 'non-computable').length });
  }
  if ((i + 1) % 10 === 0) console.log(`Preference paired legal validation ${i + 1}/100`);
}
const preferenceSummary = Object.entries(preferenceRows).map(([id, rows]) => ({ id, replicates: rows.length,
  ...Object.fromEntries(Object.keys(rows[0]).map(k => {
    const xs = rows.map(r => r[k as keyof typeof r]);
    return [k, { mean: mean(xs), mcStandardError: Math.sqrt(variance(xs) / xs.length), minimum: Math.min(...xs), maximum: Math.max(...xs) }];
  })) }));

// Small constructed paired vote footprint, using the actual fitted/assumed model
// bundle. This measures model mechanics, not calibration of national probabilities.
scenario.shares = ['A', 'B'].map(listId => ({ scope: { kind: 'national' }, listId, common: listId === 'A' ? 0.42 : 0.58, camera: null, senato: null }));
scenario.electionDate = '2026-10-20';
const prepared = preparePairedVoteReferences(scenario, snapshot);
const chamberProfiles = [...snapshot.models.chambers.profiles,
  { id: 'identical-shock-benchmark', sharedVariance: 1, residualCorrelation: 0, gapMultiplier: 0 },
  { id: 'independent-shock-benchmark', sharedVariance: 0, residualCorrelation: 0, gapMultiplier: 0 }];
const chamberSummary = [];
for (const profile of chamberProfiles) {
  const modelSnapshot = structuredClone(snapshot); modelSnapshot.models.chambers.profiles.push(profile);
  const c: number[] = [], s: number[] = [], standardC: number[] = [], standardS: number[] = [], joint: number[] = [];
  const refs = preparePairedVoteReferences({ ...scenario, chamberProfileId: profile.id }, modelSnapshot);
  for (let i = 0; i < 1000; i++) {
    const seed = `r26-19-validation-${i}`, standard = pairedStandardShock(seed, 1, profile, 'national', 'A');
    standardC.push(standard.camera); standardS.push(standard.senato);
    const rows = generatePairedMunicipalVotes({ ...scenario, seed, chamberProfileId: profile.id }, modelSnapshot, refs.units, 1);
    const shares = ['camera', 'senato'].map(ch => {
      const rs = rows.filter(r => r.chamber === ch);
      for (const r of rs) if (sum(r.votes.map(v => v.votes)) + r.abstention !== r.electors) throw new Error('Nonconserved normalized vote draw');
      return sum(rs.map(r => r.votes.find(v => v.listId === 'A')!.votes)) / sum(rs.map(r => r.electors - r.abstention));
    });
    c.push(shares[0]); s.push(shares[1]); joint.push(Number(shares[0] >= 0.42 && shares[1] >= 0.42));
  }
  const corr = (a: number[], b: number[]) => mean(a.map((v, i) => (v - mean(a)) * (b[i] - mean(b)))) / Math.sqrt(variance(a) * variance(b));
  const expectedCorrelation = profile.sharedVariance + (1 - profile.sharedVariance) * profile.residualCorrelation;
  for (const xs of [standardC, standardS]) if (Math.abs(mean(xs)) > 4 / Math.sqrt(1000) || Math.abs(variance(xs) - 1) > 0.15) throw new Error('Paired marginal moment gate failed');
  chamberSummary.push({ id: profile.id, replicates: 1000, expectedCorrelation, measuredStandardCorrelation: corr(standardC, standardS),
    normalized: { cameraMean: mean(c), cameraVariance: variance(c), senatoMean: mean(s), senatoVariance: variance(s), correlation: corr(c, s) },
    conditionalJoint42Frequency: mean(joint), jointMcStandardError: Math.sqrt(variance(joint) / 1000) });
}
const report = { schemaVersion: 1, protocol: 'docs/politics-reform-2026-generation-protocol.md', modelVersion: production.models.version,
  parameterVersions: { preferences: production.models.preferences.parameterVersion, chambers: production.models.chambers.parameterVersion },
  sourceVersions: production.sourceVersions, seeds: 'r26-19-validation-0..999',
  scope: 'Preference conditioning: independently worked completeScrutinyFixture(45), with fixed votes/candidatures and the real model bundle. Paired vote diagnostics: constructed twenty-region footprint. Production-data vertical slice is separate. Conditional joint-42 frequencies are not real-subject probabilities.',
  preferenceSummary, chamberSummary, referenceScopes: prepared.trace.references.length };
await writeVersion(resolve(outputDirectory, `${production.models.version}-validation-v2.json`), report);
console.log(JSON.stringify({ modelVersion: report.modelVersion, preferenceProfiles: preferenceSummary.length, preferenceRunsPerProfile: 100,
  chamberProfilesAndBenchmarks: chamberSummary.length, chamberRunsPerProfile: 1000,
  report: resolve(outputDirectory, `${production.models.version}-validation-v2.json`) }, null, 2));

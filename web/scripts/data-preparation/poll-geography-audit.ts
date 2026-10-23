import { generationFixture } from '../../src/lib/politics/reform-2026/generation.fixture';
import { generatePairedMunicipalVotes, preparePairedVoteReferences } from '../../src/lib/politics/reform-2026/paired-vote-generation';

/** Mechanical counterexample, not empirical geographic calibration. */
export function auditGeographicNationalDrift(simulations = 1000) {
  const { scenario, snapshot } = generationFixture();
  scenario.electionDate = '2026-10-23'; scenario.projectionDate = '2026-10-23';
  scenario.seed = 'poll-geography-audit-v1';
  const prepared = preparePairedVoteReferences(scenario, snapshot);
  const ordinary = new Set(snapshot.seats.geography.colleges.filter(c => c.chamber === 'camera' && c.kind === 'ordinary').map(c => c.id));
  const codes = new Set(snapshot.memberships.filter(m => m.chamber === 'camera' && ordinary.has(m.collegeId)).map(m => m.municipalityCode));
  const units = prepared.units.filter(u => codes.has(u.municipality.code));
  const valid = units.reduce((n, u) => n + u.municipality.camera.electors * (1 - u.camera.abstention), 0);
  const target = units.reduce((n, u) => n + u.municipality.camera.electors * (1 - u.camera.abstention) * (u.camera.shares.get('A') ?? 0), 0) / valid;
  const rows = [{ mode: 'no-effects', region: 0, local: 0 }, { mode: 'regional', region: .01, local: 0 },
    { mode: 'local', region: 0, local: .01 }, { mode: 'regional-and-local', region: .01, local: .01 }].map(mode => {
    snapshot.models.chambers.sigmaPerSqrtDay = { national: 0, region: mode.region, local: mode.local };
    const deviations = Array.from({ length: simulations }, (_, i) => {
      const result = generatePairedMunicipalVotes(scenario, snapshot, prepared.units, i + 1).filter(r => r.chamber === 'camera' && codes.has(r.municipalityCode));
      const total = result.reduce((n, r) => n + r.votes.reduce((s, v) => s + v.votes, 0), 0);
      return result.reduce((n, r) => n + (r.votes.find(v => v.listId === 'A')?.votes ?? 0), 0) / total - target;
    });
    return { ...mode, target, rmsNationalDeviation: Math.sqrt(deviations.reduce((n, x) => n + x * x, 0) / simulations),
      maximumAbsoluteDeviation: Math.max(...deviations.map(Math.abs)) };
  });
  return { version: 'poll-geography-audit-v1', simulations, fixture: snapshot.version, rows,
    limitations: ['Constructed equal electorates with one municipality per region, not measured Italian territorial error.',
      'National sigma and measurement error are zero. Regional/local party and turnout shocks retain the old historical date.',
      'Nonzero national deviations show that conditional geographic reconciliation is needed before attaching a calibrated national distribution.'] };
}

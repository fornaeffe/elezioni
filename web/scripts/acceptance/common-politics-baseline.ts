/** CP-01 test evidence only. No production assets or legal expectations are written. */
import { createHash } from 'node:crypto';
import { pathToFileURL } from 'node:url';
import { originalFitFixture } from '../../tests/fixtures/fast-fit-input';
import { compileLegacy } from '../../src/lib/politics/reform-2026/vote-share/legacy-adapter';
import { prepareScenarioGeneration } from '../../src/lib/politics/reform-2026/vote-share/unified-generation';
import type { ReleasedScenario } from '../../src/lib/politics/reform-2026/vote-share/release';
import type { ChamberInput } from '../../src/lib/politics/reform-2026/input';

// JSON encoding also makes typed arrays explicit objects with numeric keys.
const hash = (value: unknown) => createHash('sha256').update(JSON.stringify(value)).digest('hex');
const round = (n: number) => Number(n.toFixed(10));
const cases = ['joint', 'camera', 'senato', 'chamber-values', 'restricted', 'manual', 'residual-new'] as const;

export async function collectCommonPoliticsBaseline() {
  const f = await originalFitFixture();
  const records = [];
  for (const model of ['M01', 'M02'] as const) {
    for (const name of cases) {
      const s: ReleasedScenario = structuredClone(f.accepted.scenario);
      s.generation = { model, modelVersion: '2.0.0', compositions: {}, supplementalPolls: [] };
      s.models.forecast = 'bounded-mass-pair-field-v2';
      if (name === 'camera' || name === 'senato') s.renewal = name;
      if (name === 'chamber-values') {
        for (const ch of ['camera', 'senato'] as const)
          s.references.push({ id: `cp01-${ch}`, scope: { kind: 'national' }, channel: ch,
            subject: { kind: 'list', id: s.lists[ch][0].id }, mean: ch === 'camera' ? 0.55 : 0.35, sdPercentagePoints: 1 });
      }
      if (name === 'restricted') {
        // Keep special ballots populated; remove one list from a whole Camera territory.
        const college = f.accepted.context.seats.geography.colleges.find(c => c.chamber === 'camera' && c.kind === 'ordinary')!;
        const restricted = new Set(f.accepted.context.seats.geography.colleges
          .filter(c => c.chamber === 'camera' && c.territoryId === college.territoryId).map(c => c.id));
        for (const row of s.coverage.filter(c => c.chamber === 'camera' && restricted.has(c.collegeId)))
          row.listIds = [s.lists.camera[0].id];
      }
      if (name === 'residual-new') {
        s.components.push({ id: 'cp01-new', kind: 'new', provenance: 'Constructed CP-01 support', fallback: {
          geographicLogProfile: 0, profileSd: 0.45, localInnovationSd: 0.25, nationalInnovationSd: 0.35,
        } });
        for (const ch of ['camera', 'senato'] as const) {
          const residual = s.lists[ch][1].componentIds.pop()!;
          s.lists[ch].push({ id: '__other__', name: 'Altre liste', kind: 'other', coalitionId: null,
            protectedMinority: null, componentIds: [residual] });
          s.presentation.lists.push({ chamber: ch, id: '__other__', name: 'Altre liste', color: '#9ca3af' });
          s.lists[ch].push({ id: 'cp01-new-list', name: 'CP-01 new', coalitionId: null,
            protectedMinority: null, componentIds: ['cp01-new'] });
          s.coverage.filter(c => c.chamber === ch).forEach(c => c.listIds.push('cp01-new-list', '__other__'));
        }
        s.references.push({ id: 'cp01-new-reference', scope: { kind: 'national' }, channel: 'common',
          subject: { kind: 'list', id: 'cp01-new-list' }, mean: 0.1, sdPercentagePoints: 1 });
      }
      if (name === 'manual') {
        const college = f.accepted.context.seats.geography.colleges.find(c => c.chamber === 'camera' && c.kind === 'ordinary')!;
        s.candidateTemplates.push({ chamber: 'camera', role: 'ordinary', locationId: college.id,
          ownerId: s.lists.camera[0].id, position: 1, affiliationListId: s.lists.camera[0].id,
          candidate: { id: 'cp01-person', name: 'CP-01 declared person', birthDate: '1970-01-01', gender: 'female',
            availability: { kind: 'available' }, externalCandidacies: [] } });
      }
      const authoredInput = hash({ scenario: s, assets: f.accepted.assets,
        context: f.accepted.context, source: f.source });
      try {
        const legacy = model === 'M01'
          ? await compileLegacy(s, f.source, f.read, f.accepted.assets, f.accepted.context)
          : undefined;
        const prepared = prepareScenarioGeneration(s, f.accepted.assets, f.accepted.context, legacy);
        const ids = name === 'joint' ? Array.from({ length: 16 }, (_, i) => i + 1) : [7];
        const draws = ids.map(id => {
          const generated = prepared.run(id);
          const chambers: ChamberInput[] = generated.input.renewal.kind === 'joint'
            ? [generated.input.renewal.camera, generated.input.renewal.senato]
            : [generated.input.renewal.chamber];
          const collegeShares = chambers.flatMap(ch => [...ch.ordinaryVotes, ...ch.specialVotes].map(row => {
            if (row.votes.kind !== 'attributed') throw new Error('Expected attributed generated votes');
            const valid = row.votes.listVotes.reduce((n, v) => n + v.votes, 0);
            const votes = row.votes.listVotes.find(v => v.listId === s.lists[ch.chamber][0].id)?.votes ?? 0;
            return { chamber: ch.chamber, college: row.collegeId, valid,
              firstListShare: valid ? round(votes / valid) : null };
          }));
          const totals = chambers.map(ch => {
            // Camera TAA proportional rows are a second VIEW of special votes, not another ballot.
            const rows = [...ch.ordinaryVotes.filter(row =>
              f.accepted.context.seats.geography.colleges.find(c => c.id === row.collegeId)!.kind !== 'taa-proportional'),
              ...ch.specialVotes];
            const votes: Record<string, number> = {};
            let electors = 0;
            for (const row of rows) {
              electors += row.electorate.electors!;
              if (row.votes.kind !== 'attributed') throw new Error('Expected attributed generated votes');
              for (const v of row.votes.listVotes) votes[v.listId] = (votes[v.listId] ?? 0) + v.votes;
            }
            const valid = Object.values(votes).reduce((a, b) => a + b, 0);
            return { chamber: ch.chamber, electors, valid, nonValid: electors - valid, votes };
          });
          return { id, totals, collegeShares, candidateCount: generated.input.candidates.length,
            declaredCandidates: generated.input.candidates.filter(c => c.id === 'cp01-person'),
            // Exact hashes are supplementary old-traversal evidence, not the future architecture gate.
            ballotsSha256: hash(chambers.map(ch => ({ ordinary: ch.ordinaryVotes, special: ch.specialVotes }))),
            candidatureSha256: hash({ candidates: generated.input.candidates, chambers: chambers.map(ch => ({
              ordinary: ch.ordinarySlates, bonus: ch.bonusSlates, special: ch.specialCandidatures,
            })) }),
            modelVersion: generated.trace.modelVersion, warnings: generated.trace.warnings };
        });
        const summaries = draws[0].totals.map(t => {
          const values = draws.map(d => d.totals.find(v => v.chamber === t.chamber)!);
          const share = values.map(v => v.votes[s.lists[t.chamber][0].id] / v.valid);
          const mean = share.reduce((a, b) => a + b, 0) / share.length;
          return { chamber: t.chamber, firstListId: s.lists[t.chamber][0].id,
            meanValidShare: round(mean), minimum: round(Math.min(...share)), maximum: round(Math.max(...share)),
            populationSd: round(Math.sqrt(share.reduce((a, b) => a + (b - mean) ** 2, 0) / share.length)) };
        });
        records.push({ model, case: name, inputSha256: hash({ scenario: s, assets: f.accepted.assets,
          context: f.accepted.context, source: f.source, legacy }), summaries, draws });
      } catch (error) {
        // Baseline existing blockers verbatim; never reroll or weaken legal validation.
        records.push({ model, case: name, inputSha256: authoredInput,
          failure: error instanceof Error ? error.message : String(error) });
      }
    }
  }
  return { schemaVersion: 1, purpose: 'CP-01 constructed generation/candidature evidence; not fixed-input legal expectations or predictive acceptance',
    recipeRevision: 'a700d19b04272f39fad17ddd3ceb9e48048ea623',
    recipe: 'originalFitFixture; M01/M02 2.0.0; bounded-mass-pair-field-v2; cases defined in common-politics-baseline.ts',
    records };
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  process.stdout.write(JSON.stringify(await collectCommonPoliticsBaseline(), null, 2) + '\n');
}

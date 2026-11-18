/** Strict authored inputs and model release packaging. */
import { modelRegistry } from '../../src/lib/politics/reform-2026/vote-share/model-registry';
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { parsePoliticsScenario, validateActiveScenario, ScenarioValidationError } from '../../src/lib/politics/scenario/validation';
import { REFORM, ROSATELLUM, type PoliticsScenario } from '../../src/lib/politics/scenario/schema';
import { loadExecutionData, type ExecutionReaders } from '../../src/lib/politics/execution/assets';
import { commonVoteInputs } from '../../src/lib/politics/execution/inputs';
import { reformCoverage, reformLegalSettings } from '../../src/lib/politics/execution/reform';
import type { ReformAssemblyContext } from '../../src/lib/politics/reform-2026/generation-types';
import type { ReleasedScenario } from '../../src/lib/politics/reform-2026/vote-share/release';
import { requireFastScenario } from '../../src/lib/politics/reform-2026/vote-share/scenario';
import { physicalCells } from '../../src/lib/politics/reform-2026/vote-share/physical-cells';
import { rosatellumCoverage, type RosatellumSeats } from '../../src/lib/politics/execution/rosatellum';
import type { SeatTableBundle } from '../../src/lib/politics/reform-2026/endowments';

export const readAuthoredPolitics = async (root: string) => parsePoliticsScenario(JSON.parse(await readFile(resolve(root, 'data/politics-default-scenario.json'), 'utf8')));
export async function validateAuthoredPolitics(s: PoliticsScenario, readers: ExecutionReaders) {
  for (const [law, model] of [[ROSATELLUM, 'M01'], [ROSATELLUM, 'M02'], [REFORM, 'M01'], [REFORM, 'M02']] as const) {
    const active: PoliticsScenario = { ...s, selection: { scrutiny: { ...s.selection.scrutiny, id: law },
      generatorsByLaw: { ...s.selection.generatorsByLaw, [law]: { ...s.selection.generatorsByLaw[law], id: model, version: modelRegistry[model].version } } } };
    const data = await loadExecutionData(active, readers);
    const issues = validateActiveScenario(active, data.domain);
    if (issues.length) throw new ScenarioValidationError(issues);
    physicalCells(data.geography, data.electorates, ['camera', 'senato'], s.execution.simulationSeed);
    if (law === ROSATELLUM) rosatellumCoverage(active, data.seats as RosatellumSeats);
    else {
      const seats = data.seats as SeatTableBundle;
      if (seats.version !== active.data.lawAssets[law].seats.version || seats.geography.version !== data.geography.version)
        throw new Error('REFORM_SEATS_MISMATCH');
    }
  }
}
export async function reformDefaultInputs(s: PoliticsScenario, readers: ExecutionReaders, context: ReformAssemblyContext) {
  // M02 fitting is prepared even when an authored selection requests M01.
  const active: PoliticsScenario = { ...s, selection: { scrutiny: { ...s.selection.scrutiny, id: REFORM },
    generatorsByLaw: { ...s.selection.generatorsByLaw, [REFORM]: { ...s.selection.generatorsByLaw[REFORM], id: 'M02' } } } };
  const data = await loadExecutionData(active, readers);
  const inputs = commonVoteInputs(active, data, s.lawSettings[REFORM].chamberProfileId, reformCoverage(active, context));
  const legal = reformLegalSettings(active, data.domain, inputs.common);
  return { inputs, legal, data };
}
export function releasedDefault(s: PoliticsScenario, inputs: Awaited<ReturnType<typeof reformDefaultInputs>>, data: ReleasedScenario['data'], implementation: string): ReleasedScenario {
  const selected = s.selection.generatorsByLaw[REFORM], m01 = s.modelSettings.M01, m02 = s.modelSettings.M02;
  const generation = { model: selected.id, modelVersion: selected.version,
    compositions: Object.fromEntries(m01.compositions.map(c => [c.choiceId, { fractions: { ...c.fractions }, provenance: c.provenance, proxy: c.proxy }])),
    supplementalPolls: m01.supplementalPolls.map(p => ({ ...p, groups: p.groups.map(g => ({ ...g, members: [...g.members] })) })) };
  const result: ReleasedScenario = { ...inputs.inputs.fast, ...inputs.legal, lists: inputs.inputs.fast.lists, schemaVersion: 5, data,
    generation, historySelection: { electionIds: [...s.history.electionIds], pairedCamera: s.history.pairedCamera!, pairedSenato: s.history.pairedSenato! },
    sourceMemberships: inputs.inputs.common.sourceMemberships, componentMap: inputs.inputs.common.componentMap,
    polls: structuredClone(inputs.inputs.common.polls),
    models: { statistical: m02.statistical, numerical: m02.policies.numerical, references: m02.policies.references,
      chambers: m02.policies.chambers, rng: m02.policies.rng, forecast: m02.forecast, forecastScale: m02.forecastScale,
      profileRegionCorrelation: m02.profileRegionCorrelation, forecastReference: m02.forecastReference,
      generationImplementationId: implementation },
    presentation: { name: s.name, coalitions: s.subjects.coalitions.map(c => ({ ...c, chambers: [...c.chambers] })),
      components: [...s.subjects.components.map(c => ({ ...c })), ...s.newComponents.filter(c => inputs.inputs.fast.components.some(f=>f.id===c.id)).map(c => ({id:c.id,name:c.name,support:'reference' as const}))],
      lists: (['camera','senato'] as const).flatMap(chamber => s.subjects.lists.map(l => ({chamber,id:l.id,name:l.name,color:l.color}))) } };
  return requireFastScenario(result);
}
/** M00 UI compatibility until CP-06/08. This projection does not choose an independent default. */

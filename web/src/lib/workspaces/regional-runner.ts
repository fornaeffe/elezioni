import { requireSimulationCount } from '../core/runtime-data';
import { loadWorkspaceSnapshot } from './data';
import { snapshotDescriptors } from './snapshots.generated';
import { validateRegionalErScenario } from '../scenario/regional-er';
import { WorkspaceExecutionError } from './execution-error';
import { buildRegionalErDirectScrutinySnapshot } from '../regional-er/pipeline';
import { buildRegionalErResultPlotTables, buildRegionalErResultTables, summarizeRegionalErGeneratedRuns, summarizeRegionalErScrutinyRun, type RegionalErRunPresentationSummary } from '../regional-er/result-presentation';
import { projectScenarioOntoRegionalErSource } from '../regional-er/scenario-projection';
import { runRegionalErScrutiny } from '../regional-er/scrutiny';
import { buildRegionalErPipelineSourceFromSnapshot } from '../regional-er/static-snapshot';
import type { RegionalErScenarioProjectionRow } from '../regional-er/types';
import type { ResultTable, ScrutinyWarning, SimulationProgress, SimulationRequest, SimulationResult, SimulationWorkerMessage } from '../core/types';
const regionalErGeneratedChunkSize = 50;
const progress = (post: (m: SimulationWorkerMessage) => void, start: number, phase: SimulationProgress['phase'], completed: number, total: number) => post({ type: 'progress', phase, completed, total, elapsedMs: performance.now() - start });
function scenarioProjectionTable(rows: RegionalErScenarioProjectionRow[]): ResultTable {
    return {
        name: 'Scenario projection',
        columns: ['Lista', 'Fonte parametri', 'Coalizione', 'Quota scenario', 'Usata', 'Quota proiettata', 'Stato'],
        rows: rows.map((row) => ({
            Lista: row.list,
            'Fonte parametri': row.parameterSource,
            Coalizione: row.coalition,
            'Quota scenario': row.scenarioShare === null ? null : Number(row.scenarioShare.toFixed(2)),
            Usata: row.shareOverride,
            'Quota proiettata': row.projectedShare === null ? null : Number(row.projectedShare.toFixed(2)),
            Stato: row.status
        }))
    };
}
export async function runRegionalWorkspace(request: SimulationRequest, post: (message: SimulationWorkerMessage) => void): Promise<void> {
    if (request.kind !== 'regionali-er' || 'kind' in request.scenario)
        throw new Error('Politics execution requires the common politics worker');
    const startedAt = performance.now(), startedIso = new Date().toISOString(), base = request.basePath ?? '', scenario = request.scenario;
    requireSimulationCount(request.simulations);
    const errors = validateRegionalErScenario(scenario);
    if (errors.length)
        throw new Error(errors.join('\n'));
    const descriptor = snapshotDescriptors['regional-er']!;
    if (request.dataVersion !== 'v1' && request.dataVersion !== descriptor.version)
        throw new Error('Scenario/workspace data version mismatch');
    if (scenario.defaultSource.kind === 'bundled' && scenario.defaultSource.dataVersion !== 'v1' && scenario.defaultSource.dataVersion !== descriptor.version)
        throw new Error('Bundled scenario snapshot unavailable. Reset the scenario or restore its matching data explicitly.');
    progress(post, startedAt, 'prepare', 1, 5);
    const loadedSnapshot = { snapshot: await loadWorkspaceSnapshot('regional-er', base), path: descriptor.path };
    const staticSnapshot = loadedSnapshot.snapshot;
    const simulationCount = request.simulations;
    const source = buildRegionalErPipelineSourceFromSnapshot(staticSnapshot, { simulations: simulationCount });
    const projection = projectScenarioOntoRegionalErSource(source, scenario, {
        currentDate: startedIso,
        electionDate: request.electionDate,
        historicalVotes: staticSnapshot.data.comuni_liste_elezioni,
        municipalities: staticSnapshot.data.municipalities,
        simulations: simulationCount
    });
    const scenarioSource = projection.source;
    const runs: RegionalErRunPresentationSummary[] = [];
    let generatedSimulations = 0;
    let scrutinizedRuns = 0;
    progress(post, startedAt, 'simulate', generatedSimulations, simulationCount);
    for (let chunkStart = 1; chunkStart <= simulationCount; chunkStart += regionalErGeneratedChunkSize) {
        const chunkSize = Math.min(regionalErGeneratedChunkSize, simulationCount - chunkStart + 1);
        const snapshot = buildRegionalErDirectScrutinySnapshot({
            ...scenarioSource,
            simulazioni: chunkSize
        }, { seed: `${request.seed}:regional-er:chunk:${chunkStart}` });
        generatedSimulations += chunkSize;
        progress(post, startedAt, 'simulate', generatedSimulations, simulationCount);
        progress(post, startedAt, 'scrutinize', scrutinizedRuns, simulationCount);
        for (const simulation of snapshot.simulations) {
            const globalSimulation = chunkStart + simulation.sim - 1;
            const runStartedAt = performance.now();
            let output: ReturnType<typeof runRegionalErScrutiny>;
            try {
                output = runRegionalErScrutiny(simulation.input, {
                    pop_legale: scenarioSource.pop_legale,
                    liste: scenarioSource.liste
                }, { seed: `${request.seed}:regional-er:scrutiny:${globalSimulation}` });
            }
            catch (error) {
                throw new WorkspaceExecutionError(error, globalSimulation);
            }
            runs.push(summarizeRegionalErScrutinyRun({
                sim: globalSimulation,
                elapsedMs: performance.now() - runStartedAt,
                input: simulation.input,
                output
            }));
            scrutinizedRuns += 1;
            if (scrutinizedRuns % 10 === 0 || scrutinizedRuns === simulationCount) {
                progress(post, startedAt, 'scrutinize', scrutinizedRuns, simulationCount);
            }
        }
    }
    const resultWarnings: ScrutinyWarning[] = [
        {
            code: 'REGIONAL_ER_STATIC_SNAPSHOT',
            electionKind: request.kind,
            severity: 'info',
            message: `Running the TypeScript regional pipeline on ${loadedSnapshot.path}: ${staticSnapshot.metadata.source}.`
        },
        {
            code: 'REGIONAL_ER_SCENARIO_PROJECTION',
            electionKind: request.kind,
            severity: 'info',
            message: 'Scenario lists are projected from Emilia-Romagna historical correspondences when raw historical votes are available; regional workflow does not generate individual candidates.',
            todoReference: 'MIGRATION_PLAN.md#next-work'
        }
    ];
    for (const note of staticSnapshot.metadata.preparation?.notes ?? [])
        resultWarnings.push({ code: 'WORKSPACE_DATA_ASSUMPTION', electionKind: request.kind, severity: 'warning', message: note });
    for (const warning of projection.warnings) {
        resultWarnings.push({
            code: warning.code,
            electionKind: request.kind,
            severity: 'warning',
            message: warning.message,
            todoReference: warning.todoReference
        });
    }
    const result: SimulationResult = {
        type: 'result',
        status: 'completed',
        tables: [
            ...buildRegionalErResultTables(runs),
            ...buildRegionalErResultPlotTables(runs),
            scenarioProjectionTable(projection.rows),
            summarizeRegionalErGeneratedRuns(runs)
        ],
        warnings: resultWarnings,
        benchmark: {
            startedAt: startedIso,
            elapsedMs: performance.now() - startedAt,
            simulations: simulationCount,
            dataVersion: descriptor.version,
            scrutinyAlgorithmId: request.scrutinyAlgorithmId,
            seed: request.seed
        }
    };
    progress(post, startedAt, 'summarize', 5, 5);
    post(result);
    return;
}

import { resolve } from 'node:path';

export function campaignFiles(env: NodeJS.ProcessEnv = process.env) {
  const directory = resolve(env.CP_RESOURCE_OUTPUT ?? '../data/derived/common-politics-resources');
  const stem = `${env.CP_RESOURCE_MODEL ?? 'M02'}-${env.CP_RESOURCE_LAW ?? 'politiche-reform-2026-v1'}-${env.CP_RESOURCE_SIMULATIONS ?? '1000'}-${env.CP_RESOURCE_WORKERS ?? 'auto'}`;
  return { receipt: resolve(directory, `${stem}.json`), summary: resolve(directory, `${stem}.summary.json`),
    log: resolve(directory, `${stem}.log`), tests: resolve(directory, `${stem}.playwright.json`) };
}

export function campaignProgress(input: {
  model: string; law: string; simulations: number; phase: string; seconds: number; privateBytes: number;
}) {
  const lines = input.phase.split(/\r?\n/).map(s => s.trim()).filter(Boolean);
  // Keep real counters when the status contains them; never invent progress.
  const progress = lines.find(s => /\d+\s*\/\s*\d+/.test(s));
  const phase = (progress ?? lines[0] ?? 'unknown').replace(/\s+/g, ' ').slice(0, 100);
  return `[campaign] ${input.model}/${input.law}; ${phase}; requested=${input.simulations}; elapsed=${input.seconds.toFixed(0)}s; private=${(input.privateBytes / 1024 ** 3).toFixed(2)} GiB`;
}

interface CampaignReceipt {
  state?: string; law?: string; model?: string; simulations?: number;
  outcomes?: { complete: number; partial: number; failed: number };
  legalAcceptance?: { accepted: boolean; acceptedFailureIds: number[]; unexpectedSimulationIds: number[]; unmatchedErrorSimulationIds: number[] };
  infrastructureFailures?: unknown[]; pageErrors?: unknown[];
  preparationMs?: number; totalMs?: number; peakPrivateBytes?: number; peakWorkingSetBytes?: number;
  archive?: { elapsedMs: number; peakPrivateBytes: number; peakWorkingSetBytes: number; content?: unknown };
  qualification?: string;
  stored?: { execution?: { generator?: { version?: string }; actualWorkers?: number } };
}

/** Small index into the evidence, not a replacement for its IDs or diagnostics. */
export function campaignSummary(report: CampaignReceipt, files: ReturnType<typeof campaignFiles>) {
  return { schemaVersion: 1, state: report.state ?? 'unknown', law: report.law ?? null, model: report.model ?? null,
    generatorVersion: report.stored?.execution?.generator?.version ?? null,
    actualWorkers: report.stored?.execution?.actualWorkers ?? null,
    simulations: report.simulations ?? null, outcomes: report.outcomes ?? null,
    legalAcceptance: report.legalAcceptance ? { accepted: report.legalAcceptance.accepted,
      approvedFailureCount: report.legalAcceptance.acceptedFailureIds.length,
      unexpectedSimulationCount: report.legalAcceptance.unexpectedSimulationIds.length,
      unmatchedErrorSimulationCount: report.legalAcceptance.unmatchedErrorSimulationIds.length } : null,
    infrastructureFailureCount: report.infrastructureFailures?.length ?? null, pageErrorCount: report.pageErrors?.length ?? null,
    preparationMs: report.preparationMs ?? null, fittingAndExecutionMs: report.totalMs ?? null,
    peakPrivateBytes: report.peakPrivateBytes ?? null, peakWorkingSetBytes: report.peakWorkingSetBytes ?? null,
    archive: report.archive ? { verified: Boolean(report.archive.content), elapsedMs: report.archive.elapsedMs,
      peakPrivateBytes: report.archive.peakPrivateBytes, peakWorkingSetBytes: report.archive.peakWorkingSetBytes } : null,
    qualification: report.qualification ?? null, ...files };
}

export function campaignResult(summary: ReturnType<typeof campaignSummary>) {
  const time = (ms: number | null) => ms === null ? 'unavailable' : `${(ms / 1000).toFixed(1)}s`;
  const memory = (bytes: number | null) => bytes === null ? 'unavailable' : `${(bytes / 1024 ** 3).toFixed(2)}GiB`;
  const outcomes = summary.outcomes;
  const legal = summary.legalAcceptance;
  return `[campaign] ${summary.model ?? '?'}@${summary.generatorVersion ?? '?'}/${summary.law ?? '?'} ${summary.state}; requested=${summary.simulations ?? 'unavailable'} workers=${summary.actualWorkers ?? 'unavailable'}; `
    + `complete/partial/failed=${outcomes ? `${outcomes.complete}/${outcomes.partial}/${outcomes.failed}` : 'unavailable'}; `
    + `legal=${legal ? legal.accepted : 'unavailable'} approvedFailures=${legal?.approvedFailureCount ?? 'unavailable'} unexpected=${legal?.unexpectedSimulationCount ?? 'unavailable'} unmatched=${legal?.unmatchedErrorSimulationCount ?? 'unavailable'}; `
    + `infrastructure=${summary.infrastructureFailureCount ?? 'unavailable'} pageErrors=${summary.pageErrorCount ?? 'unavailable'}; `
    + `prepare=${time(summary.preparationMs)} fit+run=${time(summary.fittingAndExecutionMs)}; `
    + `sampledPrivate=${memory(summary.peakPrivateBytes)} sampledWorkingSet=${memory(summary.peakWorkingSetBytes)}; `
    + `archive=${summary.archive?.verified ?? 'unavailable'} archiveTime=${time(summary.archive?.elapsedMs ?? null)}; summary=${summary.summary}`;
}

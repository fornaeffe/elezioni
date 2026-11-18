/** Full-budget resource probe. A failure is retained and never selects defaults. */
import { mkdir, writeFile, readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { cpus } from 'node:os';
import { createHash } from 'node:crypto';
import { syntheticNational } from '../../tests/fixtures/fast-share-scale';
import { fitReference } from '../../src/lib/politics/reform-2026/vote-share/information';
const args = process.argv.slice(2),
  match = /^(28|64|128)\/(0|128|512)$/.exec(args[0] ?? '');
if (
  !match ||
  args.length > 2 ||
  (args[1] !== undefined && !/^[1-6]$/.test(args[1]))
)
  throw new Error(
    'Use COMPONENTS/POLLS [1..6 repetitions]; default is one cold run.',
  );
const components = Number(match[1]),
  polls = Number(match[2]),
  repetitions = Number(args[1] ?? 1),
  directory = resolve('../data/derived/fv08-acceptance/node-resources');
const source = syntheticNational(components, polls),
  report: { [key: string]: unknown; runs: unknown[] } = {
    stage: 'FV-08',
    scope:
      'Synthetic full-retention Node resource probe; not browser or predictive acceptance',
    environment: {
      node: process.version,
      platform: process.platform,
      cpu: cpus()[0]?.model,
    },
    components,
    polls,
    repetitions,
    protocol: {
      seed: 'fv08-resource-' + components + '-' + polls,
      burn: 1000,
      thin: 2,
      chains: 4,
      drawBudgets: [2000, 4000, 8000],
      desktopTargetMs: 30000,
    },
    inputHash: createHash('sha256')
      .update(JSON.stringify(source))
      .digest('hex'),
    engineHash: createHash('sha256')
      .update(
        await readFile(
          resolve('src/lib/politics/reform-2026/vote-share/information.ts'),
        ),
      )
      .digest('hex'),
    runs: [],
  };
await mkdir(directory, { recursive: true });
const path = resolve(directory, components + '-' + polls + '.json'),
  save = () => writeFile(path, JSON.stringify(report, null, 2) + '\n');
for (let run = 0; run < repetitions; run++) {
  const started = performance.now();
  try {
    const fitted = fitReference(
      source,
      'fv08-resource-' + components + '-' + polls,
      { solver: 'auto' },
    );
    const milliseconds = performance.now() - started,
      result = {
        run,
        cold: run === 0,
        milliseconds,
        latencyPassed: milliseconds <= 30000,
        workspaceBytes: fitted.audit.workspaceBytes,
        political: fitted.political.diagnostics,
        participation: fitted.participation.diagnostics,
        politicalDraws: fitted.political.drawsPerChain,
        participationDraws: fitted.participation.drawsPerChain,
        memory: process.memoryUsage(),
        processPeakRssKiB: process.resourceUsage().maxRSS,
      };
    report.runs.push(result);
    console.log(
      JSON.stringify({
        components,
        polls,
        run,
        milliseconds,
        latencyPassed: result.latencyPassed,
        workspaceBytes: result.workspaceBytes,
      }),
    );
  } catch (error) {
    report.runs.push({
      run,
      milliseconds: performance.now() - started,
      error: String(error),
      accepted: false,
    });
    process.exitCode = 1;
    break;
  } finally {
    await save();
  }
}
await save();
console.log('Report: ' + path);

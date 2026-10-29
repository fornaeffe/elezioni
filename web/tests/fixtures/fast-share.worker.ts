import { buildPoliticalTarget, buildParticipationTarget, fitGaussian, fitProfiles,
  type NationalSource, type SolverOptions } from '../../src/lib/politics/reform-2026/vote-share/index';
import type { ProfileSource } from '../../src/lib/politics/reform-2026/vote-share/profiles';
type TargetInput = { components: number; blocks: number; precision: number[]; linear: number[] };
export type NumericalWorkerRequest =
  | { action: 'sample'; target: TargetInput; seed: string }
  | { action: 'reference'; source: NationalSource; seed: string; fit?: boolean; options?: SolverOptions }
  | { action: 'profiles'; source: ProfileSource };

const workerScope = self as unknown as {
  onmessage: ((event: MessageEvent<NumericalWorkerRequest>) => void) | null;
  postMessage(value: unknown): void;
};
async function hash(values: Float64Array<ArrayBuffer>): Promise<string> {
  const digest = await crypto.subtle.digest('SHA-256', values.buffer);
  return [...new Uint8Array(digest)].map(x => x.toString(16).padStart(2, '0')).join('');
}
workerScope.onmessage = async event => {
  const started = performance.now(), request = event.data;
  try {
    if (request.action === 'sample') {
      const target = { ...request.target, precision: Float64Array.from(request.target.precision), linear: Float64Array.from(request.target.linear) };
      const samples = fitGaussian(target, { seed: request.seed });
      workerScope.postMessage({ ok: true, hash: await hash(samples.values), diagnostics: samples.diagnostics,
        drawsPerChain: samples.drawsPerChain, milliseconds: performance.now() - started });
    } else if (request.action === 'reference') {
      const political = buildPoliticalTarget(request.source, request.options), participation = buildParticipationTarget(request.source);
      const hashes = request.fit ? {
        political: await hash(fitGaussian(political.target, { seed: request.seed + ':political' }).values),
        participation: await hash(fitGaussian(participation.target, { seed: request.seed + ':participation' }).values)
      } : null;
      workerScope.postMessage({ ok: true, precision: [...political.target.precision], point: [...political.mode.point],
        hashes, audit: political.audit, milliseconds: performance.now() - started });
    } else if (request.action === 'profiles') {
      const result = fitProfiles(request.source);
      workerScope.postMessage({ ok: true, means: [...result.means], profileSd: [...result.profileSd],
        futureScales: [...result.futureScales], audit: result.audit, milliseconds: performance.now() - started });
    } else throw new Error('Unsupported numerical harness action');
  } catch (error) {
    workerScope.postMessage({ ok: false, error: error instanceof Error ? error.message : String(error),
      milliseconds: performance.now() - started });
  }
};

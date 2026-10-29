/** Revision-bound lifecycle. Accepted arrays remain private and are never
 * detached; consumers receive explicit, capacity-reserved copies. */
import { sourceKey } from './source-codec';
import {
  copyGenerationPacket,
  generationCapacity,
  ownedBuffers,
} from './fit-resources';
import type { WorkerResourceHints } from '../../../core/reform-worker-capacity';
import type { ReleasedScenario } from './release';
import type { PortableFitFile } from './checkpoint';
import type { PreviewSelection } from '../editor-preview';
import type { FitProgress } from './execution';
import type {
  AcceptedFit,
  FitCommand,
  FitConfiguration,
  FitPlan,
  FitTaskResult,
  FitWorkerReply,
  FitWorkerRequest,
} from './fit-types';

export interface FitWorkerPort {
  postMessage(message: FitWorkerRequest): void;
  terminate(): void;
  onmessage: ((event: MessageEvent<FitWorkerReply>) => void) | null;
  onerror: ((event: ErrorEvent) => void) | null;
  onmessageerror: ((event: MessageEvent) => void) | null;
}
export interface FitClientState {
  phase:
    | 'empty'
    | 'loading'
    | 'validating'
    | 'stale'
    | 'fitting'
    | 'ready'
    | 'cancelled'
    | 'error'
    | 'disposed';
  revision: number;
  acceptedRevision: number | null;
  progress?: FitProgress;
  plan?: FitPlan;
  error?: string;
}
export async function draftFingerprint(s: ReleasedScenario) {
  const { fittedStateId: _fit, profileStateId: _profile, ...data } = s.data;
  return sourceKey('fit-draft-revision-v1', { ...s, data });
}
type Pending = {
  previousPhase: FitClientState['phase'];
  id: number;
  revision: number;
  fingerprint: string;
  worker?: FitWorkerPort;
  command: FitCommand;
  resolve: (r: FitTaskResult) => void;
  reject: (e: Error) => void;
};
const cancelled = () =>
  new DOMException('Parameter estimation cancelled', 'AbortError');
export class ReformFitClient {
  private accepted?: AcceptedFit;
  private draft?: ReleasedScenario;
  private pending?: Pending;
  private sequence = 0;
  private activeConsumers = 0;
  private current: FitClientState = {
    phase: 'empty',
    revision: 0,
    acceptedRevision: null,
  };
  private configuration: FitConfiguration;
  constructor(
    configuration: FitConfiguration,
    private options: {
      workerFactory?: () => FitWorkerPort;
      onChange?: (state: FitClientState) => void;
    } = {},
  ) {
    this.configuration = structuredClone(configuration);
  }
  get state() {
    return structuredClone(this.current);
  }
  get canRun() {
    return (
      this.current.phase === 'ready' &&
      this.current.acceptedRevision === this.current.revision &&
      !this.pending
    );
  }
  copyDraft() {
    return this.draft ? structuredClone(this.draft) : undefined;
  }
  private publish(change: Partial<FitClientState>) {
    this.current = { ...this.current, ...change };
    this.options.onChange?.(this.state);
  }
  private requireOpen() {
    if (this.current.phase === 'disposed')
      throw new Error('Fitting client is disposed');
  }
  setDraft(scenario: ReleasedScenario) {
    this.requireOpen();
    this.cancel();
    this.draft = structuredClone(scenario);
    this.publish({
      revision: this.current.revision + 1,
      phase: 'stale',
      error: undefined,
      plan: undefined,
      progress: undefined,
    });
  }
  restoreAccepted() {
    this.requireOpen();
    if (!this.accepted) throw new Error('No accepted fit to restore');
    this.cancel();
    this.draft = structuredClone(this.accepted.scenario);
    const revision = this.current.revision + 1;
    this.publish({
      revision,
      acceptedRevision: revision,
      phase: 'ready',
      error: undefined,
      plan: undefined,
      progress: undefined,
    });
  }
  load() {
    return this.request({ action: 'load', configuration: this.configuration });
  }
  restoreCheckpoint(file: PortableFitFile) {
    return this.request({
      action: 'restore',
      configuration: this.configuration,
      file,
    });
  }
  checkpoint() {
    return this.edited('checkpoint');
  }
  preview(previewSelection?: PreviewSelection) {
    if (!this.canRun || !this.accepted || !this.draft)
      return Promise.reject(
        new Error('Verifica prima uno scenario compatibile.'),
      );
    return this.request({
      action: 'preview',
      configuration: this.configuration,
      previous: this.accepted,
      scenario: this.draft,
      previewSelection,
    });
  }
  editorData() {
    if (!this.accepted) throw new Error('Modello non ancora disponibile');
    const a = this.accepted,
      g = a.context.seats.geography;
    return structuredClone({
      catalog: {
        regions: g.regions,
        territories: g.territories,
        colleges: g.colleges,
        municipalities: a.assets.geography.targets.map((t) => ({
          code: t.code,
          name: t.name,
          provinceId: t.province,
          regionId: t.region,
        })),
      },
      profiles: {
        preferences: a.context.models.preferences.profiles,
        chambers: a.context.models.chambers.profiles,
      },
      sourceId: a.assets.sourceBundleId,
    });
  }
  source() {
    if (!this.accepted)
      return Promise.reject(new Error('Load the default first'));
    return this.request({
      action: 'source',
      configuration: this.configuration,
      catalog: this.accepted.sourceCatalog,
    });
  }
  validate() {
    return this.edited('validate');
  }
  fit() {
    return this.edited('fit');
  }
  private edited(action: 'validate' | 'fit' | 'checkpoint') {
    this.requireOpen();
    if (!this.accepted || !this.draft)
      return Promise.reject(
        new Error('Load an accepted default before editing evidence'),
      );
    return this.request({
      action,
      configuration: this.configuration,
      previous: this.accepted,
      scenario: this.draft,
    });
  }
  private request(command: FitCommand): Promise<FitTaskResult> {
    this.requireOpen();
    if (this.activeConsumers)
      return Promise.reject(
        new Error(
          'Release active simulation workers before parameter estimation',
        ),
      );
    this.cancel();
    return new Promise((resolve, reject) => {
      const pending: Pending = {
        previousPhase: this.current.phase,
        id: ++this.sequence,
        revision: this.current.revision,
        fingerprint: '',
        command,
        resolve,
        reject,
      };
      this.pending = pending;
      this.publish({
        phase:
          command.action === 'load'
            ? 'loading'
            : command.action === 'fit'
              ? 'fitting'
              : 'validating',
        error: undefined,
        progress: undefined,
        plan: undefined,
      });
      void this.launch(pending).catch((e) =>
        this.fail(pending, e instanceof Error ? e : new Error(String(e))),
      );
    });
  }
  private async launch(job: Pending) {
    job.fingerprint =
      job.command.action === 'load' || job.command.action === 'source'
        ? await sourceKey('load-fit-v1', {
            pointer: this.configuration.pointer,
            implementations: this.configuration.implementations,
          })
        : job.command.action === 'restore'
          ? await sourceKey('restore-fit-v1', job.command.file.pointer)
          : await draftFingerprint(job.command.scenario);
    if (this.pending !== job) return;
    const worker: FitWorkerPort =
      this.options.workerFactory?.() ??
      new Worker(
        new URL('../../../workers/reform-fit.worker.ts', import.meta.url),
        { type: 'module' },
      );
    job.worker = worker;
    worker.onmessage = (event) => {
      void this.receive(job, event.data).catch((e) =>
        this.fail(job, e instanceof Error ? e : new Error(String(e))),
      );
    };
    worker.onerror = (e) => {
      e.preventDefault();
      this.fail(job, new Error(e.message || 'Fitting worker crashed'));
    };
    worker.onmessageerror = () =>
      this.fail(job, new Error('Unable to decode fitting worker reply'));
    worker.postMessage({
      ...job.command,
      requestId: job.id,
      revision: job.revision,
      fingerprint: job.fingerprint,
    });
  }
  private async receive(job: Pending, r: FitWorkerReply) {
    if (
      this.pending !== job ||
      r.requestId !== job.id ||
      r.revision !== job.revision ||
      r.fingerprint !== job.fingerprint
    )
      return;
    if (r.action === 'progress') {
      this.publish({ progress: r.progress });
      return;
    }
    if (r.action === 'error') {
      this.fail(job, r.cancelled ? cancelled() : new Error(r.error));
      return;
    }
    if (r.result.accepted) {
      const a = r.result.accepted;
      if (
        a.implementations.generation !==
          this.configuration.implementations.generation ||
        a.implementations.fitting !==
          this.configuration.implementations.fitting ||
        a.implementations.profiles !==
          this.configuration.implementations.profiles ||
        a.scenario.data.fittedStateId !== a.assets.fittedStateId ||
        a.scenario.data.profileStateId !== a.assets.profileStateId ||
        a.scenario.referenceDate !== a.assets.fit.referenceDate ||
        (job.command.action !== 'load' &&
          job.command.action !== 'restore' &&
          job.command.action !== 'source' &&
          (await draftFingerprint(a.scenario)) !== job.fingerprint)
      )
        throw new Error('FIT_REPLY_BINDING_MISMATCH');
    }
    if (this.pending !== job || this.current.revision !== job.revision) return;
    this.pending = undefined;
    job.worker?.terminate();
    if (r.result.accepted) {
      this.accepted = r.result.accepted;
      this.draft = structuredClone(this.accepted.scenario);
      this.publish({
        phase: 'ready',
        acceptedRevision: job.revision,
        plan: r.result.plan,
        progress: undefined,
      });
    } else
      this.publish({
        phase: ['checkpoint', 'source', 'preview'].includes(job.command.action)
          ? job.previousPhase
          : 'stale',
        plan: r.result.plan,
        progress: undefined,
      });
    // Do not expose the authoritative accepted buffers to callers.
    job.resolve({
      plan: structuredClone(r.result.plan),
      audit: structuredClone(r.result.audit),
      file: r.result.file,
      source: r.result.source,
      preview: r.result.preview,
    });
  }
  private fail(job: Pending, error: Error) {
    if (this.pending !== job) return;
    this.pending = undefined;
    job.worker?.terminate();
    this.publish({
      phase: error.name === 'AbortError' ? 'cancelled' : 'error',
      error: error.message,
      progress: undefined,
    });
    job.reject(error);
  }
  cancel() {
    const job = this.pending;
    if (!job) return;
    try {
      job.worker?.postMessage({
        action: 'cancel',
        requestId: job.id,
        revision: job.revision,
        fingerprint: job.fingerprint,
      });
    } catch {
      /* Immediate termination remains the cancellation fallback. */
    }
    this.fail(job, cancelled());
  }
  reserveGenerationCopies(
    count: number,
    hints: WorkerResourceHints,
    additionalReservedBytes = 0,
  ) {
    this.requireOpen();
    if (!this.canRun || !this.accepted)
      throw new Error('No compatible accepted fit for the current draft');
    const packet = {
        scenario: this.accepted.scenario,
        assets: this.accepted.assets,
        context: this.accepted.context,
        keys: this.accepted.keys,
      },
      base = generationCapacity(packet, hints, additionalReservedBytes),
      capacity = generationCapacity(
        packet,
        hints,
        additionalReservedBytes + this.activeConsumers * base.perWorkerBytes,
      );
    if (!Number.isSafeInteger(count) || count < 1 || count > capacity.maximum)
      throw new Error(
        'GENERATION_AGGREGATE_MEMORY_LIMIT: maximum ' + capacity.maximum,
      );
    const leases = [];
    try {
      for (let i = 0; i < count; i++) {
        const copy = copyGenerationPacket(this.accepted);
        let released = false;
        this.activeConsumers++;
        leases.push({
          packet: copy,
          transfer: ownedBuffers(copy),
          release: () => {
            if (!released) {
              released = true;
              this.activeConsumers--;
            }
          },
        });
      }
    } catch (e) {
      for (const lease of leases) lease.release();
      throw e;
    }
    return { capacity, leases };
  }
  generationCapacity(hints: WorkerResourceHints) {
    if (!this.accepted) return undefined;
    return generationCapacity(this.accepted, hints);
  }
  dispose() {
    this.cancel();
    this.accepted = undefined;
    this.draft = undefined;
    this.publish({ phase: 'disposed', progress: undefined });
  }
}

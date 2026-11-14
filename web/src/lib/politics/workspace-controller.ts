import type { StoredResult } from '../core/result-store';
import type { PoliticsScenario } from './scenario/schema';
import type { PoliticsCheckpoint } from './scenario/envelopes';
import { parsePoliticsScenario } from './scenario/validation';
import { canonicalJson } from './scenario/dependencies';
import { activeGenerator } from './scenario/registry';
import { readPoliticsDraft, writePoliticsDraft } from './files/draft';
import type { PoliticsRuntime, PreparationSummary } from './execution/client';
import { politicsRunResources } from './execution/resources';

export const politicsSessionsKey = 'elezioni:politics:common-sessions:v1';
export interface SessionEntry {
  id: string; law: string; model: string; createdAt: string; renewal: string; completed: number; requested: number;
}
export interface PoliticsWorkspacePorts {
  runtime: PoliticsRuntime;
  storage: Pick<Storage, 'getItem' | 'setItem'>;
  manifest(id: string, owner: string): Promise<StoredResult>;
  importResult(file: File, id: string, owner: string): Promise<StoredResult>;
  validateScenario(s: PoliticsScenario): Promise<void>;
  changed(): void;
}
export class PoliticsWorkspaceController {
  scenario: PoliticsScenario;
  sessions: SessionEntry[] = [];
  selected = '';
  owner = '';
  viewed: StoredResult | null = null;
  preparation: PreparationSummary | null = null;
  busy = false;
  preparing = false;
  status = 'Inizializzazione';
  progress = '';
  error = '';
  storageError = '';
  private revision = 0;
  private viewRevision = 0;
  private taskRevision = 0;
  private disposed = false;
  private pending?: Promise<void>;
  constructor(initial: PoliticsScenario, private ports: PoliticsWorkspacePorts) { this.scenario = parsePoliticsScenario(initial); }
  get stale() { return !!this.viewed?.politics && canonicalJson(this.scenario) !== canonicalJson(this.viewed.politics.run.executedScenario); }
  get recommend1000() {
    const run = this.viewed?.politics?.run;
    return !!run && run.session.state === 'complete' && run.executedScenario.execution.simulations === 10 &&
      run.session.simulations.length === 10 && run.session.simulations.every(s => s.outcome === 'complete');
  }
  private notify() { if (!this.disposed) this.ports.changed(); }
  private saveSessions() {
    try { this.ports.storage.setItem(politicsSessionsKey, JSON.stringify({ owner: this.owner, selected: this.selected, sessions: this.sessions })); }
    catch (e) { this.storageError = `Indice delle sessioni non salvato: ${String(e)}`; }
  }
  async initialize() {
    this.owner = crypto.randomUUID();
    try { this.scenario = readPoliticsDraft(this.ports.storage) ?? this.scenario; }
    catch (e) { this.error = `Bozza salvata non caricata: ${String(e)}`; }
    try {
      const saved = this.ports.storage.getItem(politicsSessionsKey);
      if (saved) {
        const index = JSON.parse(saved);
        if (typeof index.owner !== 'string' || !index.owner || typeof index.selected !== 'string' || !Array.isArray(index.sessions) ||
          index.sessions.some((s: SessionEntry) => !s || typeof s.id !== 'string' || !s.id ||
            !['politiche-r-parity-v1', 'politiche-reform-2026-v1'].includes(s.law) || !/^M0[12]@2\.0\.0$/.test(s.model) ||
            typeof s.createdAt !== 'string' || !['camera', 'senato', 'joint'].includes(s.renewal) ||
            !Number.isSafeInteger(s.completed) || s.completed < 0 || !Number.isSafeInteger(s.requested) || s.requested < 1 || s.completed > s.requested) ||
          new Set(index.sessions.map((s: SessionEntry) => s.id)).size !== index.sessions.length) throw new Error('Invalid session index');
        this.owner = index.owner; this.selected = index.selected; this.sessions = index.sessions;
      }
    } catch (e) { this.error = `Indice delle sessioni non caricato: ${String(e)}`; }
    this.notify();
    const restore = this.selected ? this.selectSession(this.selected) : Promise.resolve();
    const initializationError = this.error;
    this.prepare();
    if (initializationError) { this.error = initializationError; this.notify(); }
    await restore;
  }
  change(next: PoliticsScenario) {
    if (this.busy || this.disposed) return false;
    const parsed = parsePoliticsScenario(next);
    this.scenario = parsed; ++this.revision;
    try { writePoliticsDraft(this.ports.storage, parsed); this.storageError = ''; }
    catch (e) { this.storageError = `Bozza non salvata: ${String(e)}`; }
    this.prepare(); return true;
  }
  prepare(force = false, checkpoint?: PoliticsCheckpoint) {
    if (this.busy || this.disposed) return;
    const revision = this.revision, token = ++this.taskRevision;
    const previous = this.preparation;
    this.preparing = true; this.preparation = null; this.status = 'Preparazione del modello'; this.error = ''; this.notify();
    this.pending = this.ports.runtime.prepare(structuredClone(this.scenario), force, checkpoint).then(summary => {
      if (this.disposed || token !== this.taskRevision || revision !== this.revision) return;
      this.preparation = summary;
      this.status = summary.reuse.fit ? 'Modello pronto · fitting riutilizzato' : 'Modello pronto'; this.progress = summary.note ?? '';
    }).catch(e => { if (!this.disposed && token === this.taskRevision) {
      if (checkpoint) this.preparation = previous;
      this.error = String(e); this.status = checkpoint && previous ? 'Checkpoint non caricato · modello precedente pronto' : 'Preparazione non riuscita';
    } })
      .finally(() => { if (token === this.taskRevision) { this.preparing = false; this.notify(); } });
  }
  preparationProgress(detail: string, completed: number, total: number) {
    if (!this.preparing) return;
    this.progress = `${detail} · ${completed}/${total}`; this.notify();
  }
  async cancel() {
    if (this.disposed) return;
    if (!this.busy) { ++this.taskRevision; this.preparing = false; this.preparation = null; this.status = 'Preparazione annullata'; this.notify(); }
    else { this.status = 'Annullamento · conservazione delle simulazioni assegnate'; this.notify(); }
    try { await this.ports.runtime.cancel(); } catch (e) { if (!this.disposed) this.error = String(e); }
    this.notify();
  }
  private entry(record: StoredResult): SessionEntry {
    const run = record.politics!.run;
    return { id: record.id, law: run.execution.scrutiny.id, model: `${run.execution.generator.id}@${run.execution.generator.version}`,
      createdAt: run.execution.startedAt, renewal: run.executedScenario.renewal, completed: run.result.completedSimulations,
      requested: run.executedScenario.execution.simulations };
  }
  private attach(record: StoredResult) {
    if (!record.politics) throw new Error('Not a common politics session');
    this.viewed = record; this.selected = record.id;
    this.sessions = [this.entry(record), ...this.sessions.filter(s => s.id !== record.id)]; this.saveSessions(); this.notify();
  }
  async run() {
    if (this.busy || this.disposed) return;
    // Lock before awaiting preparation: an edit cannot race the captured run.
    this.busy = true; this.notify();
    const revision = this.revision;
    const viewRevision = ++this.viewRevision;
    try {
      await this.pending;
      if (this.disposed || revision !== this.revision) return;
      if (!this.preparation) throw new Error('Preparare il modello prima di eseguire la simulazione');
      politicsRunResources(this.scenario, this.preparation.capacity);
      const captured = parsePoliticsScenario(this.scenario), id = crypto.randomUUID();
      if (viewRevision === this.viewRevision) { this.selected = id; this.viewed = null; }
      this.sessions = [{ id, law: captured.selection.scrutiny.id, model: `${activeGenerator(captured).id}@${activeGenerator(captured).version}`,
        createdAt: new Date().toISOString(), renewal: captured.renewal, completed: 0, requested: captured.execution.simulations }, ...this.sessions];
      this.saveSessions(); this.status = 'Esecuzione'; this.error = ''; this.notify();
      const record = await this.ports.runtime.run(captured, id, this.owner, p => {
        if (this.disposed || revision !== this.revision) return;
        if (p.phase === 'simulate') this.sessions = this.sessions.map(s => s.id === id ? { ...s, completed: p.completed } : s);
        this.progress = `${p.phase} · ${p.completed}/${p.total}`; this.notify();
      });
      if (this.disposed || revision !== this.revision) return;
      // Browsing another session while running is permitted and does not get undone.
      if (this.selected === id) this.attach(record);
      else { this.sessions = this.sessions.map(s => s.id === id ? this.entry(record) : s); this.saveSessions(); }
      this.status = record.result.storage?.state === 'complete' ? 'Esecuzione completata' : 'Esecuzione interrotta';
    } catch (e) { if (!this.disposed) { this.error = String(e); this.status = 'Esecuzione non riuscita'; } }
    finally { this.busy = false; this.notify(); }
  }
  async selectSession(id: string) {
    const token = ++this.viewRevision; this.selected = id; this.viewed = null; this.saveSessions(); this.notify();
    try {
      const record = await this.ports.manifest(id, this.owner);
      if (!this.disposed && token === this.viewRevision) this.attach(record);
    } catch (e) { if (!this.disposed && token === this.viewRevision) { this.error = `Sessione selezionata non disponibile: ${String(e)}`; this.notify(); } }
  }
  async importResult(file: File) {
    const token = ++this.viewRevision;
    try {
      const record = await this.ports.importResult(file, crypto.randomUUID(), this.owner);
      if (!this.disposed && token === this.viewRevision) this.attach(record);
    } catch (e) { if (!this.disposed && token === this.viewRevision) { this.error = `Risultati non caricati: ${String(e)}`; this.notify(); } }
  }
  async loadViewedScenario() {
    if (this.busy || !this.viewed?.politics) return false;
    const revision = this.revision, next = parsePoliticsScenario(this.viewed.politics.run.executedScenario);
    try { await this.ports.validateScenario(next); if (!this.disposed && !this.busy && revision === this.revision) return this.change(next); }
    catch (e) { if (!this.disposed) { this.error = `Scenario non caricato: ${String(e)}`; this.notify(); } }
    return false;
  }
  dispose() { this.disposed = true; ++this.revision; ++this.viewRevision; ++this.taskRevision; this.ports.runtime.close(); }
}

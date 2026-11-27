import { getContext, setContext, untrack } from 'svelte';
import type { ErrorDiagnostic } from './core/operation-error';

type StatusContent = { label: string; detail?: string; cancel?: () => void; retry?: () => void };
export type StatusError = StatusContent & { state: 'error'; diagnostics: readonly Pick<ErrorDiagnostic, 'code' | 'message'>[] };
export type StatusEntry = (StatusContent & { state: 'ready' | 'working' }) | StatusError;
const key = Symbol('app-status');
export class AppStatus {
  entries = $state<Record<string, StatusEntry>>({});
  set(id: string, entry: StatusEntry) { untrack(() => { this.entries = { ...this.entries, [id]: entry }; }); }
  clear(id: string) { untrack(() => { const next = { ...this.entries }; delete next[id]; this.entries = next; }); }
}
export function provideStatus() { const status = new AppStatus(); setContext(key, status); return status; }
export function useStatus() { return getContext<AppStatus>(key); }

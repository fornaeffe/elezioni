export interface ArchiveProgress {
  phase: 'reading' | 'validating' | 'saving' | 'exporting' | 'finalizing' | 'complete';
  completed: number;
  total?: number;
  rows?: number;
}
export type ArchiveProgressListener = (progress: ArchiveProgress) => void;

/** Completion is a separate event, emitted only after the durable operation. */
export function archiveProgressText(progress: ArchiveProgress): string {
  const labels = { reading: 'Lettura e importazione', validating: 'Verifica archivio', saving: 'Salvataggio',
    exporting: 'Esportazione', finalizing: 'Completamento del file', complete: 'Operazione completata' };
  const percent = progress.total ? ` · ${Math.min(100, Math.floor(100 * progress.completed / progress.total))}% della fase` : '';
  const rows = progress.rows === undefined ? '' : ` · ${progress.rows.toLocaleString('it-IT')} righe`;
  return `${labels[progress.phase]}${percent}${rows}`;
}

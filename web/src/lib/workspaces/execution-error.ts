/** Diagnostic context only. Preserve the scrutiny exception and fail the run;
 * do not reroll, transfer seats, or manufacture an accepted partial result. */
export class WorkspaceExecutionError extends Error {
  readonly reason: string;
  constructor(error: unknown, readonly simulationId: number, readonly chamber?: 'camera' | 'senato') {
    const reason = error instanceof Error ? error.message : String(error);
    super(`Simulazione ${simulationId}${chamber ? ` (${chamber})` : ''}: ${reason}`, { cause: error });
    this.reason = reason;
  }
}

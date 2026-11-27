export interface ErrorDiagnostic {
  code: string;
  message: string;
  name?: string;
  stack?: string;
  cause?: ErrorDiagnostic;
}

export class OperationError extends Error {
  constructor(readonly code: string, message: string, options?: ErrorOptions) {
    super(message, options);
  }
}

export function errorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

/** Preserve exception information across worker boundaries without formatting it for display. */
export function errorDiagnostic(error: unknown, fallbackCode: string): ErrorDiagnostic {
  const seen = new Set<unknown>();
  function describe(value: unknown, code: string): ErrorDiagnostic {
    if (!(value instanceof Error)) return { code, message: errorMessage(value) };
    seen.add(value);
    return {
      code: value instanceof OperationError ? value.code : code,
      message: value.message,
      name: value.name,
      stack: value.stack,
      cause: value.cause !== undefined && !seen.has(value.cause) ? describe(value.cause, 'CAUSE') : undefined,
    };
  }
  return describe(error, fallbackCode);
}

export function restoreOperationError(diagnostic: ErrorDiagnostic): OperationError {
  const cause = diagnostic.cause ? restoreOperationError(diagnostic.cause) : undefined;
  const error = new OperationError(diagnostic.code, diagnostic.message, cause ? { cause } : undefined);
  if (diagnostic.name) error.name = diagnostic.name;
  if (diagnostic.stack) error.stack = diagnostic.stack;
  return error;
}

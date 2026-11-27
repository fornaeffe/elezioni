import { errorDiagnostic } from '../../core/operation-error';

/** Translate the established reference diagnostic at the worker boundary. */
export function politicsErrorDiagnostic(error: unknown, fallbackCode: string) {
  const diagnostic = errorDiagnostic(error, fallbackCode);
  const code = 'REFERENCE_SUBJECT';
  const prefix = `${code}: `;
  if (diagnostic.code === fallbackCode && diagnostic.message.startsWith(prefix)) {
    return { ...diagnostic, code, message: diagnostic.message.slice(prefix.length) };
  }
  return diagnostic;
}

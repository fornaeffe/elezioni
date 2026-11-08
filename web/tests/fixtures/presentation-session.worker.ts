/** Test-only storage seed for retained pre-common result presentation evidence.
 * This is not a runtime file-import route or a converter for common sessions. */
import { handleResultService } from '../../src/lib/workers/result-service';
self.onmessage = async ({ data }) => {
  const resultId = crypto.randomUUID(), owner = crypto.randomUUID();
  const reply = await handleResultService({ type: 'result-service', requestId: 1, action: 'import', resultId, owner,
    file: new File([JSON.stringify(data)], 'presentation-evidence.json') });
  self.postMessage(reply?.ok ? { resultId, owner } : { error: reply?.error?.message ?? 'Missing fixture reply' });
};

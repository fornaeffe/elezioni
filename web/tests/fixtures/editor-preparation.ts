import { test as base, expect, type Page } from '@playwright/test';
const installed = new WeakSet<Page>();

/** UI-only preparation boundary. Assets, imports, edits and result queries remain
 * real. This fixture cannot fit, execute elections or create checkpoints. */
export async function installEditorPreparation(page: Page, holdFirstRefit = false) {
  if (installed.has(page)) return;
  installed.add(page);
  base.info().annotations.push({ type: 'editor-preparation', description: 'Controlled UI boundary; no election execution' });
  await page.route('**/politics-workspace.worker-*.js', async route => {
    const response = await route.fetch();
    const boundary = function (hold: boolean) {
      let seen = false, held: number | undefined;
      self.addEventListener('message', (event: MessageEvent) => {
        const m = event.data;
        if (m.type !== 'politics-task' || m.action === 'dispose') return;
        // Delegate result-service requests to the original worker below.
        event.stopImmediatePropagation();
        const reply = (data: unknown) => self.postMessage({ type: 'politics-task', requestId: m.requestId, ok: true, data });
        if (m.action === 'cancel') {
          if (held !== undefined) self.postMessage({ type: 'politics-task', requestId: held, ok: false,
            error: { code: 'CANCELLED', message: 'Controlled refit cancelled' } });
          held = undefined; reply(null); return;
        }
        if (m.action !== 'prepare') {
          self.postMessage({ type: 'politics-task', requestId: m.requestId, ok: false,
            error: { code: 'UNSUPPORTED_OPERATION', message: `Editor-only fixture forbids ${m.action}` } }); return;
        }
        if (hold && m.force) {
          hold = false; held = m.requestId;
          self.postMessage({ type: 'politics-task', requestId: m.requestId,
            progress: { phase: 'preparation', detail: 'Controlled editor refit', completed: 0, total: 1 } });
          return;
        }
        const MiB = 1024 * 1024;
        reply({ note: 'Editor-only preparation fixture; no numeric fit',
          keys: { preparationKey: 'editor', generationKey: 'editor', assemblyKey: 'editor' },
          reuse: { fit: seen, profiles: seen },
          capacity: { maximum: 26, recommended: 4, perWorkerBytes: 3072 * MiB,
            coordinatorBytes: 768 * MiB, deviceBytes: 64 * 1024 * MiB,
            budgetBytes: 32 * 1024 * MiB, estimateVersion: 'editor-fixture' } });
        seen = true;
      });
    };
    await route.fulfill({ response, body: `(${boundary.toString()})(${JSON.stringify(holdFirstRefit)});\n${await response.text()}` });
  });
}

// Auto fixture is local to importing specs, never a global production fallback.
export const test = base.extend({ page: async ({ page }, use) => {
  await installEditorPreparation(page); await use(page);
} });
export { expect };

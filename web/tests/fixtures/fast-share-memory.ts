/** Test instrumentation only. No isolation headers or memory API enter the app. */
import type { Browser, Page, CDPSession } from '@playwright/test';
export async function watchMemory(browser: Browser, page: Page) {
  if (browser.browserType().name() !== 'chromium')
    return {
      stop: async () => ({
        supported: false,
        reason: 'CDP heap instrumentation requires Chromium',
      }),
    };
  const root = await browser.newBrowserCDPSession(),
    main = await page.context().newCDPSession(page),
    sessions = new Map<string, string>(),
    pending = new Map<
      number,
      { resolve: (v: any) => void; reject: (e: Error) => void }
    >();
  let sequence = 0,
    active = true,
    busy = false,
    peakBytes = 0,
    samples = 0;
  const rows: object[] = [];
  root.on('Target.receivedMessageFromTarget', (event) => {
    const m = JSON.parse(event.message),
      p = pending.get(m.id);
    if (!p) return;
    pending.delete(m.id);
    if (m.error) p.reject(new Error(m.error.message));
    else p.resolve(m.result);
  });
  async function call(sessionId: string, method: string) {
    const id = ++sequence;
    return new Promise<any>((resolve, reject) => {
      pending.set(id, { resolve, reject });
      void root
        .send('Target.sendMessageToTarget', {
          sessionId,
          message: JSON.stringify({ id, method }),
        })
        .catch((e) => {
          pending.delete(id);
          reject(e);
        });
    });
  }
  const bytes = (m: any) =>
    m.usedSize + (m.backingStorageSize ?? 0) + (m.embedderHeapUsedSize ?? 0);
  async function sample() {
    if (!active || busy) return;
    busy = true;
    try {
      const targets = (await root.send('Target.getTargets')).targetInfos.filter(
        (t) => t.type === 'worker',
      );
      for (const t of targets)
        if (!sessions.has(t.targetId)) {
          try {
            sessions.set(
              t.targetId,
              (
                await root.send('Target.attachToTarget', {
                  targetId: t.targetId,
                  flatten: false,
                })
              ).sessionId,
            );
          } catch {
            /* Worker may have ended between enumeration and attach. */
          }
        }
      const mainUsage = await main.send('Runtime.getHeapUsage'),
        workers: object[] = [];
      let total = bytes(mainUsage);
      for (const t of targets) {
        const session = sessions.get(t.targetId);
        if (!session) continue;
        try {
          const usage = await call(session, 'Runtime.getHeapUsage');
          total += bytes(usage);
          workers.push({ url: t.url, ...usage });
        } catch {
          /* A terminated worker no longer contributes to active state. */
        }
      }
      samples++;
      peakBytes = Math.max(peakBytes, total);
      if (rows.length < 30 || total === peakBytes)
        rows.push({ total, main: mainUsage, workers });
    } finally {
      busy = false;
    }
  }
  const timer = setInterval(() => {
    void sample().catch(() => undefined);
  }, 100);
  return {
    stop: async () => {
      clearInterval(timer);
      await sample();
      active = false;
      for (const p of pending.values())
        p.reject(new Error('Instrumentation stopped'));
      pending.clear();
      await main.detach();
      await root.detach();
      return {
        supported: true,
        intervalMs: 100,
        samples,
        peakBytes,
        passed: peakBytes <= 512 * 1024 * 1024,
        rows,
        qualification:
          'Sampled sum of V8 used heap, backing stores and embedder heap across page and active workers; browser framework/GPU and persisted results excluded. Not an unsampled RSS maximum.',
      };
    },
  };
}

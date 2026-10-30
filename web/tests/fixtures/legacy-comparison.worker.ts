import {
  prepareReformGeneration,
  generateReformSimulation,
} from "../../src/lib/politics/reform-2026/generation";
import { runPoliticsReformScrutiny } from "../../src/lib/politics/reform-2026/scrutiny";
const scope = globalThis as unknown as {
  onmessage: ((e: MessageEvent) => void) | null;
  postMessage: (v: unknown) => void;
};
scope.onmessage = async (e) => {
  const started = performance.now(),
    ledger: object[] = [];
  try {
    const payload = await (await fetch(e.data.url)).json(),
      loaded = performance.now(),
      prepared = prepareReformGeneration(payload.scenario, payload.snapshot),
      ready = performance.now();
    let generationMs = 0,
      scrutinyMs = 0,
      failed = 0,
      nonComputable = 0;
    for (let id = 1; id <= e.data.draws; id++) {
      if (performance.now() - started > 3600000)
        throw new Error("60-minute browser budget exceeded");
      try {
        const a = performance.now(),
          generated = generateReformSimulation(prepared, id),
          b = performance.now(),
          result = runPoliticsReformScrutiny(generated.input);
        generationMs += b - a;
        scrutinyMs += performance.now() - b;
        const bad =
          result.status === "non-computable" ||
          result.stages.some((s) => s.status === "non-computable");
        nonComputable += Number(bad);
        ledger.push({
          id,
          status: result.status,
          seats: result.seats.length,
          filled: result.seats.filter((s) => s.status === "filled").length,
        });
      } catch (error) {
        failed++;
        ledger.push({ id, error: String(error) });
      }
      if (id % 25 === 0)
        scope.postMessage({
          progress: true,
          completed: id,
          draws: e.data.draws,
          milliseconds: performance.now() - started,
          failed,
          lastFailure: ledger.filter((r: any) => r.error).at(-1),
        });
    }
    scope.postMessage({
      ok: failed === 0 && nonComputable === 0,
      milliseconds: performance.now() - started,
      loadMs: loaded - started,
      prepareMs: ready - loaded,
      generationMs,
      scrutinyMs,
      failed,
      nonComputable,
      completed: ledger.length,
      requested: e.data.draws,
      lists: prepared.scenario.lists.camera.length,
      ledger,
    });
  } catch (error) {
    scope.postMessage({
      ok: false,
      error: String(error),
      milliseconds: performance.now() - started,
      ledger,
    });
  }
};

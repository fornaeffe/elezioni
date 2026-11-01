/** Actual current release, generation, candidature and scrutiny in Chromium workers. */
import { loadCandidate, loadCandidateSource } from '../../src/lib/politics/reform-2026/vote-share/release';
import { compileLegacy } from '../../src/lib/politics/reform-2026/vote-share/legacy-adapter';
import { prepareScenarioGeneration } from '../../src/lib/politics/reform-2026/vote-share/unified-generation';
import { runPoliticsReformScrutiny } from '../../src/lib/politics/reform-2026/scrutiny';
const scope = self as unknown as { onmessage: ((e: MessageEvent<{ model: 'M01' | 'M02'; first: number; last: number }>) => void) | null; postMessage(value: unknown): void };
scope.onmessage = async ({ data }) => {
  const started = performance.now();
  try {
    const root = new URL('/data/reform-2026/fast-share/', self.location.href),
      read = async (path: string) => { const r = await fetch(new URL(path, root)); if (!r.ok) throw new Error('Missing release asset:' + path); return new Uint8Array(await r.arrayBuffer()); },
      pointerResponse = await fetch(new URL('candidate.json', root));
    if (!pointerResponse.ok) throw new Error('Missing candidate pointer');
    const pointer = await pointerResponse.json(), loaded = await loadCandidate(pointer, read, { validateGeneration: false }),
      loadedAt = performance.now(), scenario = structuredClone(loaded.scenario);
    scenario.renewal = 'camera'; scenario.generation!.model = data.model;
    scenario.generation!.modelVersion = '1.0.0';
    scenario.references = scenario.references.filter(r => r.channel !== 'senato');
    scenario.nonValidReferences = scenario.nonValidReferences.filter(r => r.channel !== 'senato');
    const legacy = data.model === 'M01' ? await compileLegacy(scenario, await loadCandidateSource(loaded.catalog, read), read, loaded.assets, loaded.context) : undefined,
      prepared = prepareScenarioGeneration(scenario, loaded.assets, loaded.context, legacy), ready = performance.now(), runs = [];
    for (let id = data.first; id <= data.last; id++) {
      let generationMs = 0, scrutinyMs = 0, stage = 'generation', phaseStarted = performance.now();
      try {
        const generated = prepared.run(id); generationMs = performance.now() - phaseStarted;
        phaseStarted = performance.now(); stage = 'scrutiny';
        const result = runPoliticsReformScrutiny(generated.input); scrutinyMs = performance.now() - phaseStarted;
        const seats = result.seats.filter(s => s.status === 'filled');
        runs.push({ id, generationMs, scrutinyMs, status: result.status,
          seats: seats.length, distinctCandidates: new Set(seats.map(s => s.status === 'filled' ? s.candidateId : '')).size,
          camera: seats.filter(s => s.entitlement.chamber === 'camera').length,
          senato: seats.filter(s => s.entitlement.chamber === 'senato').length,
          unfilled: result.seats.length - seats.length,
          failures: result.stages.filter(s => s.status === 'non-computable'),
        });
      } catch (error) {
        if (stage === 'generation') generationMs = performance.now() - phaseStarted;
        else scrutinyMs = performance.now() - phaseStarted;
        runs.push({ id, generationMs, scrutinyMs, error: String(error), stage });
      }
      if ((id - data.first + 1) % 10 === 0) scope.postMessage({ progress: true, model: data.model, id, first: data.first });
      await new Promise(resolve => setTimeout(resolve, 0));
    }
    scope.postMessage({ done: true, model: data.model, first: data.first, last: data.last, catalogId: pointer.catalogId,
      loadMs: loadedAt - started, preparationMs: ready - loadedAt, elapsedMs: performance.now() - started, runs });
  } catch (error) { scope.postMessage({ fatal: String(error), model: data.model, first: data.first }); }
};

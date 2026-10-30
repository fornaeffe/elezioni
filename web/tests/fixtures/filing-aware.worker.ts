/** Browser-only measured computation; production numerical modules only. */
import { fitReference } from "../../src/lib/politics/reform-2026/vote-share/information";
import { fitProfiles } from "../../src/lib/politics/reform-2026/vote-share/profiles";
import {
  feasibleField,
  FIELD_MODELS,
} from "../../src/lib/politics/reform-2026/vote-share/feasible-field";
import { syntheticNational, syntheticProfiles } from "./fast-share-scale";
import { replay } from "../../scripts/acceptance/fast-share-replay";
import { runFitTask } from "../../src/lib/politics/reform-2026/vote-share/fit-service";
import { prepareFastGeneration } from "../../src/lib/politics/reform-2026/vote-share/generation";
import { runPoliticsReformScrutiny } from "../../src/lib/politics/reform-2026/scrutiny";
const scope = globalThis as unknown as {
  onmessage: ((e: MessageEvent) => void) | null;
  postMessage: (x: unknown) => void;
};
scope.onmessage = async (event) => {
  const started = performance.now(),
    request = event.data;
  try {
    if (request.action === "production") {
      const pointer = await (
          await fetch("/data/reform-2026/fast-share/candidate.json")
        ).json(),
        read = async (path: string) => {
          const response = await fetch("/" + pointer.assetRoot + path);
          if (!response.ok) throw new Error("Missing browser asset " + path);
          return {
            transport: "http" as const,
            body: new Uint8Array(await response.arrayBuffer()),
          };
        },
        configuration = {
          base: "",
          pointer,
          implementations: request.implementations,
        },
        loaded = await runFitTask({ action: "load", configuration }, read),
        loadedMs = performance.now() - started;
      if (!loaded.accepted) throw new Error("Missing accepted default");
      const scenario = structuredClone(loaded.accepted.scenario);
      scenario.referenceDate = "2026-10-20";
      const refitStarted = performance.now(),
        refitted = await runFitTask(
          { action: "fit", configuration, previous: loaded.accepted, scenario },
          read,
          {
            checkpoint: async () => {
              if (performance.now() - started > 1800000)
                throw new Error("BROWSER_BUDGET_EXCEEDED");
            },
          },
        ),
        refitMs = performance.now() - refitStarted;
      if (!refitted.accepted) throw new Error("Missing accepted refit");
      const accepted = refitted.accepted,
        runScenario = {
          ...accepted.scenario,
          seed: "filing-browser-production-v1",
          models: {
            ...accepted.scenario.models,
            forecast: request.model,
            forecastScale: request.scale,
          },
        },
        prepareStarted = performance.now(),
        prepared = prepareFastGeneration(
          runScenario,
          accepted.assets,
          accepted.context,
        ),
        prepareMs = performance.now() - prepareStarted,
        ledger: object[] = [];
      let failed = 0,
        nonComputable = 0,
        generationOnlyMs = 0,
        legalOnlyMs = 0;
      const generationStarted = performance.now();
      for (let id = 1; id <= request.draws; id++) {
        if (performance.now() - started > 1800000)
          throw new Error("BROWSER_BUDGET_EXCEEDED");
        try {
          const drawStarted = performance.now(),
            generated = prepared.run(id),
            legalStarted = performance.now(),
            legal = runPoliticsReformScrutiny(generated.input);
          generationOnlyMs += legalStarted - drawStarted;
          legalOnlyMs += performance.now() - legalStarted;
          const bad =
            legal.status === "non-computable" ||
            legal.stages.some((s) => s.status === "non-computable");
          nonComputable += Number(bad);
          ledger.push({
            id,
            status: legal.status,
            seats: legal.seats.length,
            filled: legal.seats.filter((s) => s.status === "filled").length,
            projectionResidual: generated.audit.projectionResidual,
            referenceConditioned: generated.audit.warnings.filter((w) =>
              w.startsWith("REFERENCE_POSTERIOR_CONDITIONED"),
            ),
          });
        } catch (error) {
          failed++;
          ledger.push({ id, error: String(error) });
        }
        if (id % 10 === 0)
          scope.postMessage({
            progress: true,
            completed: id,
            requested: request.draws,
            failed,
            nonComputable,
            milliseconds: performance.now() - started,
            model: request.model,
          });
      }
      scope.postMessage({
        ok: failed === 0 && nonComputable === 0,
        action: "production",
        milliseconds: performance.now() - started,
        loadMs: loadedMs,
        refitMs,
        prepareMs,
        generationLegalMs: performance.now() - generationStarted,
        requested: request.draws,
        completed: ledger.length,
        failed,
        nonComputable,
        generationOnlyMs,
        legalOnlyMs,
        model: request.model,
        scale: request.scale,
        profileRegionCorrelation:
          accepted.scenario.models.profileRegionCorrelation ?? 0,
        referenceDate: accepted.scenario.referenceDate,
        lists: accepted.scenario.lists.camera.length,
        components: accepted.scenario.components.length,
        refitAudit: refitted.audit,
        fitPlan: refitted.plan,
        ledger,
      });
      return;
    }
    if (request.action === "replay") {
      const source = await (await fetch(request.url)).json(),
        transferred = performance.now() - started,
        result = replay(source, request.seed, "reference", request.draws, {
          model: request.model,
          scaleMultiplier: request.scale,
          aggregateDomain: true,
        });
      scope.postMessage({
        ok: true,
        action: "replay",
        milliseconds: performance.now() - started,
        transferDecodeMs: transferred,
        scores: result.scores,
        maximumResidual: result.maximumResidual,
        fitDiagnostics: result.fitDiagnostics,
      });
      return;
    }
    const fit = fitReference(syntheticNational(28, 64), "filing-browser-gate"),
      profile = fitProfiles(syntheticProfiles(28, 128)),
      fitted = performance.now() - started;
    const reference = profile.municipalityIds.map((_, i) => {
      const weights = Float64Array.from({ length: 29 }, (_, j) =>
        j === 0 ? 0.3 : i % 3 === 0 && j > 20 ? 0 : 0.7 / 28,
      );
      const total = weights.reduce((a, b) => a + b, 0);
      return weights.map((x) => x / total);
    });
    const times = [];
    let valid = true;
    for (const model of FIELD_MODELS) {
      const before = performance.now();
      for (let draw = 1; draw <= 32; draw++) {
        const out = feasibleField({
          model,
          reference,
          cells: profile.municipalityIds.map((id, i) => ({
            id,
            region: profile.regions[i],
            electors: profile.electors[i],
          })),
          politicalScales: new Float64Array(28).fill(0.35),
          localScales: new Float64Array(29).fill(0.2),
          participationScale: 0.15,
          discrepancy: 0.25,
          horizon: 2,
          seed: "browser-gate",
          simulationId: draw,
        });
        out.probabilities.forEach((p, i) => {
          valid &&=
            Math.abs(p.reduce((a, b) => a + b, 0) - 1) < 1e-10 &&
            p.every((x, j) => x >= 0 && (reference[i][j] > 0 || x === 0));
        });
      }
      times.push({
        model,
        draws: 32,
        milliseconds: performance.now() - before,
      });
    }
    scope.postMessage({
      ok: true,
      milliseconds: performance.now() - started,
      fitProfileMs: fitted,
      politicalDiagnostics: fit.political.diagnostics,
      participationDiagnostics: fit.participation.diagnostics,
      valid,
      times,
    });
  } catch (error) {
    scope.postMessage({
      ok: false,
      error: String(error),
      milliseconds: performance.now() - started,
    });
  }
};

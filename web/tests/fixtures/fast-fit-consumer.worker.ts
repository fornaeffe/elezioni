import { prepareFastGeneration } from '../../src/lib/politics/reform-2026/vote-share/generation';
import { runPoliticsReformScrutiny } from '../../src/lib/politics/reform-2026/scrutiny';
import { byteHash } from '../../src/lib/politics/reform-2026/vote-share/source-codec';
import type { GenerationPacket } from '../../src/lib/politics/reform-2026/vote-share/fit-types';
const scope = self as unknown as {
  onmessage:
    | ((
        e: MessageEvent<{ packet: GenerationPacket; simulations: number[] }>,
      ) => void)
    | null;
  postMessage(x: unknown): void;
};
scope.onmessage = async ({ data: { packet: p, simulations } }) => {
  try {
    const prepared = prepareFastGeneration(p.scenario, p.assets, p.context),
      hashes = [];
    for (const id of simulations) {
      const result = prepared.run(id),
        legal = runPoliticsReformScrutiny(result.input);
      hashes.push({
        id,
        input: await byteHash(
          new TextEncoder().encode(JSON.stringify(result.input)),
        ),
        scrutiny: await byteHash(
          new TextEncoder().encode(JSON.stringify(legal)),
        ),
        status: legal.status,
      });
    }
    scope.postMessage({ ok: true, hashes });
  } catch (e) {
    scope.postMessage({
      ok: false,
      error: e instanceof Error ? e.message : String(e),
    });
  }
};

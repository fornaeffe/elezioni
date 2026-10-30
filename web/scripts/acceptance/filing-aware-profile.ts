/** Local CPU diagnostic; not a browser timing acceptance result. */
import { Session } from "node:inspector";
import { readFile, writeFile } from "node:fs/promises";
import { resolve } from "node:path";
import { createHash } from "node:crypto";
import { loadCandidate } from "../../src/lib/politics/reform-2026/vote-share/release";
import { readBytes } from "../data-preparation/fast-share-release-io";
const directory = resolve("static/data/reform-2026/fast-share"),
  loaded = await loadCandidate(
    JSON.parse(await readFile(resolve(directory, "candidate.json"), "utf8")),
    readBytes(directory),
  ),
  session = new Session();
session.connect();
const post = (method: string) =>
  new Promise<any>((resolve, reject) =>
    session.post(method, (error, result) =>
      error ? reject(error) : resolve(result),
    ),
  );
await post("Profiler.enable");
await post("Profiler.start");
const hashes = [];
for (let i = 1; i <= 3; i++)
  hashes.push(
    createHash("sha256")
      .update(JSON.stringify(loaded.prepared!.run(i)))
      .digest("hex"),
  );
const result = await post("Profiler.stop");
session.disconnect();
await writeFile(
  "../data/derived/filing-aware-research/generation.cpuprofile",
  JSON.stringify(result.profile),
);
console.log(JSON.stringify({ hashes }));
const nodes = new Map(result.profile.nodes.map((n: any) => [n.id, n])),
  counts = new Map<number, number>();
for (const id of result.profile.samples)
  counts.set(id, (counts.get(id) ?? 0) + 1);
console.log(
  JSON.stringify(
    [...counts]
      .sort((a, b) => b[1] - a[1])
      .slice(0, 25)
      .map(([id, count]) => ({ count, ...(nodes.get(id) as any).callFrame })),
    null,
    2,
  ),
);

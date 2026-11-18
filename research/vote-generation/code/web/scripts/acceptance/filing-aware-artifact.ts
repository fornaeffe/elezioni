/** Stream country-sized predictive arrays below V8's single-string limit. */
import { createWriteStream, createReadStream } from "node:fs";
import { createGzip, createGunzip } from "node:zlib";
import { Readable } from "node:stream";
import { pipeline } from "node:stream/promises";
export async function writeResearchArtifact(
  path: string,
  arrays: Record<string, readonly unknown[]>,
) {
  function* chunks() {
    yield "{";
    let first = true;
    for (const [key, rows] of Object.entries(arrays)) {
      if (!first) yield ",";
      first = false;
      yield JSON.stringify(key) + ":[\n";
      for (let i = 0; i < rows.length; i++) {
        if (i) yield ",\n";
        yield JSON.stringify(rows[i]);
      }
      yield "\n]";
    }
    yield "}";
  }
  await pipeline(
    Readable.from(chunks()),
    createGzip({ level: 6 }),
    createWriteStream(path),
  );
}
/** The format is a root object of named arrays of JSON records. Both old compact
 * and new line-separated artifacts stream one bounded record at a time. */
export async function* parseResearchChunks(chunks: AsyncIterable<string>) {
  let section: string | undefined,
    header = "",
    depth = 0,
    quoted = false,
    escaped = false,
    lastNonspace = "",
    parts: string[] = [];
  for await (const chunk of chunks) {
    const trimmed = chunk.trimEnd();
    if (trimmed.length) lastNonspace = trimmed.at(-1)!;
    let at = 0,
      start = 0;
    while (at < chunk.length) {
      if (section === undefined) {
        header += chunk[at++];
        const match = /"([\w-]+)":\s*\[$/.exec(header);
        if (match) {
          section = match[1];
          header = "";
          start = at;
        } else if (header.length > 256) header = header.slice(-128);
        continue;
      }
      const ch = chunk[at++];
      if (!depth) {
        if (ch === "]") {
          section = undefined;
          header = "";
        } else if (ch === "{") {
          depth = 1;
          start = at - 1;
          quoted = false;
          escaped = false;
        } else if (!/[\s,]/.test(ch))
          throw new Error("Invalid research array framing");
        continue;
      }
      if (quoted) {
        if (escaped) escaped = false;
        else if (ch === "\\") escaped = true;
        else if (ch === '"') quoted = false;
      } else if (ch === '"') quoted = true;
      else if (ch === "{") depth++;
      else if (ch === "}") {
        depth--;
        if (!depth) {
          parts.push(chunk.slice(start, at));
          yield { section, record: JSON.parse(parts.join("")) };
          parts = [];
          start = at;
        }
      }
    }
    if (depth) parts.push(chunk.slice(start));
  }
  if (depth || quoted || section !== undefined || lastNonspace !== "}")
    throw new Error("Incomplete research artifact");
}
export async function* readResearchArtifact(path: string) {
  const stream = createReadStream(path).pipe(createGunzip());
  stream.setEncoding("utf8");
  yield* parseResearchChunks(stream as unknown as AsyncIterable<string>);
}

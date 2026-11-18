import { it, expect } from "vitest";
import { parseResearchChunks } from "./filing-aware-artifact";
it("rejects unfinished arrays, records and root framing instead of accepting partial evidence", async () => {
  for (const text of [
    "",
    '{"metrics":[',
    '{"metrics":[{"values":[1]}',
    '{"metrics":[]',
  ]) {
    const consume = async () => {
      async function* chunks() {
        yield text;
      }
      for await (const item of parseResearchChunks(chunks())) void item;
    };
    await expect(consume()).rejects.toThrow("Incomplete research artifact");
  }
});
it("streams original compact and line-separated records across every token boundary", async () => {
  const input = {
    metrics: [
      { scope: "municipal", id: 'x}[]"\\', values: [0.1, 0.2] },
      { values: [1], label: "à" },
    ],
    college: [{ score: { coverage90: 1 }, values: [0] }],
    constituency: [],
  };
  for (const width of [1, 3, 17]) {
    async function* chunks() {
      const text = JSON.stringify(input);
      for (let i = 0; i < text.length; i += width)
        yield text.slice(i, i + width);
    }
    const out: Record<string, unknown[]> = {
      metrics: [],
      college: [],
      constituency: [],
    };
    for await (const item of parseResearchChunks(chunks()))
      out[item.section].push(item.record);
    expect(out).toEqual(input);
  }
});

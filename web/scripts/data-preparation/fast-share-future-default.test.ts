import { beforeAll, describe, expect, it } from "vitest";
import { existsSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { loadCandidate } from "../../src/lib/politics/reform-2026/vote-share/release";
import { readBytes } from "./fast-share-release-io";
const directory = resolve("static/data/reform-2026/fast-share");
describe.skipIf(!existsSync(resolve(directory, "candidate.json")))(
  "generated future-default filing acceptance (requires prepared candidate)",
  () => {
    let loaded: Awaited<ReturnType<typeof loadCandidate>>;
    beforeAll(async () => {
      loaded = await loadCandidate(
        JSON.parse(
          await readFile(resolve(directory, "candidate.json"), "utf8"),
        ),
        readBytes(directory),
      );
    });
    it("installs exactly the requested lists, coalitions and minority declarations", () => {
      for (const ch of ["camera", "senato"] as const) {
        const lists = loaded.scenario.lists[ch];
        expect(lists).toHaveLength(24);
        expect(lists.find((l) => l.id === "uv")?.name).toBe("Union Valdôtaine");
        expect(lists.find((l) => l.id === "suedtiroler-freiheit")?.name).toBe(
          "Süd-Tiroler Freiheit",
        );
        expect(
          lists.filter((l) => l.coalitionId === "centrodestra"),
        ).toHaveLength(7);
        expect(
          lists.filter((l) => l.coalitionId === "centrosinistra"),
        ).toHaveLength(7);
        expect(
          lists
            .filter((l) => l.protectedMinority)
            .map((l) => l.id)
            .sort(),
        ).toEqual([
          "freiheitlichen",
          "suedtiroler-freiheit",
          "svp-patt",
          "team-k",
          "uv",
        ]);
        expect(
          lists.find((l) => l.id === "futuro-nazionale")?.componentIds,
        ).toEqual(["poll-only-futuro-nazionale"]);
      }
    });
    it("covers all 26 ordinary Camera constituencies and restricts the two specified lists exactly", () => {
      const territories = (ch: "camera" | "senato", list: string) =>
        [
          ...new Set(
            loaded.scenario.coverage
              .filter((c) => c.chamber === ch && c.listIds.includes(list))
              .map(
                (c) =>
                  loaded.context.seats.geography.colleges
                    .find((k) => k.id === c.collegeId)
                    ?.territoryId.split(":")
                    .at(-1) ??
                  (c.collegeId === "camera:P:040001" ? "28" : undefined),
              ),
          ),
        ].sort();
      const ordinary = Array.from({ length: 26 }, (_, i) =>
        String(i + 1).padStart(2, "0"),
      );
      expect(territories("camera", "fdi")).toEqual([...ordinary, "28"]);
      expect(territories("camera", "sud-chiama-nord")).toEqual(ordinary);
      expect(territories("camera", "futuro-nazionale")).toEqual([
        ...ordinary,
        "27",
        "28",
      ]);
      for (const list of ["ora", "dsp"]) {
        expect(territories("camera", list)).toEqual([
          "01",
          "02",
          "03",
          "04",
          "07",
          "08",
          "10",
          "11",
          "12",
          "15",
          "16",
          "19",
          "24",
          "25",
        ]);
        expect(territories("senato", list)).toEqual([
          "01",
          "03",
          "05",
          "07",
          "08",
          "09",
          "12",
          "15",
          "19",
        ]);
      }
      expect(territories("camera", "uv")).toEqual(["27"]);
      expect(territories("senato", "uv")).toEqual(["02"]);
      expect(territories("camera", "team-k")).toEqual(["28"]);
      expect(territories("senato", "team-k")).toEqual(["04"]);
    });
    it("keeps supported and assumed identities explicit without overlapping future memberships", () => {
      expect(
        loaded.scenario.components
          .filter((c) => c.kind === "new")
          .map((c) => c.id)
          .sort(),
      ).toEqual([
        "poll-only-ora",
        "suedtiroler-freiheit",
        "team-k",
        "union-valdotaine",
        "valdostano-cdx",
        "valdostano-csx",
      ]);
      expect(loaded.assets.fit.componentIds).toHaveLength(28);
      for (const ch of ["camera", "senato"] as const) {
        const members = loaded.scenario.lists[ch].flatMap(
          (l) => l.componentIds,
        );
        expect(new Set(members).size).toBe(members.length);
      }
    });
  },
);

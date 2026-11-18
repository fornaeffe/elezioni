import { describe, it, expect } from "vitest";
import {
  links,
  eligiblePolls,
  pollPoints,
  historicalInput,
  digest,
} from "./legacy-comparison-input";
import {
  preparePairedVoteReferences,
  generatePairedMunicipalVotes,
} from "../../src/lib/politics/reform-2026/paired-vote-generation";
import { scoreAreas } from "./filing-aware-scores";
import type {
  ReformGenerationScenario,
  ReformGenerationSnapshot,
} from "../../src/lib/politics/reform-2026/generation-types";
const profile = {
  id: "central",
  sharedVariance: 0.5,
  residualCorrelation: 0.7,
  gapMultiplier: 1,
};
const base = {
  version: "test",
  models: {
    version: "model",
    chambers: {
      profiles: [profile],
      sigmaPerSqrtDay: { national: 0.02, region: 0.01, local: 0.01 },
    },
  },
  seats: { geography: { colleges: [] } },
} as unknown as ReformGenerationSnapshot;
const template = {
  chamberProfileId: "central",
  renewal: "camera",
  shares: [],
  abstention: [],
  seed: "seed",
} as unknown as ReformGenerationScenario;
const policy = {
  aliases: [{ labels: ["A"], id: "a", sources: [] }],
  joints: [],
  eventOverrides: [],
};
const municipality = (
  id: string,
  regionId: string,
  lists: { name: string; votes: number }[],
) => ({
  id,
  regionId,
  provinceId: "p",
  name: id,
  electors: 100,
  voters: 80,
  validVotes: lists.reduce((n, l) => n + l.votes, 0),
  matchable: true,
  lists,
});
const input = () => ({
  policy,
  polls: [],
  supplementalPolls: [],
  history: {
    events: [
      {
        id: "camera-2008",
        kind: "camera",
        date: "2008-04-13",
        sourceSha256: "history",
        municipalities: [
          municipality("01:a", "01", [
            { name: "A", votes: 40 },
            { name: "unrelated", votes: 20 },
          ]),
          municipality("04:b", "04", [{ name: "A", votes: 50 }]),
        ],
      },
      {
        id: "camera-2013",
        kind: "camera",
        date: "2013-02-24",
        municipalities: [
          municipality("01:a", "01", [{ name: "A", votes: 70 }]),
          municipality("04:b", "04", [{ name: "A", votes: 30 }]),
        ],
      },
    ],
  },
});
describe("installed legacy research inputs", () => {
  it("freezes explicit joint weights without treating a partial factor as discarded mass", () => {
    const result = links(
      "camera-2022",
      "AZIONE - ITALIA VIVA - CALENDA",
      ["azione", "italia-viva"],
      [
        { id: "a", members: ["azione"] },
        { id: "iv", members: ["italia-viva"] },
      ],
    );
    expect(result).toEqual([
      { id: "a", factor: 0.56 },
      { id: "iv", factor: 0.44 },
    ]);
    expect(
      links("", "", "unrelated".split(","), [{ id: "a", members: ["a"] }]),
    ).toEqual([]);
  });
  it("requires publication, fieldwork and measurement availability by the cutoff", () => {
    const p = {
      kind: "camera",
      groups: [["a"]],
      shares: [0.1],
      sample: 100,
      source: "test",
    };
    const x = {
      polls: [
        {
          ...p,
          id: "eligible",
          date: "2012-01-01",
          end: "2012-01-01",
          available: "2012-01-02",
        },
        {
          ...p,
          id: "late-publication",
          date: "2012-01-02",
          end: "2012-01-02",
          available: "2012-01-04",
        },
        {
          ...p,
          id: "future-fieldwork",
          date: "2012-01-01",
          end: "2012-01-04",
          available: "2012-01-02",
        },
      ],
      supplementalPolls: [],
    };
    expect(eligiblePolls(x, "2012-01-03").map((p) => p.id)).toEqual([
      "eligible",
    ]);
    expect(
      pollPoints(x, "2012-01-03", [{ id: "a", members: ["a"] }]).get("a")
        ?.share,
    ).toBe(0.1);
  });
  it("keeps unmapped counts non-valid and conserves electors", () => {
    const p = historicalInput(
        input(),
        "camera-2013",
        "2012-01-01",
        template,
        base,
        "no-polls",
      ),
      refs = preparePairedVoteReferences(p.scenario, p.snapshot);
    expect(refs.units[0].camera.abstention).toBeCloseTo(0.6);
    expect(p.audit.unmappedOrUnfiledValid).toBeCloseTo(20);
    for (const r of generatePairedMunicipalVotes(
      p.scenario,
      p.snapshot,
      refs.units,
      1,
    ))
      expect(r.abstention + r.votes.reduce((n, v) => n + v.votes, 0)).toBe(
        r.electors,
      );
  });
  it("uses dates only as source cutoffs when effective inputs match", () => {
    const a = historicalInput(
        input(),
        "camera-2013",
        "2011-01-01",
        template,
        base,
        "no-polls",
      ),
      b = historicalInput(
        input(),
        "camera-2013",
        "2012-01-01",
        template,
        base,
        "no-polls",
      );
    expect(a.audit.effectiveKey).toBe(b.audit.effectiveKey);
  });
  it("never uses held-out vote counts for reference preparation", () => {
    const x = input(),
      a = historicalInput(
        x,
        "camera-2013",
        "2012-01-01",
        template,
        base,
        "no-polls",
      );
    x.history.events[1].municipalities[0].lists[0].votes = 1;
    x.history.events[1].municipalities[0].validVotes = 1;
    const b = historicalInput(
      x,
      "camera-2013",
      "2012-01-01",
      template,
      base,
      "no-polls",
    );
    expect(a.audit.effectiveKey).toBe(b.audit.effectiveKey);
  });
  it("excludes TAA from ordinary poll refresh without changing the no-poll law", () => {
    const p = historicalInput(
      input(),
      "camera-2013",
      "2012-01-01",
      template,
      base,
      "no-polls",
    );
    expect(
      p.snapshot.seats.geography.colleges.some(
        (c) => c.kind === "special-uninominal",
      ),
    ).toBe(true);
    const old = structuredClone(p.snapshot);
    for (const c of old.seats.geography.colleges) (c as any).kind = "ordinary";
    expect(digest(preparePairedVoteReferences(p.scenario, old).units)).toBe(
      digest(preparePairedVoteReferences(p.scenario, p.snapshot).units),
    );
    expect(
      generatePairedMunicipalVotes(
        p.scenario,
        old,
        preparePairedVoteReferences(p.scenario, old).units,
        1,
      ),
    ).toEqual(
      generatePairedMunicipalVotes(
        p.scenario,
        p.snapshot,
        preparePairedVoteReferences(p.scenario, p.snapshot).units,
        1,
      ),
    );
  });
  it("uses a regional historical fallback for missing or invalid municipalities", () => {
    const x = input();
    x.history.events[0].municipalities.push(
      municipality("01:c", "01", [{ name: "A", votes: 40 }]),
    );
    x.history.events[0].municipalities[0].electors = 10;
    const p = historicalInput(
      x,
      "camera-2013",
      "2012-01-01",
      template,
      base,
      "no-polls",
    );
    expect(p.audit.fallback).toContain("01:a");
  });
  it("scores observable groups independently of the internal carrier coordinate", () => {
    const source = {
        truth: [
          {
            id: "x",
            region: "01",
            electors: 100,
            nonValid: 20,
            groups: [["a", "b"]],
            counts: [80],
          },
        ],
      },
      mapping = [{ municipality: "x", areas: [{ id: "college", weight: 1 }] }];
    expect(
      scoreAreas(
        source,
        ["x"],
        ["a", "b"],
        [new Uint32Array([20, 80, 0])],
        mapping,
      ),
    ).toEqual(
      scoreAreas(
        source,
        ["x"],
        ["a", "b"],
        [new Uint32Array([20, 0, 80])],
        mapping,
      ),
    );
  });
});

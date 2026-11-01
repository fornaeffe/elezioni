/** Group totals and unidentified internal contrasts are distinct coordinates. */
import { politicalIntervention } from "./interventions";
import { sum } from "./types";
export function componentIntervention(
  rows: Float64Array[],
  electors: number[],
  members: number[][],
  values: number[],
  identifiedGroups: number[][],
  fallback: (i: number, j: number) => number,
  options: { chunkSize?: number } = {},
) {
  const complete = identifiedGroups.filter((group) =>
      group.every((j) => members.some((m) => m.length === 1 && m[0] === j)),
    ),
    internal = new Set(complete.flat()),
    macro = members.flatMap((m, i) =>
      m.some((j) => internal.has(j)) ? [] : [{ members: m, share: values[i] }],
    );
  for (const group of complete)
    macro.push({
      members: group,
      share: sum(
        group.map(
          (j) => values[members.findIndex((m) => m.length === 1 && m[0] === j)],
        ),
      ),
    });
  let result = politicalIntervention(
    rows,
    electors,
    macro.map((g) => g.members),
    macro.map((g) => g.share),
    fallback,
    options,
  );
  for (const group of complete) {
    const target = group.map(
        (j) => values[members.findIndex((m) => m.length === 1 && m[0] === j)],
      ),
      total = sum(target);
    if (total === 0) continue;
    const field = rows.map((row) =>
      row.map((v, j) =>
        j === 0 ? 1 - sum(group.map((k) => row[k])) : group.includes(j) ? v : 0,
      ),
    );
    const fitted = politicalIntervention(
      field,
      electors,
      group.map((j) => [j]),
      target.map((v) => v / total),
      (i, j) => (group.includes(j) ? fallback(i, j) : 0),
      options,
    );
    rows.forEach((row, i) => group.forEach((j) => (row[j] = field[i][j])));
    result = {
      residual: Math.max(result.residual, fitted.residual),
      iterations: result.iterations + fitted.iterations,
    };
  }
  return result;
}

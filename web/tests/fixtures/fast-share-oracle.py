"""Independent FV-02 oracle. Optional reproduction only; never imported by app.

Run with isolated NumPy 2.3.4 / SciPy 1.16.3, PYTHONDONTWRITEBYTECODE=1.
The TypeScript gate reads the generated JSON and needs neither package.
"""
from __future__ import annotations
import argparse, hashlib, json, math, sys
from pathlib import Path
import numpy as np
import scipy
from scipy.integrate import quad
from scipy.optimize import brentq
from scipy.special import ndtri
from scipy.stats import truncnorm


def reduced_references(fixture, tolerance):
    Q = np.array(fixture["Q"], float)
    b = np.array(fixture["b"], float)
    triangle = fixture["components"] == 3

    def point(x, y, coordinate=0):
        if not triangle:
            return np.array([x, 1-x, y, 1-y])
        indexes = [j for j in range(3) if j != coordinate]
        value = np.zeros(3); value[coordinate] = x
        value[indexes[0]] = y; value[indexes[1]] = 1-x-y
        return value

    # A numerical scale changes neither the density nor normalized integrals.
    centre = np.array(fixture.get("mode", [0.3, 0.4, 0.3]), float)
    scale = -.5*centre@Q@centre+b@centre

    def density(x, y, coordinate=0):
        p = point(x, y, coordinate)
        return math.exp(-.5*p@Q@p+b@p-scale)

    def marginal(x, coordinate=0):
        if not triangle and coordinate >= 2:
            return quad(lambda y: density(y, x), 0, 1,
                        epsabs=tolerance, epsrel=tolerance, limit=200)[0]
        return quad(lambda y: density(x, y, coordinate), 0, 1-x if triangle else 1,
                    epsabs=tolerance, epsrel=tolerance, limit=200)[0]

    normalizer = quad(lambda x: marginal(x), 0, 1, epsabs=tolerance,
                      epsrel=tolerance, limit=200)[0]
    means, intervals = [], []
    for coordinate in range(len(b)):
        if not triangle and coordinate in [1, 3]:
            means.append(1-means[coordinate-1])
            intervals.append([1-intervals[coordinate-1][1], 1-intervals[coordinate-1][0]])
            continue
        m = lambda x: marginal(x, coordinate)
        means.append(quad(lambda x: x*m(x), 0, 1, epsabs=tolerance,
                          epsrel=tolerance, limit=200)[0]/normalizer)
        def cdf(x):
            return quad(m, 0, x, epsabs=tolerance, epsrel=tolerance,
                        limit=200)[0]/normalizer
        intervals.append([brentq(lambda x: cdf(x)-p, 0, 1, xtol=1e-12)
                          for p in [.05, .95]])
    return {"mean": means, "interval90": intervals}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("input", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[3]
    script_dir = root/"web/scripts/data-preparation/stan"
    sys.dont_write_bytecode = True
    sys.path.insert(0, str(script_dir))
    from fast_party_model import political_precision, turnout_precision, diagnose
    from modular_geography import fit_profiles
    inputs = json.loads(args.input.read_text())
    national = inputs["national"]
    elections = [{"id":e["id"], "date":e["date"], "kind":e["context"],
                  "electors":e["electors"], "groups":e["groups"], "counts":e["counts"]}
                 for e in national["elections"]]
    polls = [{**p, "kind":p["context"], "available":p["availableDate"],
              "end":p["fieldworkEnd"], "group":p["errorGroup"]} for p in national["polls"]]
    source = {"components":national["components"], "cutoff":national["referenceDate"],
              "nationalHistory":elections, "polls":polls}
    Q,b,mode,audit = political_precision(source,polls)
    Q0,b0,mode0 = turnout_precision(source)
    profiles = inputs["profiles"]
    # The reference function calls its conditional target metadata "truth";
    # these rows contain no future observed vote counts.
    geo = {**source, "targetKind":profiles["targetContext"],
           "cities":sorted({r["id"] for e in profiles["elections"] for r in e["rows"]} |
                           {r["id"] for r in profiles["targets"]}),
           "elections":[{**e,"kind":e["context"]} for e in profiles["elections"]],
           "truth":[{**r,"groups":[[s] for s in source["components"]]} for r in sorted(profiles["targets"], key=lambda r:r["id"])]}
    geographic = fit_profiles(geo)
    frozen = json.loads((root/"research/vote-generation/fv01/contract-fixtures.json").read_text())
    references=[]
    for case in frozen["quadratureCases"]:
        fine=reduced_references(case, 2e-13)
        coarse=reduced_references(case, 1e-11)
        references.append({"id":case["id"], **case, **fine,
                           "meanRefinement":float(np.max(np.abs(np.array(fine["mean"])-coarse["mean"]))),
                           "endpointRefinement":float(np.max(np.abs(np.array(fine["interval90"])-coarse["interval90"])))})
    rng=np.random.default_rng(20261028)
    chains=rng.normal(size=(4,256,3))
    for t in range(1,256):
        chains[:,t,1]=.7*chains[:,t-1,1]+math.sqrt(1-.7**2)*chains[:,t,1]
    chains[:,:,2]=np.round(chains[:,:,2]*2)/2
    result={"schemaVersion":1,"purpose":"Independent reduced oracle; not production data",
      "environment":{"numpy":np.__version__,"scipy":scipy.__version__},
      "input":inputs,
      "sourceHashes":{p.name:hashlib.sha256(p.read_bytes()).hexdigest()
                      for p in [Path(__file__),script_dir/"fast_party_model.py",script_dir/"modular_geography.py"]},
      "quadrature":references,
      "national":{"precision":Q.tolist(),"linear":b.tolist(),"mode":mode.tolist(),
                  "participationPrecision":Q0.tolist(),"participationLinear":b0.tolist(),"participationMode":mode0.tolist(),
                  "sigmaPolitical":audit["sigmaPolitical"],"sigmaParticipation":audit["sigmaParticipation"],
                  "electionDiscrepancy":audit["electionShockSd"],"historicalScaleBounds":audit["historicalScaleBounds"]},
      "profiles":{"municipalityIds":geographic["cities"],"means":geographic["offset"].tolist(),
                  "profileSd":geographic["profileSd"].tolist(),"filterScales":geographic["scale"].tolist(),
                  "commonLocalVariance":geographic["audit"]["commonLocalVariancePerYear"],
                  "fallback":geographic["fallback"].astype(int).tolist()},
      "diagnostics":{"chains":chains.tolist(),"expected":diagnose(chains)},
      "normalQuantiles":[{"p":p,"value":float(ndtri(p))} for p in [1e-8,.001,.025,.5,.975,.999,1-1e-8]],
      "truncatedNormals":[{"a":a,"b":b,"mean":float(truncnorm.mean(a,b)),"variance":float(truncnorm.var(a,b))}
                          for a,b in [(8.,9.),(-9.,-8.),(-.01,.02),(0.,1.)]]}
    args.output.write_text(json.dumps(result,separators=(",",":"))+"\n")
    print(json.dumps({"quadrature":len(references),"maximumMeanRefinement":max(r["meanRefinement"] for r in references),
                      "maximumEndpointRefinement":max(r["endpointRefinement"] for r in references),
                      "outputBytes":args.output.stat().st_size}))


if __name__=="__main__":
    main()

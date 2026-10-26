"""Offline Stan validation; no time-budget acceptance substitute for diagnostics."""
from __future__ import annotations
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import time
import platform
import numpy as np
import cmdstanpy

parser = argparse.ArgumentParser()
parser.add_argument("directory", type=Path)
parser.add_argument("--cmdstan", type=Path, required=True)
parser.add_argument("--cache", type=Path, required=True)
args = parser.parse_args()
root = Path(__file__).resolve().parents[4]
source = Path(__file__).with_name("restricted-party-drift.stan")
digest = hashlib.sha256(source.read_bytes()).hexdigest()
manifest = json.loads((args.directory / "manifest.json").read_text())
if digest != manifest["modelHash"]:
    raise RuntimeError("Stan model hash does not match prepared inputs")
cmdstanpy.set_cmdstan_path(str(args.cmdstan))
cache = args.cache / digest[:12]
cache.mkdir(parents=True, exist_ok=True)
copy = cache / source.name
if not copy.exists():
    shutil.copyfile(source, copy)
tic = time.monotonic()
model = cmdstanpy.CmdStanModel(stan_file=str(copy), cpp_options={"O": "1"})
print(json.dumps({"phase": "compiled", "seconds": time.monotonic()-tic}), flush=True)
results = []
policy = manifest["policy"]
reference_path = root / manifest["referenceSummary"]
if hashlib.sha256(reference_path.read_bytes()).hexdigest() != manifest["referenceHash"]:
    raise RuntimeError("Independent reference artifact hash changed")
references = json.loads(reference_path.read_text())
for case in manifest["cases"]:
    if hashlib.sha256((args.directory/case["dataFile"]).read_bytes()).hexdigest() != case["dataSha256"]:
        raise RuntimeError("Prepared Stan data hash changed")
    output = cache / case["id"] / time.strftime("%Y%m%d-%H%M%S", time.gmtime())
    print(json.dumps({"phase": "sampling", "case": case["id"]}), flush=True)
    tic = time.monotonic()
    fit = model.sample(data=str(args.directory / case["dataFile"]), seed=policy["seed"], chains=policy["chains"],
                       parallel_chains=policy["chains"], iter_warmup=policy["warmup"], iter_sampling=policy["sampling"],
                       adapt_delta=policy["adaptDelta"], max_treedepth=policy["maxTreedepth"],
                       inits=0, output_dir=str(output), show_progress=False, refresh=100)
    seconds = time.monotonic()-tic
    summary = fit.summary()
    diagnostics = fit.method_variables()
    divergences = int(diagnostics["divergent__"].sum())
    max_depth = int((diagnostics["treedepth__"] >= policy["maxTreedepth"]).sum())
    rhat_col = "R_hat" if "R_hat" in summary else "Rhat"
    ess_col = next(c for c in ["ESS_bulk", "N_Eff", "N_eff"] if c in summary)
    varying = summary.loc[summary["StdDev"] > 1e-10] if "StdDev" in summary else summary
    max_rhat = float(varying[rhat_col].max())
    min_ess = float(varying[ess_col].min())
    energy = diagnostics["energy__"]
    ebfmi = np.mean(np.diff(energy, axis=0)**2, axis=0)/np.var(energy, axis=0)
    elector = fit.stan_variable("national_elector")
    within = fit.stan_variable("national_within_joint")
    latent = fit.stan_variable("local_log_a_b")
    outputs = np.column_stack([elector, within, latent])
    values = [{"mean": float(v.mean()), "lower90": float(np.quantile(v,.05)), "upper90": float(np.quantile(v,.95)),
               "width90": float(np.quantile(v,.95)-np.quantile(v,.05))} for v in outputs.T]
    hard_residual = float(np.max(np.abs(fit.stan_variable("national_valid_a")-.2))) if case["hard"] else None
    passed = max_rhat <= policy["maxRhat"] and min_ess >= policy["minEss"] and divergences == 0 and max_depth == 0 and float(ebfmi.min()) >= .3
    if case["hard"]:
        passed = passed and hard_residual < 1e-10
    comparison = None
    if case["referenceCaseId"]:
        reference = next(r for r in references["results"] if r["fixture"]["id"] == case["referenceCaseId"])
        comparison = [{"meanDifference": a["mean"]-b["referenceMean"], "widthRatio": a["width90"]/b["referenceWidth90"]}
                      for a,b in zip(values,reference["laplace"]["variables"])]
        passed = passed and reference["samplingPassed"] and all((i==5 or abs(v["meanDifference"])<=.025) and .65<=v["widthRatio"]<=1.5 for i,v in enumerate(comparison))
    row = {"case": case["id"], "seconds": seconds, "passed": bool(passed), "maxRhat": max_rhat, "minEss": min_ess,
           "divergences": divergences, "maxTreedepthHits": max_depth, "ebfmi": ebfmi.tolist(), "hardResidual": hard_residual,
           "outputs": values, "independentReferenceComparison": comparison}
    results.append(row)
    fit.save_csvfiles(str(args.directory / "stan-csv" / case["id"]))
    row["csvFiles"] = fit.runset.csv_files
    (args.directory / "stan-summary.json").write_text(json.dumps({"modelHash": digest, "cmdstanVersion": cmdstanpy.cmdstan_version(),
        "cmdstanpyVersion": cmdstanpy.__version__, "python": platform.python_version(), "platform": platform.platform(),
        "policy": policy, "results": results, "timeGateWaived": True, "fullDataFitStarted": False}, indent=2)+"\n")
    print(json.dumps({k: v for k,v in row.items() if k not in {"outputs", "csvFiles"}}), flush=True)
if not all(r["passed"] for r in results):
    raise SystemExit(2)

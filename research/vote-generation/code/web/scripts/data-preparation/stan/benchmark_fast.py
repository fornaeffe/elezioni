"""Measure complete national+geographic fitting separately from generation."""
from __future__ import annotations
import argparse,hashlib,json,resource,time
from pathlib import Path
import numpy as np
from run_fast import fit,forecast

def main():
    p=argparse.ArgumentParser();p.add_argument('directory',type=Path);p.add_argument('--cache',type=Path,required=True);p.add_argument('--draws',type=int,default=512);p.add_argument('--fit-only',action='store_true');args=p.parse_args()
    directory=args.directory;manifest=json.loads((directory/'manifest.json').read_text());source=json.loads((directory/'profiles.json').read_text());start=time.monotonic()
    if hashlib.sha256((directory/'profiles.json').read_bytes()).hexdigest()!=manifest['profileHash']:raise RuntimeError('Prepared profile input changed')
    for item in manifest['sourceVersions']:
        if hashlib.sha256((Path(__file__).parent.parent/item['path']).read_bytes()).hexdigest()!=item['sha256']:raise RuntimeError('Re-export after code change: '+item['path'])
    a,b,fd=fit(source,args.cache,kind_effect=True);fd['version']=manifest['version']
    (directory/'fast-kind-fit.json').write_text(json.dumps(fd,indent=2)+'\n');np.savez_compressed(directory/'fast-kind-posterior.npz',political=a,turnout=b)
    result={'version':manifest['version'],'model':'kind-common-conservative','fittingDimensions':{'political':a.shape[2],'participation':b.shape[2]},'nationalFit':fd,
      'benchmarkSourceHash':hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),'dataHash':manifest['profileHash'],'accepted':fd['accepted']}
    if not fd['accepted']:
        (directory/'benchmark.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps({'accepted':False,'nationalFitSeconds':fd['fitSeconds']}));raise SystemExit(2)
    if not args.fit_only:
        prediction=forecast(source,a,b,fd['audit'],draws=args.draws,component_scales=False,shock_factor=1.5)
        result.update(prediction);result['totalFittingSeconds']=fd['fitSeconds']+prediction['profileSeconds']
        result['totalSeconds']=time.monotonic()-start;result['profileMunicipalities']=len(source['cities']);result['futureMunicipalities']=len(source['truth'])
    result['peakPythonResidentBytes']=resource.getrusage(resource.RUSAGE_SELF).ru_maxrss*1024
    (directory/'benchmark.json').write_text(json.dumps(result,indent=2)+'\n')
    print(json.dumps({key:result.get(key) for key in ['accepted','fittingDimensions','totalFittingSeconds','profileSeconds','generationSeconds','totalSeconds','futureMunicipalities','peakPythonResidentBytes']}),flush=True)
    if not args.fit_only:print(json.dumps({'national':result['nationalScores']['validShares'],'municipal':result['scores']['municipalValidShares'],'stress':result['nationalStress']}),flush=True)

if __name__=='__main__':main()

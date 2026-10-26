"""Reduced real-data pilot before full-data fitting. Reject diagnostic failures."""
from __future__ import annotations
import argparse,hashlib,json,shutil,time,platform,threading,subprocess
from pathlib import Path
import numpy as np
import cmdstanpy

p=argparse.ArgumentParser()
p.add_argument('directory',type=Path)
p.add_argument('--cmdstan',required=True,type=Path)
p.add_argument('--cache',required=True,type=Path)
p.add_argument('--warmup',type=int)
p.add_argument('--sampling',type=int)
args=p.parse_args()
manifest=json.loads((args.directory/'manifest.json').read_text())
source=Path(__file__).with_name('real-party-drift.stan')
for item in manifest['sourceVersions']:
    if hashlib.sha256((source.parent.parent/item['path']).read_bytes()).hexdigest()!=item['sha256']:
        raise RuntimeError('Research implementation changed; prepare a new fingerprint: '+item['path'])
for item in manifest['auxiliaryHashes']:
    if hashlib.sha256((args.directory/item['path']).read_bytes()).hexdigest()!=item['sha256']:
        raise RuntimeError('Prepared auxiliary data changed: '+item['path'])
digest=hashlib.sha256(source.read_bytes()).hexdigest()
if digest!=manifest['modelHash'] or hashlib.sha256((args.directory/'data.json').read_bytes()).hexdigest()!=manifest['dataHash']:
    raise RuntimeError('Prepared source or data hash mismatch')
cmdstanpy.set_cmdstan_path(str(args.cmdstan))
cache=args.cache/digest[:12];cache.mkdir(parents=True,exist_ok=True)
copy=cache/source.name
if not copy.exists():shutil.copyfile(source,copy)
tic=time.monotonic();model=cmdstanpy.CmdStanModel(stan_file=str(copy),cpp_options={'O':'1'})
print(json.dumps({'phase':'compiled','seconds':time.monotonic()-tic}),flush=True)
policy=manifest['policy'].copy()
if args.warmup:policy['warmup']=args.warmup
if args.sampling:policy['sampling']=args.sampling
out=cache/time.strftime('%Y%m%d-%H%M%S',time.gmtime())
peak=[0];stop=threading.Event()
def memory():
    while not stop.wait(.2):
        # All live processes from this executable; isolated benchmark only.
        rss=0
        for proc in Path('/proc').glob('[0-9]*'):
            try:
                if str(model.exe_file) not in (proc/'cmdline').read_bytes().decode(errors='replace'):continue
                status=(proc/'status').read_text()
                rss+=int(next(s.split()[1] for s in status.splitlines() if s.startswith('VmRSS:')))*1024
            except (OSError,StopIteration):pass
        peak[0]=max(peak[0],rss)
monitor=threading.Thread(target=memory,daemon=True);monitor.start()
tic=time.monotonic()
try:
    fit=model.sample(data=str(args.directory/'data.json'),inits=str(args.directory/'initial.json'),metric='diag_e',inv_metric=str(args.directory/'metric.json'),step_size=.05,seed=policy['seed'],chains=4,parallel_chains=4,
        iter_warmup=policy['warmup'],iter_sampling=policy['sampling'],adapt_delta=policy['adaptDelta'],max_treedepth=policy['maxTreedepth'],
        output_dir=str(out),show_progress=False,show_console=True,refresh=100)
except Exception as error:
    (args.directory/'stan-error.json').write_text(json.dumps({'modelHash':digest,'policy':policy,'seconds':time.monotonic()-tic,
        'peakSamplingProcessBytes':peak[0],'error':str(error),'passed':False,'fullDataFitStarted':False},indent=2)+'\n')
    raise
finally:
    stop.set();monitor.join()
elapsed=time.monotonic()-tic
summary=fit.summary();diagnostics=fit.method_variables();varying=summary[summary['StdDev']>1e-10]
energy=diagnostics['energy__'];bfmi=np.mean(np.diff(energy,axis=0)**2,axis=0)/np.var(energy,axis=0)
row={'modelHash':digest,'dataHash':manifest['dataHash'],'policy':policy,'seconds':elapsed,'peakSamplingProcessBytes':peak[0],
    'maxRhat':float(varying['R_hat'].max()),'minEss':float(varying['ESS_bulk'].min()),
    'divergences':int(diagnostics['divergent__'].sum()),'maxTreedepthHits':int((diagnostics['treedepth__']>=policy['maxTreedepth']).sum()),
    'minBfmi':float(bfmi.min()),'csvBytes':sum(Path(x).stat().st_size for x in fit.runset.csv_files),
    'scaleSummary':{k:summary.loc[k].to_dict() for k in ['sigma_n','sigma_l']},
    'platform':platform.platform(),'cmdstan':cmdstanpy.cmdstan_version(),'cmdstanpy':cmdstanpy.__version__,
    'fullDataFitStarted':False,'timeGateWaived':True,'scope':manifest['scope']}
row['passed']=row['maxRhat']<=policy['maxRhat'] and row['minEss']>=policy['minEss'] and row['divergences']==0 and row['maxTreedepthHits']==0 and row['minBfmi']>=.3
ratio=manifest['fullCities']/len(manifest['cities'])
row['roughFullScaleEstimate']={'municipalityRatio':ratio,'secondsLinear':elapsed*ratio,'peakBytesLinear':peak[0]*ratio,'csvBytesLinear':row['csvBytes']*ratio,
    'caveat':'Gross city-linear extrapolation only; NUTS step size, mixing and shared national work need not scale linearly; full hyperprior/aggregate variants add work'}
fit.save_csvfiles(str(args.directory/'stan-csv'));row['csvFiles']=fit.runset.csv_files
(args.directory/'stan-summary.json').write_text(json.dumps(row,indent=2)+'\n')
print(json.dumps({k:v for k,v in row.items() if k not in {'csvFiles','scaleSummary'}}),flush=True)
if not row['passed']:raise SystemExit(2)

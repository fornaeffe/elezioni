"""Fast research fitting and predictive evaluation; production is not touched."""
from __future__ import annotations
import argparse,hashlib,json,shutil,time,platform
from pathlib import Path
import numpy as np
import cmdstanpy
from modular_geography import fit_profiles,generate,score,softmax

def main():
    p=argparse.ArgumentParser();p.add_argument('directory',type=Path);p.add_argument('--cmdstan',type=Path,required=True);p.add_argument('--cache',type=Path,required=True)
    p.add_argument('--warmup',type=int);p.add_argument('--sampling',type=int);p.add_argument('--timeout',type=float,default=120)
    p.add_argument('--draws',type=int,default=128);p.add_argument('--fit-only',action='store_true');args=p.parse_args()
    directory=args.directory;m=json.loads((directory/'manifest.json').read_text());source=Path(__file__).with_name(m['modelSource'])
    for item in m['sourceVersions']:
        if hashlib.sha256((source.parent.parent/item['path']).read_bytes()).hexdigest()!=item['sha256']:raise RuntimeError('Re-export after implementation change: '+item['path'])
    for name,key in [('data.json','dataHash'),('profiles.json','profileHash')]:
        if hashlib.sha256((directory/name).read_bytes()).hexdigest()!=m[key]:raise RuntimeError('Prepared bytes changed: '+name)
    cmdstanpy.set_cmdstan_path(str(args.cmdstan));cache=args.cache/(m['modelHash'][:12]+'-O1-cpp3');cache.mkdir(parents=True,exist_ok=True)
    copy=cache/source.name
    if not copy.exists():shutil.copyfile(source,copy)
    tic=time.monotonic();model=cmdstanpy.CmdStanModel(stan_file=str(copy),stanc_options={'O1':True},cpp_options={'O':'3'})
    compile_seconds=time.monotonic()-tic;print(json.dumps({'phase':'compiled','seconds':compile_seconds}),flush=True)
    policy=m['policy'].copy()
    if args.warmup:policy['warmup']=args.warmup
    if args.sampling:policy['sampling']=args.sampling
    tic=time.monotonic()
    try:
        fit=model.sample(data=str(directory/'data.json'),chains=policy['chains'],parallel_chains=policy['chains'],seed=policy['seed'],
          iter_warmup=policy['warmup'],iter_sampling=policy['sampling'],adapt_delta=policy['adaptDelta'],max_treedepth=policy['maxTreedepth'],
          inits=0,output_dir=str(cache/time.strftime('%Y%m%d-%H%M%S',time.gmtime())),show_progress=False,refresh=100,timeout=args.timeout)
    except Exception as error:
        (directory/'fit-error.json').write_text(json.dumps({'seconds':time.monotonic()-tic,'error':str(error),'policy':policy,'accepted':False},indent=2)+'\n');raise
    seconds=time.monotonic()-tic;summary=fit.summary();method=fit.method_variables();varying=summary[summary['StdDev']>1e-10]
    energy=method['energy__'];bfmi=np.mean(np.diff(energy,axis=0)**2,axis=0)/np.var(energy,axis=0)
    diagnostic={'maxRhat':float(varying['R_hat'].max()),'minEss':float(varying['ESS_bulk'].min()),'divergences':int(method['divergent__'].sum()),
       'depthHits':int((method['treedepth__']>=policy['maxTreedepth']).sum()),'minBfmi':float(bfmi.min()),'meanLeapfrogSteps':float(method['n_leapfrog__'].mean())}
    accepted=diagnostic['maxRhat']<=policy['maxRhat'] and diagnostic['minEss']>=policy['minEss'] and diagnostic['divergences']==0 and diagnostic['depthHits']==0 and diagnostic['minBfmi']>=.3
    reference=fit.stan_variable('reference_state');kind=fit.stan_variable('kind_effect');sigma=fit.stan_variable('sigma_n')
    np.savez_compressed(directory/'national-posterior.npz',reference_state=reference,kind_effect=kind,sigma_n=sigma)
    fit.save_csvfiles(str(directory/'stan-csv'))
    result={'version':m['version'],'modelHash':m['modelHash'],'policy':policy,'diagnostics':diagnostic,'accepted':accepted,'fitSeconds':seconds,'compileSeconds':compile_seconds,
      'parameters':len(varying),'scope':m['scope'],'platform':platform.platform(),'cmdstan':cmdstanpy.cmdstan_version(),
      'fullFitEstimate':'National fitting dimension is independent of municipality count; geographic preparation/generation must be benchmarked separately',
      'scaleSummary':summary.loc['sigma_n'].to_dict()}
    (directory/'fit-summary.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(result),flush=True)
    if not accepted:raise SystemExit(2)
    if args.fit_only:return
    data=json.loads((directory/'data.json').read_text());profiles=json.loads((directory/'profiles.json').read_text())
    rng=np.random.default_rng(policy['seed']);n=args.draws;selected=rng.integers(len(reference),size=n);horizon=m['horizon']/365.25
    state=reference[selected]+rng.normal(size=(n,len(m['components'])))*sigma[selected,None]*np.sqrt(horizon)
    if profiles['targetKind']=='european':state+=kind[selected]
    logits=state@np.array(data['H']).T;logits[:,0]+=data['state_offset'];mean=softmax(logits)
    national=np.array([rng.dirichlet(data['concentration']*v) for v in mean])
    tic=time.monotonic();profile=fit_profiles(profiles);profile_seconds=time.monotonic()-tic
    tic=time.monotonic();draws,audit=generate(national,profile,horizon,seed=policy['seed']);generation_seconds=time.monotonic()-tic
    scores=score(draws,profile);audit.pop('expectedProbabilityMeans')
    result.update({'draws':n,'profileSeconds':profile_seconds,'generationSeconds':generation_seconds,'profileAudit':profile['audit'],'generationAudit':audit,'scores':scores})
    (directory/'forecast-summary.json').write_text(json.dumps(result,indent=2)+'\n')
    print(json.dumps({k:result[k] for k in ['accepted','fitSeconds','profileSeconds','generationSeconds','generationAudit']}),flush=True)
    print(json.dumps({'scores':scores['municipalElectorShares']}),flush=True)

if __name__=='__main__':main()

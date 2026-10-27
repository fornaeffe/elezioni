"""Research runner for the bounded reference-share alternative."""
from __future__ import annotations
import argparse,hashlib,json,time
from pathlib import Path
import numpy as np
from fast_party_model import political_precision,turnout_precision,sample,evolve
from modular_geography import fit_profiles,generate,score,national_score

def fit(source,cache,kind_effect=True):
    tic=time.monotonic();Q,b,point,audit=political_precision(source,source['polls'],kind_effect=kind_effect)
    construction=time.monotonic()-tic;K=len(source['components']);political=turnout=None;pd=td=None
    for draws in [2000,4000,8000]:
        political,pd=sample(Q,b,point,K,cache,draws=draws)
        if pd['passed']:break
    Q0,b0,start0=turnout_precision(source,kind_effect=kind_effect)
    for draws in [2000,4000,8000]:
        turnout,td=sample(Q0,b0,start0,2,cache,draws=draws,seed=20261028)
        if td['passed']:break
    return political,turnout,{'constructionSeconds':construction,'fitSeconds':time.monotonic()-tic,
        'politicalDiagnostics':pd,'turnoutDiagnostics':td,'audit':audit,'accepted':pd['passed'] and td['passed']}

def forecast(source,political,turnout,audit,draws=128,seed=20261027,regional=True,component_scales=True,shock_factor=1):
    rng=np.random.default_rng(seed);K=len(source['components']);blocks=political.shape[2]//K;kind=int(source['targetKind']=='european') if blocks==2 else 0
    q=political.reshape(-1,blocks,K);v=turnout.reshape(-1,blocks,2)
    q=q[rng.integers(len(q),size=draws),kind];v=v[rng.integers(len(v),size=draws),kind]
    horizon=(np.datetime64(source['targetDate'])-np.datetime64(source['cutoff'])).astype(int)/365.25
    sigma=np.clip(audit['sigmaPolitical']*np.exp(rng.normal(0,.2,draws)),.06,1)
    sigma0=np.clip(audit['sigmaParticipation']*np.exp(rng.normal(0,.2,draws)),.025,.7)
    q=evolve(q,sigma,horizon,rng);v=evolve(v,sigma0,horizon,rng)
    if shock_factor>0:q=evolve(q,np.full(draws,audit['electionShockSd']*shock_factor),1,rng)
    national=np.c_[v[:,0],v[:,1,None]*q]
    tic=time.monotonic();profile=fit_profiles(source)
    if not component_scales:profile['scale'][:]=np.sqrt(profile['audit']['commonLocalVariancePerYear'])
    profile_seconds=time.monotonic()-tic;tic=time.monotonic()
    available=np.zeros(national.shape[1],bool);available[0]=True
    for ballot in profile['ballots']:
        for group in ballot:available[group]=True
    national[:,~available]=0;national/=national.sum(axis=1,keepdims=True)
    counts,ga=generate(national,profile,horizon,seed=seed,regional=regional);generation_seconds=time.monotonic()-tic
    ga.pop('expectedProbabilityMeans');scores=score(counts,profile)
    valid=national[:,1:]/national[:,1:].sum(axis=1,keepdims=True)
    return {'draws':draws,'profileSeconds':profile_seconds,'generationSeconds':generation_seconds,'profileAudit':profile['audit'],'generationAudit':ga,
      'nationalStress':{'namedComponent80':float(np.mean(np.max(valid,axis=1)>.8)),'nonValid90':float(np.mean(national[:,0]>.9))},
      'scores':scores,'nationalScores':national_score(national,source),'nationalIndexMeans':national.mean(axis=0).tolist()}

def main():
    p=argparse.ArgumentParser();p.add_argument('directory',type=Path);p.add_argument('--cache',type=Path,required=True)
    p.add_argument('--draws',type=int,default=128);p.add_argument('--fit-only',action='store_true');p.add_argument('--pooled-kind',action='store_true');args=p.parse_args()
    directory=args.directory;manifest=json.loads((directory/'manifest.json').read_text());source=json.loads((directory/'profiles.json').read_text())
    if hashlib.sha256((directory/'profiles.json').read_bytes()).hexdigest()!=manifest['profileHash']:raise RuntimeError('Prepared profile input changed')
    for item in manifest['sourceVersions']:
        if hashlib.sha256((Path(__file__).parent.parent/item['path']).read_bytes()).hexdigest()!=item['sha256']:raise RuntimeError('Re-export after code change: '+item['path'])
    political,turnout,result=fit(source,args.cache,kind_effect=not args.pooled_kind);result.update({'version':manifest['version'],'model':'bounded-reference-share-v1',
      'scope':'Gaussian share-information approximation, conditional covariance and empirical geographic profiles; former joint multinomial model superseded for this candidate',
      'fittingDimensions':{'political':political.shape[2],'participation':turnout.shape[2]},
      'fullFitEstimate':'National fit uses the same small matrices at full municipal scale; profile preparation and generation benchmark separately'})
    name='fast-pooled' if args.pooled_kind else 'fast-kind'
    np.savez_compressed(directory/(name+'-posterior.npz'),political=political,turnout=turnout)
    (directory/(name+'-fit.json')).write_text(json.dumps(result,indent=2)+'\n');print(json.dumps({k:result[k] for k in ['model','accepted','fitSeconds','constructionSeconds','fittingDimensions']}),flush=True)
    print(json.dumps({'political':{k:result['politicalDiagnostics'][k] for k in ['seconds','maxRhat','minEss','minTailEss']},'turnout':{k:result['turnoutDiagnostics'][k] for k in ['seconds','maxRhat','minEss','minTailEss']}}),flush=True)
    if not result['accepted']:raise SystemExit(2)
    if not args.fit_only:
        result.update(forecast(source,political,turnout,result['audit'],draws=args.draws))
        (directory/(name+'-forecast.json')).write_text(json.dumps(result,indent=2)+'\n')
        print(json.dumps({k:result[k] for k in ['profileSeconds','generationSeconds','generationAudit','nationalStress']}),flush=True)
        print(json.dumps(result['scores']['municipalElectorShares']),flush=True)

if __name__=='__main__':main()

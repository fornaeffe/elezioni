"""Portable research posterior contract; no runtime installation or activation."""
from __future__ import annotations
import argparse,gzip,hashlib,json
from pathlib import Path
import numpy as np
from modular_geography import fit_profiles

def main():
    p=argparse.ArgumentParser();p.add_argument('directory',type=Path);p.add_argument('--output',type=Path,required=True);p.add_argument('--draws',type=int,default=1024);p.add_argument('--shock-factor',type=float,default=1.5);args=p.parse_args()
    directory=args.directory;manifest=json.loads((directory/'manifest.json').read_text());source=json.loads((directory/'profiles.json').read_text());meta=json.loads((directory/'fast-kind-fit.json').read_text())
    if not meta['accepted']:raise RuntimeError('An accepted posterior is required')
    if meta.get('version')!=manifest['version']:raise RuntimeError('Fit and prepared input versions differ')
    if hashlib.sha256((directory/'profiles.json').read_bytes()).hexdigest()!=manifest['profileHash']:raise RuntimeError('Prepared profile input changed')
    for item in manifest['sourceVersions']:
        if hashlib.sha256((Path(__file__).parent.parent/item['path']).read_bytes()).hexdigest()!=item['sha256']:raise RuntimeError('Re-export after code change: '+item['path'])
    posterior=np.load(directory/'fast-kind-posterior.npz');K=len(source['components']);political=posterior['political'].reshape(-1,2,K);turnout=posterior['turnout'].reshape(-1,2,2)
    if args.draws<1 or args.draws>min(len(political),len(turnout)):raise ValueError('Draw count must be positive and no larger than the accepted posterior')
    for values in [political,turnout]:
        if not np.all(np.isfinite(values)) or np.any(values<0) or not np.allclose(values.sum(axis=-1),1,atol=1e-10):raise RuntimeError('Invalid posterior probabilities')
    selected=np.linspace(0,len(political)-1,args.draws,dtype=int);selected0=np.linspace(0,len(turnout)-1,args.draws,dtype=int);country={}
    for kind,h in [('camera',0),('european',1)]:
        v=turnout[selected0,h];q=political[selected,h];country[kind]=np.c_[v[:,0],v[:,1,None]*q].tolist()
    profile=fit_profiles(source)
    profile['scale'][:]=np.sqrt(profile['audit']['commonLocalVariancePerYear'])
    result={'schema':'party-share-research-v1','researchOnly':True,'referenceDate':source['cutoff'],'sourceFingerprint':manifest['version'],'componentIds':source['components'],
      'model':'kind-common-conservative','exportSourceHash':hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
      'countryReferenceDraws':country,'estimatedParameters':{**{key:meta['audit'][key] for key in ['sigmaPolitical','sigmaParticipation']},
        'calibratedElectionShockSd':meta['audit']['electionShockSd'],'electionShockMultiplier':args.shock_factor,'electionShockSd':meta['audit']['electionShockSd']*args.shock_factor},
      'historicalListTemplates':source.get('listTemplates',[]),
      'geography':{'kind':source['targetKind'],'municipalityIds':profile['cities'],'regions':profile['regions'],'electors':profile['electors'].tolist(),
        'logProfile':profile['offset'].tolist(),'profileSd':profile['profileSd'].tolist(),'localInnovationLogitSd':profile['scale'].tolist(),'fallback':profile['fallback'].tolist()},
      'scenarioContract':{'historicalAtomsIndivisible':True,'jointListsSumComponentCounts':True,'newListRequiresReferenceShare':True,
        'referenceShareUnit':'valid-voter fraction','referenceUncertaintyUnit':'percentage-point SD','referenceSemantics':'Intervention on reference support; future movement and election realization uncertainty remain',
        'newGeographyDefault':'national profile, log-profile SD 0.45, local innovation 0.25 per square-root year',
        'uncertaintyOverrides':'National/local list uncertainty multipliers default 1; new component national volatility defaults to fitted common scale',
        'browserIntegration':'Portable data contract and headless prototype only; browser workers/editor implementation and parity acceptance pending',
        'chambers':'Camera/European research only; no fitted Senate or paired-chamber acceptance'},
      'fitDiagnostics':{'political':{key:meta['politicalDiagnostics'][key] for key in ['maxRhat','minEss','minTailEss']},'turnout':{key:meta['turnoutDiagnostics'][key] for key in ['maxRhat','minEss','minTailEss']}},
      'sourceVersions':manifest['sourceVersions'],'inputVersions':manifest['inputs']}
    args.output.parent.mkdir(parents=True,exist_ok=True)
    with args.output.open('wb') as file:
        with gzip.GzipFile(filename='',mode='wb',fileobj=file,mtime=0,compresslevel=9) as handle:handle.write(json.dumps(result,separators=(',',':')).encode('utf8'))
    print(json.dumps({'path':str(args.output),'bytes':args.output.stat().st_size,'sha256':hashlib.sha256(args.output.read_bytes()).hexdigest(),'municipalities':len(profile['cities']),'posteriorDraws':args.draws}))

if __name__=='__main__':main()

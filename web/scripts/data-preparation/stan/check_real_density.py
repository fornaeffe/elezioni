"""Independent NumPy density differences and finite-difference gradient checks.

The mathematical construction is separate from Stan's AD implementation. This
also checks its unconstrained array/matrix ordering used by the initial metric.
"""
from __future__ import annotations
import argparse,copy,hashlib,json,math
from pathlib import Path
import numpy as np
from scipy.special import logsumexp
import cmdstanpy

p=argparse.ArgumentParser();p.add_argument('directory',type=Path);p.add_argument('--cmdstan',type=Path,required=True);p.add_argument('--cache',type=Path,required=True);args=p.parse_args()
m=json.loads((args.directory/'manifest.json').read_text());d=json.loads((args.directory/'data.json').read_text());initial=json.loads((args.directory/'initial.json').read_text())
cmdstanpy.set_cmdstan_path(str(args.cmdstan));model=cmdstanpy.CmdStanModel(exe_file=str(args.cache/m['modelHash'][:12]/'real-party-drift'))
J,C,T,U,B=d['J'],d['C'],d['T'],d['U'],d['B'];D=J-1;H=np.array(d['H'])
def decode(v,groups,offset):
    s=np.r_[0,np.array(v,dtype=float)]
    for group in sorted(set(groups)-{0,1}):
        members=np.flatnonzero(np.array(groups)==group);first=members[0]
        within=np.r_[0,np.array(v)[members[1:]-1]]
        s[members]=v[first-1]+within-logsumexp(within)
    s[0]-=offset
    return s
def independent(x):
    raw=np.array(x['national_raw']);bias=np.array(x['bias_raw']).reshape(B,D);coordinate=np.array(x['coordinate'])
    sn=math.exp(x['log_sigma_n']);sl=math.exp(x['log_sigma_l'])
    national=np.empty((U,D));national[-1]=1.5*raw[-1]
    for u in range(U-2,-1,-1):national[u]=national[u+1]+sn*np.sqrt(d['dt'][u])*raw[u]
    value=-.5*(np.square(raw).sum()+np.square(bias).sum())-.5*((x['log_sigma_n']-np.log(.35))/.7)**2-.5*((x['log_sigma_l']-np.log(.25))/.7)**2
    cov=.25+sl*sl*np.array(d['backward_cov']);sign,ld=np.linalg.slogdet(cov);assert sign==1
    for c in range(C):
        residual=np.array([decode(coordinate[c,t],d['coord_group'][c][t],d['state_offset'])@H-national[d['count_time'][t]-1] for t in range(T)])
        value-=.5*(np.sum(residual*np.linalg.solve(cov,residual))+D*ld)
    for n in range(d['N']):
        s=decode(coordinate[d['cell_c'][n]-1,d['cell_t'][n]-1],d['cell_group'][n],0)
        a=np.array([logsumexp(s[np.array(d['cell_group'][n])==g]) for g in range(1,d['cell_groups'][n]+1)])
        value+=np.array(d['y'][n][:len(a)])@(a-logsumexp(a))
    for q in range(d['Q']):
        s=H@(national[d['poll_time'][q]-1]+.12*bias[d['poll_bias'][q]-1]);G=d['poll_groups'][q]
        a=np.zeros(J)
        for g in range(1,G+1):a[g-1]=logsumexp(s[np.array(d['poll_group'][q])==g])
        residual=(np.array(d['poll_H'][q]).T@(a-d['poll_log_shares'][q]))[:G-1]
        value-=.5*np.square(np.linalg.solve(np.array(d['poll_L'][q])[:G-1,:G-1],residual)).sum()
    return float(value)
def flatten(x):
    return np.r_[*[np.array(a).flatten(order='F') for a in x['coordinate']],np.array(x['national_raw']).flatten(order='F'),np.array(x['bias_raw']).flatten(order='F'),x['log_sigma_n'],x['log_sigma_l']]
def unflatten(v):
    i=0;x=copy.deepcopy(initial)
    for c in range(C):x['coordinate'][c]=v[i:i+T*D].reshape((T,D),order='F').tolist();i+=T*D
    x['national_raw']=v[i:i+U*D].reshape((U,D),order='F').tolist();i+=U*D
    x['bias_raw']=v[i:i+B*D].reshape((B,D),order='F').tolist();i+=B*D
    x['log_sigma_n']=float(v[i]);x['log_sigma_l']=float(v[i+1]);return x
v=flatten(initial);rng=np.random.default_rng(20261026)
def stan(x):return model.log_prob(params=x,data=d,sig_figs=18).iloc[0].to_numpy()
base=stan(initial);differences=[]
for k in range(4):
    x=unflatten(v+rng.normal(0,.03,len(v)))
    differences.append(abs((stan(x)[0]-base[0])-(independent(x)-independent(initial))))
gradient_errors=[]
indices=sorted(set([0,T-1,T*D-1,T*D,C*T*D-1,len(v)-2,len(v)-1]+np.linspace(0,len(v)-1,15,dtype=int).tolist()))
for i in indices:
    a=v.copy();b=v.copy();a[i]+=1e-5;b[i]-=1e-5
    fd=(independent(unflatten(a))-independent(unflatten(b)))/2e-5
    gradient_errors.append({'index':int(i),'stan':float(base[i+1]),'finiteDifference':fd,'error':abs(base[i+1]-fd)})
result={'modelHash':m['modelHash'],'dataHash':m['dataHash'],'maximumDensityDifferenceError':max(differences),
        'gradientChecks':gradient_errors,'maximumGradientError':max(r['error'] for r in gradient_errors)}
result['passed']=max(differences)<1e-6 and all(r['error']<1e-3+abs(r['finiteDifference'])*1e-6 for r in gradient_errors)
(args.directory/'independent-density-check.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result));
if not result['passed']:raise SystemExit(2)

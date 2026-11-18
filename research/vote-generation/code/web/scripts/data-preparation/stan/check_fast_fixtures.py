"""Independent quadrature, sampler diagnostics, conservation and forecast gates."""
from __future__ import annotations
import argparse,json,time
from pathlib import Path
import numpy as np
from numpy.polynomial.legendre import leggauss
from fast_party_model import sample,mode,evolve
from modular_geography import rake,sample_counts

def quadrature(Q,b,k,blocks,order):
    x,w=leggauss(order);x=(x+1)/2;w=w/2;u,v=np.meshgrid(x,x,indexing='ij');wu,wv=np.meshgrid(w,w,indexing='ij')
    if k==3 and blocks==1:points=np.stack([u,(1-u)*v,(1-u)*(1-v)],axis=-1).reshape(-1,3);weights=(wu*wv*(1-u)).ravel()
    elif k==2 and blocks==2:points=np.stack([u,1-u,v,1-v],axis=-1).reshape(-1,4);weights=(wu*wv).ravel()
    else:raise ValueError('Fixture quadrature dimension')
    log=-.5*np.einsum('ni,ij,nj->n',points,Q,points)+points@b;weights*=np.exp(log-log.max());weights/=weights.sum()
    means=weights@points;quantiles=[]
    for j in range(len(b)):
        order=np.argsort(points[:,j]);quantiles.append(np.interp([.05,.95],np.cumsum(weights[order]),points[order,j]).tolist())
    return means,np.array(quantiles)

def main():
    p=argparse.ArgumentParser();p.add_argument('--cache',type=Path,required=True);p.add_argument('--output',type=Path,required=True);args=p.parse_args();start=time.monotonic();results=[]
    cases=[]
    for name,centre,precision in [('interior',[.3,.45,.25],[800,500,400]),('near-zero',[-.1,.7,.4],[900,200,200])]:
        Q=np.diag(precision);b=Q@centre;cases.append((name,Q,b,3,1))
    A=np.array([[.7,-.3,-.3],[0,.4,-.6]])
    Q=A.T@np.diag([2000,100])@A+np.eye(3)*4;b=np.ones(3)*4/3;cases.append(('joint-and-distinguishing-information',Q,b,3,1))
    Q=np.eye(4)*900;mu=np.array([.35,.65,.55,.45]);b=Q@mu
    for j in range(2):Q[j,j]+=1800;Q[2+j,2+j]+=1800;Q[j,2+j]-=1800;Q[2+j,j]-=1800
    cases.append(('coupled-kinds',Q,b,2,2))
    for name,Q,b,k,blocks in cases:
        point=mode(Q,b,k,blocks);draws,diagnostic=sample(Q,b,point,k,args.cache,draws=4000,burn=1000,seed=20261027)
        mean,interval=quadrature(Q,b,k,blocks,320);coarse,_=quadrature(Q,b,k,blocks,240)
        actual=draws.reshape(-1,len(b));actual_interval=np.quantile(actual,[.05,.95],axis=0).T
        row={'fixture':name,'diagnostics':diagnostic,'maximumMeanError':float(np.max(np.abs(actual.mean(axis=0)-mean))),
          'maximumIntervalEndpointError':float(np.max(np.abs(actual_interval-interval))),'quadratureMeanRefinementError':float(np.max(np.abs(mean-coarse)))}
        row['passed']=diagnostic['passed'] and row['maximumMeanError']<.005 and row['maximumIntervalEndpointError']<.008 and row['quadratureMeanRefinementError']<1e-6
        results.append(row);print(json.dumps({k:v for k,v in row.items() if k!='diagnostics'}),flush=True)
    rng=np.random.default_rng(20261027);reference=np.tile([.30,.25,.20,.10,.08,.07],(120000,1));future=evolve(reference,np.full(len(reference),.4),2,rng)
    martingale_error=float(np.max(np.abs(future.mean(axis=0)-reference[0])));extreme=float(np.mean(future.max(axis=1)>.8))
    p,iterations=rake([[.2,.7,.1],[.8,.1,.1]],[100,900],[.3,.4,.3]);counts=sample_counts(p,[100,900],rng)
    invariant=bool(np.all(counts>=0) and np.all(counts.sum(axis=1)==[100,900]) and np.max(np.abs(np.array([100,900])@p/1000-[.3,.4,.3]))<1e-9)
    result={'fixtures':results,'martingaleMeanError':martingale_error,'nationalExtreme80Rate':extreme,'projectionIterations':iterations,'integerElectorConservation':invariant,
       'seconds':time.monotonic()-start,'passed':all(r['passed'] for r in results) and martingale_error<.002 and extreme<=.005 and invariant}
    args.output.parent.mkdir(parents=True,exist_ok=True);args.output.write_text(json.dumps(result,indent=2)+'\n');print(json.dumps({k:v for k,v in result.items() if k!='fixtures'}))
    if not result['passed']:raise SystemExit(2)

if __name__=='__main__':main()

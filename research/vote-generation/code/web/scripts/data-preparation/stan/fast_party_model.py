"""Bounded reference-share research model; small native Gaussian Gibbs kernel.

The source observations are grouped proportions with covariance for sampling,
rounding, shared polling error and shared historical movement. Covariance is
linearized at training-only reference estimates, an empirical-Bayes approximation.
This is deliberately different from the original grouped multinomial drift model.
"""
from __future__ import annotations
import hashlib,json,math,subprocess,time
from pathlib import Path
import numpy as np
from scipy.linalg import cho_factor,cho_solve,helmert
from scipy.special import ndtri
from scipy.stats import rankdata

def estimate_scales(source):
    K=len(source['components']);index={s:i for i,s in enumerate(source['components'])};history=source['nationalHistory'];rates=[];turn=[];audit=[]
    for kind in ['camera','european']:
        records=[e for e in history if e['kind']==kind]
        for a,b in zip(records[:-1],records[1:]):
            dt=(np.datetime64(b['date'])-np.datetime64(a['date'])).astype(int)/365.25
            def named(e):return{g[0]:(n+.5)/(sum(e['counts'][1:])+.5*len(e['groups'])) for g,n in zip(e['groups'],e['counts'][1:]) if len(g)==1 and n/sum(e['counts'][1:])>=.005}
            aa=named(a);bb=named(b);common=sorted(set(aa)&set(bb));changes=[]
            for i,j in [(i,j) for i in range(len(common)) for j in range(i)]:
                x,y=common[i],common[j];changes.append((math.log(bb[x]/bb[y])-math.log(aa[x]/aa[y]))**2/(2*dt))
            if changes:rates.append(float(np.median(changes)))
            va=a['counts'][0]/a['electors'];vb=b['counts'][0]/b['electors']
            turn.append((math.log(vb/(1-vb))-math.log(va/(1-va)))**2/dt)
            audit.append({'from':a['id'],'to':b['id'],'kind':kind,'politicalPairs':len(changes),'varianceRateMedian':rates[-1] if changes else None})
    sigma=math.sqrt((4*.35**2+sum(rates))/(4+len(rates)))
    sigma0=math.sqrt((4*.15**2+sum(turn))/(4+len(turn)))
    return float(np.clip(sigma,.12,.7)),float(np.clip(sigma0,.05,.5)),audit

def initial_reference(source):
    K=len(source['components']);idx={s:i for i,s in enumerate(source['components'])};q=np.ones(K)/K
    for e in source['nationalHistory']:
        V=sum(e['counts'][1:]);offered=set(j for g in e['groups'] for j in g);new=q.copy()
        for g,n in zip(e['groups'],e['counts'][1:]):
            js=[idx[s] for s in g];weights=q[js]/q[js].sum();new[js]=(n+.5)/(V+.5*len(e['groups']))*weights
        q=new/np.sum(new)
    return np.maximum(q,1e-4)/np.maximum(q,1e-4).sum()

def poll_discrepancy(source,sigma):
    """One training-election unit per latest compatible pre-election survey.
    Coarsen both memberships collectively before comparing; never infer counts.
    """
    events=[];ids=source['components']
    for event in source['nationalHistory']:
        candidates=[p for p in source['polls'] if p['kind']==event['kind'] and p['available']<=event['date'] and p['end']<=event['date']
          and 0<=(np.datetime64(event['date'])-np.datetime64(p['date'][:10])).astype(int)<=45]
        if not candidates:continue
        p=max(candidates,key=lambda q:q['date']);parent={s:s for s in ids}
        def root(s):
            while parent[s]!=s:s=parent[s]
            return s
        for g in event['groups']+p['groups']:
            for member in g[1:]:parent[root(member)]=root(g[0])
        actual={};reported={}
        for g,n in zip(event['groups'],event['counts'][1:]):actual[root(g[0])]=actual.get(root(g[0]),0)+n
        for g,r in zip(p['groups'],p['shares']):reported[root(g[0])]=reported.get(root(g[0]),0)+r
        common=[s for s in actual if s in reported];ta=sum(actual[s] for s in common);tr=sum(reported[s] for s in common)
        if ta<=0 or tr<=0:continue
        pairs=[];gap=float((np.datetime64(event['date'])-np.datetime64(p['date'][:10])).astype(int))/365.25
        for i,s in enumerate(common):
            for t in common[:i]:
                a,b=actual[s]/ta,actual[t]/ta;r,u=reported[s]/tr,reported[t]/tr
                if min(a,b,r,u)<.005:continue
                error=math.log(a/b)-math.log(r/u);sampling=(1/r+1/u)/p['sample']
                pairs.append(max(0,error*error/2-sampling/2-sigma*sigma*gap-.12**2))
        if pairs:events.append({'election':event['id'],'poll':p['id'],'gapYears':gap,'variance':float(np.median(pairs)),'contrasts':len(pairs)})
    scale=math.sqrt((4*.25**2+sum(e['variance'] for e in events))/(4+len(events)))
    return float(np.clip(scale,.10,.50)),events

def mode(Q,b,k,blocks):
    # Feasible active-set convex QP. Scaling an entire objective by its largest
    # rare-party precision made SLSQP lose the informative larger-party scales.
    d=k*blocks;B=np.zeros((blocks,d))
    for h in range(blocks):B[h,h*k:(h+1)*k]=1
    x=np.ones(d)/k;active=np.zeros(d,bool)
    for iteration in range(20*d):
        free=np.flatnonzero(~active);F=B[:,free];L=cho_factor(Q[np.ix_(free,free)],lower=True)
        u=cho_solve(L,b[free]);V=cho_solve(L,F.T);lagrange=np.linalg.solve(F@V,F@u-np.ones(blocks));candidate=u-V@lagrange
        direction=candidate-x[free]
        negative=np.flatnonzero(candidate< -1e-12)
        if len(negative):
            falling=negative[direction[negative]<0]
            ratios=-x[free[falling]]/direction[falling];blocking=int(np.argmin(ratios));step=np.clip(ratios[blocking],0,1)
            x[free]+=step*direction;hit=free[falling[blocking]]
            x[hit]=0;active[hit]=True;continue
        x[:]=0;x[free]=np.maximum(candidate,0);gradient=Q@x-b+B.T@lagrange
        tolerance=1e-8+64*np.finfo(float).eps*(np.abs(Q)@np.abs(x)+np.abs(b)+np.abs(B.T@lagrange))
        violation=np.flatnonzero(active&(gradient< -tolerance))
        if not len(violation):
            if np.max(np.abs(B@x-1))>1e-7:raise RuntimeError('Reference QP equality residual')
            point=np.maximum(x.reshape(blocks,k),1e-12);point/=point.sum(axis=1,keepdims=True);return point.ravel()
        active[violation[np.argmin(gradient[violation])]]=False
    raise RuntimeError('Reference active-set QP did not converge')

def political_precision(source,polls,kind_effect=True,iterations=3):
    K=len(source['components']);idx={s:i for i,s in enumerate(source['components'])};blocks=2 if kind_effect else 1
    qbar=initial_reference(source);sigma,sigma0,scale_audit=estimate_scales(source);cutoff=np.datetime64(source['cutoff'])
    records=[]
    for e in source['nationalHistory']:
        V=sum(e['counts'][1:]);records.append({'id':e['id'],'date':e['date'],'kind':e['kind'],'groups':e['groups'],'shares':np.array(e['counts'][1:])/V,
          'sample':V,'poll':False,'period':None,'rounding':0,'varianceShares':(np.array(e['counts'][1:])+.5)/(V+.5*len(e['groups']))})
    for p in polls:
        records.append({'id':p['id'],'date':p['date'][:10],'kind':p['kind'],'groups':p['groups'],'shares':np.array(p['shares']),
          'sample':p['sample'],'poll':True,'period':p['group'],'rounding':.001,'varianceShares':np.array(p['shares'])})
    designs=[];global_design=[];noise=[];ages=[];period=[];record_audit=[]
    # Residual A*q=0 uses observed answers without inventing internal shares.
    for r in records:
        G=len(r['groups']);H=helmert(G,full=False);S=np.zeros((G,K))
        for g,members in enumerate(r['groups']):
            for member in members:S[g,idx[member]]=1
        available=S.sum(axis=0);A=H@(S-r['shares'][:,None]*available)
        X=np.zeros((G-1,blocks*K));h=int(r['kind']=='european') if blocks==2 else 0;X[:,h*K:(h+1)*K]=A
        designs.append(X);global_design.append(A);v=r['varianceShares']
        cov=H@((np.diag(v)-np.outer(v,v))/r['sample']+np.eye(G)*r['rounding']**2/12)@H.T
        noise.append((cov,available));age=max(0,float((cutoff-np.datetime64(r['date'])).astype(int))/365.25)
        ages.extend([age]*(G-1));period.extend([r['period']]*(G-1));record_audit.append({'id':r['id'],'ageYears':age,'answers':G,'originalSample':r['sample'],'role':'poll' if r['poll'] else 'election'})
    X=np.vstack(designs);A=np.vstack(global_design);ages=np.array(ages);n=len(ages);shared=np.minimum(ages[:,None],ages[None,:])
    survey=np.array([[x is not None and x==y for y in period] for x in period],float)
    historical=[]
    for member in source['components']:
        separate=[];bounds=[]
        for record in records:
            for group,share in zip(record['groups'],record['shares']):
                if member in group:
                    bounds.append(share)
                    if len(group)==1:separate.append(share)
        historical.append(max(separate) if separate else min(bounds) if bounds else 1e-4)
    historical=np.maximum(historical,1e-4)
    for iteration in range(iterations):
        scales=np.maximum(qbar,historical)
        J=np.diag(qbar)-np.outer(qbar,qbar);process=np.diag(scales)-np.outer(qbar,scales)
        bias=A@J;movement=A@process
        covariance=sigma*sigma*shared*(movement@movement.T)+.12**2*survey*(bias@bias.T)
        offset=0
        for cov,available in noise:
            m=len(cov);mass=max(1e-5,float(available@qbar));covariance[offset:offset+m,offset:offset+m]+=cov*mass*mass;offset+=m
        # A declared, negligible arithmetic-noise variance, not a posterior repair.
        covariance+=np.eye(n)*1e-16
        L=cho_factor(covariance,lower=True,check_finite=True)
        Q=X.T@cho_solve(L,X);b=np.zeros(blocks*K)
        # Weak proper Gaussian reference regularization on the bounded simplex.
        Q+=np.eye(blocks*K)*4;b+=4*np.tile(np.ones(K)/K,blocks)
        if blocks==2:
            # Approximate half-logit kind variation, scaled by reference shares.
            precision=1/np.square(.5*scales)
            for j,w in enumerate(precision):Q[j,j]+=w;Q[K+j,K+j]+=w;Q[j,K+j]-=w;Q[K+j,j]-=w
        Q=(Q+Q.T)/2;point=mode(Q,b,K,blocks)
        qbar=np.maximum(point.reshape(blocks,K).mean(axis=0),1e-4);qbar/=qbar.sum()
    shock,calibration=poll_discrepancy(source,sigma)
    return Q,b,point,{'sigmaPolitical':sigma,'sigmaParticipation':sigma0,'electionShockSd':shock,'pollCalibration':calibration,
      'historicalScaleBounds':historical.tolist(),'scaleTransitions':scale_audit,'linearization':qbar.tolist(),
      'iterations':iterations,'observations':record_audit,'kindEffects':kind_effect,'sourceRoles':'Grouped source proportions and reported sampling covariance; Gaussian movement approximation, not the original multinomial posterior'}

def turnout_precision(source,kind_effect=True):
    blocks=2 if kind_effect else 1;k=2;sigma,sigma0,_=estimate_scales(source);records=source['nationalHistory'];N=len(records)
    p=np.array([e['counts'][0]/e['electors'] for e in records]);bar=float(p[-1]);ages=np.array([max(0,float((np.datetime64(source['cutoff'])-np.datetime64(e['date'])).astype(int))/365.25) for e in records])
    covariance=sigma0**2*bar**2*(1-bar)**2*np.minimum(ages[:,None],ages[None,:])+np.diag(p*(1-p)/np.array([e['electors'] for e in records]))+np.eye(N)*1e-16
    X=np.zeros((N,2*blocks))
    for i,e in enumerate(records):h=int(e['kind']=='european') if blocks==2 else 0;X[i,2*h:2*h+2]=[1-p[i],-p[i]]
    Q=X.T@cho_solve(cho_factor(covariance),X)+np.eye(2*blocks)*4;b=np.ones(2*blocks)*2
    if blocks==2:
        w=1/(.5*bar*(1-bar))**2
        for j in range(2):Q[j,j]+=w;Q[2+j,2+j]+=w;Q[j,2+j]-=w;Q[2+j,j]-=w
    Q=(Q+Q.T)/2;return Q,b,mode(Q,b,2,blocks)

def compile_kernel(cache):
    source=Path(__file__).with_name('simplex_gibbs.cpp');digest=hashlib.sha256(source.read_bytes()).hexdigest();directory=Path(cache)/('simplex-'+digest[:12]);directory.mkdir(parents=True,exist_ok=True);exe=directory/'simplex_gibbs'
    if not exe.exists():
        result=subprocess.run(['g++','-std=c++20','-O3',str(source),'-o',str(exe)],capture_output=True,text=True)
        if result.returncode:raise RuntimeError(result.stderr)
    return exe,digest

def sample(Q,b,point,k,cache,draws=2000,burn=1000,thin=2,seed=20261027):
    blocks=len(point)//k;exe,digest=compile_kernel(cache);header=f'{k} {blocks} 4 {burn} {draws} {thin} {seed}\n'
    body=' '.join(format(float(v),'.17g') for a in [Q.ravel(),b,point] for v in a)+'\n';tic=time.monotonic()
    result=subprocess.run([str(exe)],input=(header+body).encode(),capture_output=True)
    if result.returncode:raise RuntimeError(result.stderr.decode())
    values=np.frombuffer(result.stdout,dtype=np.float64).copy().reshape(4,draws,len(point));elapsed=time.monotonic()-tic
    if np.any(values< -1e-12) or np.max(np.abs(values.reshape(4,draws,blocks,k).sum(axis=3)-1))>1e-9:raise AssertionError('Simplex sampler conservation')
    diagnostic=diagnose(values)
    return values,{'kernelHash':digest,'seconds':elapsed,'drawsPerChain':draws,'burnSweeps':burn,'thinSweeps':thin,**diagnostic}

def diagnose(samples):
    from scipy.fft import rfft,irfft,next_fast_len
    a=np.asarray(samples);chains,n,d=a.shape;n2=n//2;a=np.concatenate([a[:,:n2],a[:,-n2:]],axis=0)
    def rhat(x):
        W=np.var(x,axis=1,ddof=1).mean(axis=0);B=n2*np.var(x.mean(axis=1),axis=0,ddof=1);return np.sqrt(((n2-1)/n2*W+B/n2)/W)
    def ranks(x):
        shape=x.shape;r=rankdata(x.reshape(-1,d),axis=0);return ndtri((r-.375)/(len(r)+.25)).reshape(shape)
    z=ranks(a);rh=np.maximum(rhat(z),rhat(ranks(np.abs(a-np.median(a,axis=(0,1))))))
    bulk=[];tail=[]
    def ess(x):
        W=np.var(x,axis=1,ddof=1).mean();B=n2*np.var(x.mean(axis=1),ddof=1);V=(n2-1)/n2*W+B/n2
        if V<=1e-30:return float(x.size)
        centred=x-x.mean(axis=1,keepdims=True);F=rfft(centred,n=next_fast_len(2*n2),axis=1);ac=irfft(F*np.conj(F),n=next_fast_len(2*n2),axis=1)[:,:n2].mean(axis=0)/n2
        rho=1-(W-ac)/V;rho[0]=1;pairs=[]
        for t in range(0,n2-1,2):
            pair=rho[t]+rho[t+1]
            if pair<0:break
            pairs.append(min(pair,pairs[-1] if pairs else pair))
        return float(min(x.size,x.size/max(1.,-1+2*sum(pairs))))
    for j in range(d):
        bulk.append(ess(z[:,:,j]));lo,hi=np.quantile(a[:,:,j],[.05,.95]);tail.append(min(ess((a[:,:,j]<=lo).astype(float)),ess((a[:,:,j]>=hi).astype(float))))
    return {'maxRhat':float(np.max(rh)),'minEss':float(min(bulk)),'minTailEss':float(min(tail)),
      'passed':bool(np.all(np.isfinite(rh)) and np.max(rh)<=1.05 and min(bulk)>=400 and min(tail)>=200),
      'rhatByCoordinate':rh.tolist(),'essByCoordinate':bulk,'tailEssByCoordinate':tail}

def evolve(reference,sigma,horizon,rng):
    """Permutation-symmetric Beta-stick martingale with quarterly time steps.

    Conditional means stay unchanged; minority shapes do not collapse merely
    because their shares are small. This replaces logit-Brownian Jensen drift.
    """
    values=np.array(reference,copy=True);steps=max(1,math.ceil(horizon/.25));dt=horizon/steps
    if not np.isfinite(horizon) or horizon<0 or np.any(np.asarray(sigma)<0) or not np.all(np.isfinite(sigma)):raise ValueError('Invalid forecast horizon or volatility')
    if values.ndim!=2 or values.shape[1]<1 or np.any(values<0) or np.max(np.abs(values.sum(axis=1)-1))>1e-8:raise ValueError('Invalid reference simplex')
    if dt==0:return values
    if not np.any(sigma):return values
    S,K=values.shape;rows=np.arange(S)
    for _ in range(steps):
        order=np.argsort(rng.random((S,K)),axis=1);remaining=np.ones(S);new=np.zeros_like(values);old_remaining=np.ones(S)
        for h in range(K-1):
            j=order[:,h];old=values[rows,j]
            exact_mean=np.divide(old,old_remaining,out=np.zeros(S),where=old_remaining>1e-15)
            mean=np.clip(exact_mean,1e-12,1-1e-12)
            current_sigma=sigma[rows,j] if np.ndim(sigma)==2 else sigma
            variance=np.square(current_sigma)*dt*mean*(1-mean)
            concentration=np.maximum(2.,1/np.maximum(variance,1e-15)-1)
            beta=rng.beta(mean*concentration,(1-mean)*concentration)
            beta=np.where((exact_mean<=0)|(exact_mean>=1)|(np.asarray(current_sigma)==0),exact_mean,beta)
            new[rows,j]=remaining*beta;remaining-=new[rows,j];old_remaining-=old
        new[rows,order[:,-1]]=remaining;values=new
    return values

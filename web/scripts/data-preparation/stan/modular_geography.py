"""Empirical-Bayes predictive geography for the modular research alternative.

Profiles are estimated features, not a second independent likelihood or inferred
component counts. A joint source group updates its pooled profile only. An
individual profile uses separate historical observations when available, with
regional/national shrinkage and explicit fallback uncertainty.
"""
from __future__ import annotations
import math
import numpy as np

def softmax(x,axis=-1):
    a=np.asarray(x);e=np.exp(a-a.max(axis=axis,keepdims=True));return e/e.sum(axis=axis,keepdims=True)

def rake(probabilities,weights,target,tolerance=1e-9,max_iterations=120):
    """Probability projection before count generation, with structural zeros."""
    p=np.array(probabilities,dtype=float,copy=True);w=np.asarray(weights,dtype=float)
    q=np.asarray(target,dtype=float)
    if p.ndim!=2 or p.shape[0]!=len(w) or p.shape[1]!=len(q) or np.any(w<=0) or np.any(q<0) or abs(q.sum()-1)>1e-8:raise ValueError('Invalid geographic projection')
    for iteration in range(max_iterations):
        current=w@p/w.sum()
        if np.max(np.abs(current-q))<=tolerance:return p,iteration
        if np.any((current==0)&(q>0)):raise ValueError('Requested national support is unavailable in this geography')
        p*=np.divide(q,current,out=np.zeros_like(q),where=current>0)
        total=p.sum(axis=1,keepdims=True)
        if np.any(total<=0):raise ValueError('Empty municipal choice set')
        p/=total
    raise RuntimeError('Geographic probability projection did not converge')

def fit_profiles(source):
    ids=source['components'];index={s:i+1 for i,s in enumerate(ids)};J=len(ids)+1
    cities=source['cities'];ci={s:i for i,s in enumerate(cities)};C=len(cities)
    by_id={e['id']:e for e in source['nationalHistory']}
    dates={e['id']:e['date'] for e in source['elections']}
    # One unsplit group profile per actual source outcome. No likelihood is
    # evaluated against an inferred count inside that group.
    evidence=[[[] for _ in range(J)] for _ in cities];paired={};regions={ci[r['id']]:r['region'] for r in source['truth']};electors={};pooled_fallback={}
    for event in source['elections']:
        national=by_id[event['id']];E=national['electors'];V=sum(national['counts'][1:]);p0=national['counts'][0]/E
        national_groups={tuple(sorted(g)):n/V for g,n in zip(national['groups'],national['counts'][1:])}
        for row in event['rows']:
            c=ci[row['id']];regions[c]=row['region'];electors[c]=row['electors'];valid=row['electors']-row['nonValid']
            values={}
            # Participation profile on odds, political profile on conditional
            # valid-share ratios. Pseudocounts stabilize FEATURES only.
            local0=(row['nonValid']+.5)/(row['electors']+1)
            g0=math.log(local0/(1-local0))-math.log(max(1e-10,p0)/max(1e-10,1-p0))
            evidence[c][0].append((event['date'],g0,row['electors'],event['kind'],1/(row['nonValid']+.5)+1/(valid+.5)))
            values[0]=(g0,1/(row['nonValid']+.5)+1/(valid+.5))
            for members,count in zip(row['groups'],row['counts']):
                key=tuple(sorted(members));baseline=national_groups.get(key)
                if baseline is None:continue # National coarsening crosses this source group.
                local=(count+.5)/(valid+.5*max(1,len(row['groups'])))
                feature=math.log(local/max(1e-10,baseline))
                if len(members)==1:
                    j=index[members[0]];evidence[c][j].append((event['date'],feature,valid,event['kind'],1/(count+.5)))
                    values[j]=(feature,1/(count+.5))
                else:
                    for member in members:pooled_fallback[c,index[member]]=(event['date'],feature,valid,event['kind'],1/(count+.5))
            paired[c,event['id']]=values
    target_kind=source['targetKind'];cutoff=source['cutoff'];offset=np.zeros((C,J));fallback=np.zeros((C,J),bool)
    # Estimate regional means from the same observed feature family, then shrink
    # small municipalities toward their region. Kind matching has twice weight.
    raw=np.full((C,J),np.nan);information=np.zeros((C,J))
    for c in range(C):
        for j in range(J):
            entries=evidence[c][j]
            if not entries:
                entry=pooled_fallback.get((c,j));entries=[entry] if entry else [];fallback[c,j]=True
            if entries:
                weights=np.array([2**(-max(0,(np.datetime64(cutoff)-np.datetime64(x[0])).astype(int))/365.25/5)*(2 if x[3]==target_kind else 1) for x in entries])
                raw[c,j]=np.average([x[1] for x in entries],weights=weights)
                information[c,j]=sum(x[2] for x in entries)/len(entries)
    for region in sorted(set(regions.values())):
        group=[c for c in range(C) if regions.get(c)==region]
        for j in range(J):
            known=[c for c in group if np.isfinite(raw[c,j])]
            centre=np.average(raw[known,j],weights=np.sqrt(information[known,j]+1)) if known else 0
            for c in group:
                if np.isfinite(raw[c,j]):
                    weight=information[c,j]/(information[c,j]+500)
                    offset[c,j]=weight*raw[c,j]+(1-weight)*centre
                else:offset[c,j]=centre
    # Within-place changes after removing national movement already in features.
    # Count noise is subtracted; transitions remain the information units.
    rates=[[] for _ in range(J)];all_rates=[];transition_audit=[]
    for before,after in zip(source['elections'][:-1],source['elections'][1:]):
        dt=(np.datetime64(after['date'])-np.datetime64(before['date'])).astype(int)/365.25
        if dt<=0:continue
        for j in range(J):
            change=[];noise=[]
            for c in range(C):
                a=paired.get((c,before['id']),{}).get(j);b=paired.get((c,after['id']),{}).get(j)
                if a and b:change.append(b[0]-a[0]);noise.append(a[1]+b[1])
            if len(change)>=3:
                rate=max(0,(float(np.var(change,ddof=1))-float(np.mean(noise)))/dt)
                rates[j].append(rate);all_rates.append(rate);transition_audit.append({'before':before['id'],'after':after['id'],'component':j,'municipalities':len(change),'variancePerYear':rate})
    common=np.median(all_rates) if all_rates else .25**2
    common=float(np.clip(common,.03**2,.8**2))
    scales=np.array([math.sqrt((4*common+sum(xs))/(4+len(xs))) for xs in rates])
    scales=np.clip(scales,.03,.8)
    # Reused history without separately observed components gets deliberately
    # larger uncertainty. These are prediction assumptions, not fitted splits.
    uncertainty=np.zeros((C,J))
    for c in range(C):
        for j in range(J):
            entries=evidence[c][j]
            if not entries:
                entry=pooled_fallback.get((c,j));entries=[entry] if entry else []
            mean=offset[c,j];variance=1.;previous=None
            for entry in entries:
                date,value,_,kind,noise=entry
                if previous is not None:variance+=scales[j]**2*float((np.datetime64(date)-np.datetime64(previous)).astype(int))/365.25
                measurement=noise+(0 if kind==target_kind else .15**2)
                gain=variance/(variance+measurement);mean+=gain*(value-mean);variance*=1-gain;previous=date
            if previous is not None:variance+=scales[j]**2*max(0,float((np.datetime64(cutoff)-np.datetime64(previous)).astype(int)))/365.25
            offset[c,j]=mean;uncertainty[c,j]=math.sqrt(variance+(.45 if fallback[c,j] else .1)**2)
    lookup={row['id']:row for row in source['truth']}
    missing=[s for s in cities if s not in lookup]
    active=[c for c,s in enumerate(cities) if s in lookup]
    return {'offset':offset[active],'fallback':fallback[active],'profileSd':uncertainty[active],
            'scale':scales,'cities':[cities[c] for c in active],
            'regions':[lookup[cities[c]]['region'] for c in active],
            'electors':np.array([lookup[cities[c]]['electors'] for c in active]),
            'ballots':[[[index[j] for j in g] for g in lookup[cities[c]]['groups']] for c in active],
            'truth':[lookup[cities[c]] for c in active],
            'audit':{'method':'empirical feature profiles; separate observations preferred, pooled group fallback with added predictive uncertainty',
              'transitionCount':len(transition_audit),'transitions':transition_audit,'fallbackCells':int(fallback[active].sum()),
              'missingFutureRows':missing,'commonLocalVariancePerYear':common,'localScales':scales.tolist(),
      'fixedConventions':{'initialRegionalProfileHalfLifeYears':5,'regionalShrinkElectors':500,'regionalVarianceFraction':.15,'profileKnownSd':.10,'profileFallbackSd':.45,'otherKindProfileSd':.15,'localLogScaleSd':.2,'historyFilter':'Scalar Gaussian feature filter with count-noise and missing-history variance propagated to cutoff'}}}

def sample_counts(p,electors,rng):
    n=np.asarray(electors,dtype=np.int64);remaining=n.copy();mass=np.ones(len(n));out=np.zeros_like(p,dtype=np.int64)
    for j in range(p.shape[1]-1):
        prob=np.clip(np.divide(p[:,j],mass,out=np.zeros_like(mass),where=mass>1e-14),0,1)
        out[:,j]=rng.binomial(remaining,prob);remaining-=out[:,j];mass-=p[:,j]
    out[:,-1]=remaining
    return out

def generate(national,profile,horizon_years,seed=20261027,regional=True):
    rng=np.random.default_rng(seed);S,J=national.shape;C=len(profile['cities']);regions=sorted(set(profile['regions']));ri=np.array([regions.index(x) for x in profile['regions']])
    available=np.zeros((C,J),bool);available[:,0]=True
    for c,groups in enumerate(profile['ballots']):
        for g in groups:available[c,g]=True
    target=np.array(national,copy=True)
    # Choice-set substitution retains the previously approved rule: removing
    # support renormalizes every remaining outcome, including non-valid voting.
    offered=available.any(axis=0);target[:,~offered]=0
    target/=target.sum(axis=1,keepdims=True)
    mean=np.zeros((C,J));draws=np.empty((S,C,J),np.int32);iterations=[];max_residual=0
    for s in range(S):
        scale=profile['scale']*np.exp(rng.normal(0,.2))
        fraction=.15 if regional else 0
        innovation=rng.normal(size=(C,J))*math.sqrt(1-fraction)
        if regional:innovation+=rng.normal(size=(len(regions),J))[ri]*math.sqrt(fraction)
        scores=profile['offset']+rng.normal(size=(C,J))*profile['profileSd']+innovation*scale*math.sqrt(horizon_years)
        scores+=np.log(np.maximum(target[s],1e-300));scores[~available]=-np.inf
        p=softmax(scores)
        p,steps=rake(p,profile['electors'],target[s]);iterations.append(steps)
        max_residual=max(max_residual,float(np.max(np.abs(profile['electors']@p/profile['electors'].sum()-target[s]))))
        mean+=p/S;draws[s]=sample_counts(p,profile['electors'],rng)
    if np.any(draws.sum(axis=2)!=profile['electors']):raise AssertionError('Elector count conservation')
    return draws,{'maximumNationalProbabilityResidual':max_residual,'maximumProjectionIterations':max(iterations),
      'zeroValidDraws':int(np.sum(draws[:,:,1:].sum(axis=2)==0)),
      'municipalConcentration95':int(np.sum((draws[:,:,1:].max(axis=2)>.95*draws[:,:,1:].sum(axis=2))&(draws[:,:,1:].sum(axis=2)>=500))),
      'nonValid90':float(np.mean(draws[:,:,0]/profile['electors']>.9)),
      'expectedProbabilityMeans':mean.tolist()}

def scalar_score(x,truth):
    q=np.quantile(x,[.025,.05,.1,.25,.5,.75,.9,.95,.975]);wis=.5*abs(q[4]-truth);interval90=0
    for lo,hi,level in [(q[3],q[5],.5),(q[2],q[6],.8),(q[1],q[7],.9),(q[0],q[8],.95)]:
        alpha=1-level;interval=hi-lo+2/alpha*(max(0,lo-truth)+max(0,truth-hi));wis+=alpha/2*interval
        if level==.9:interval90=interval
    mean=float(np.mean(x));return {'truth':truth,'mean':mean,'lower90':float(q[1]),'upper90':float(q[7]),'coverage90':float(q[1]<=truth<=q[7]),
       'width90':float(q[7]-q[1]),'meanMae':abs(mean-truth),'meanBias':mean-truth,'wis':float(wis/4.5),'intervalScore90':float(interval90)}

def aggregate_scores(rows):
    keys=['coverage90','width90','meanMae','wis','intervalScore90']
    return {**{key:float(np.mean([r[key] for r in rows])) for key in keys},'variables':len(rows)}

def national_score(national,source):
    truth=source['nationalTruth'];index={s:i+1 for i,s in enumerate(source['components'])};E=truth['electors'];V=sum(truth['counts'][1:]);rows=[];political=[]
    rows.append({'members':['non-valid'],**scalar_score(national[:,0],truth['counts'][0]/E)})
    for members,n in zip(truth['groups'],truth['counts'][1:]):
        js=[index[s] for s in members];x=national[:,js].sum(axis=1)
        rows.append({'members':members,**scalar_score(x,n/E)});political.append({'members':members,**scalar_score(x/(1-national[:,0]),n/V)})
    return {'electorShares':aggregate_scores(rows),'validShares':aggregate_scores(political),'rows':rows,'politicalRows':political,
      'scope':'National latent predictive probabilities; geographic-sample count totals are not substituted for country totals'}

def score(draws,profile):
    fractions=draws/profile['electors'][None,:,None];truth=np.zeros(fractions.shape[1:]);masks=[]
    for c,row in enumerate(profile['truth']):
        groups=[[0]]+profile['ballots'][c];counts=[row['nonValid']]+row['counts']
        masks.extend((c,g,n/row['electors']) for g,n in zip(groups,counts))
    values=[];labels=[];valid_values=[]
    for c,g,t in masks:
        x=fractions[:,c,g].sum(axis=1);r=scalar_score(x,t);values.append(r);labels.append({'city':profile['cities'][c],'members':g,**r})
        if g!=[0]:
            valid=draws[:,c,1:].sum(axis=1);truth_valid=1-profile['truth'][c]['nonValid']/profile['truth'][c]['electors']
            y=np.divide(draws[:,c,g].sum(axis=1),valid,out=np.zeros(len(valid)),where=valid>0);valid_values.append(scalar_score(y,t/truth_valid))
    return {'municipalElectorShares':{**aggregate_scores(values),'note':'Observed grouped source outcomes only; no internal pooled-component truth'},
      'municipalValidShares':aggregate_scores(valid_values),'rows':labels}

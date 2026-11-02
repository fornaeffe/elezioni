"""Research scenario composition, kept separate from production/editor code.

Historical IDs are indivisible fitted units. Joint lists sum those units; a
new unit requires an explicit reference share and inherits no historical votes.
References are interventions on latent reference support, not a new posterior
conditioned on fabricated observations. Forecast/election uncertainty stays separate.
"""
from __future__ import annotations
import numpy as np
from scipy.special import expit,logit
from scipy.optimize import least_squares
from numpy.polynomial.hermite import hermgauss

def soft_reference(mean,sd_pp,draws,rng,upper=1.):
    if not 0<mean<upper or not 0<sd_pp/100<np.sqrt(mean*(upper-mean)):
        raise ValueError('Infeasible reference mean or percentage-point standard deviation')
    target=mean/upper;sd=sd_pp/100/upper;nodes,weights=hermgauss(96);weights=weights/np.sqrt(np.pi)
    def moments(x):
        p=expit(x[0]+np.exp(x[1])*np.sqrt(2)*nodes);m=weights@p;return m,weights@np.square(p)-m*m
    def residual(x):
        m,v=moments(x);return np.array([m-target,(v-sd*sd)/max(.01,sd)])
    fit=least_squares(residual,[logit(target),np.log(sd/(target*(1-target)))],bounds=([-50,-16],[50,6]),xtol=1e-12,ftol=1e-12,gtol=1e-12)
    m,v=moments(fit.x)
    if abs(m-target)>1e-7 or abs(np.sqrt(v)-sd)>1e-7:raise ValueError('Reference uncertainty is too broad for the calibrated logistic-normal family')
    return upper*expit(rng.normal(fit.x[0],np.exp(fit.x[1]),draws))

def reference_intervention(country,known_ids,lists,new_components=(),overrides=(),seed=20261027,reference_date=None,default_sigma=.35):
    country=np.asarray(country,float);S,J=country.shape
    if J!=len(known_ids)+1 or np.any(country<0) or np.max(np.abs(country.sum(axis=1)-1))>1e-8:raise ValueError('Invalid reference country distribution')
    known=set(known_ids);new={x['id']:x for x in new_components}
    if len(new)!=len(new_components) or known&set(new):raise ValueError('Duplicate new/historical identity')
    offered=[];list_map={};targets=[]
    for item in lists:
        if item['id'] in list_map or not item['members'] or len(set(item['members']))!=len(item['members']):raise ValueError('Invalid list identity or membership')
        for member in item['members']:
            if member not in known and member not in new:raise ValueError('Unknown or unsupported historical split; define a new unit with a reference share')
            if member in offered:raise ValueError('A component cannot appear in two lists')
            offered.append(member)
        list_map[item['id']]=item['members']
    active=[x for x in known_ids if x in offered]+[x for x in new if x in offered]
    if not active:raise ValueError('Empty future political ballot')
    positions={x:i+1 for i,x in enumerate(active)};weights=np.zeros((S,len(active)+1));weights[:,0]=country[:,0]
    for member in active:
        if member in known:weights[:,positions[member]]=country[:,known_ids.index(member)+1]
        else:
            spec=new[member]
            if 'referenceShare' not in spec:raise ValueError('New component requires a reference share')
            targets.append({'members':[member],**spec})
    for spec in overrides:
        if spec['listId'] not in list_map:raise ValueError('Unknown override list')
        targets.append({'members':list_map[spec['listId']],**spec})
    fixed=[];uncertain=[];used=set()
    for spec in targets:
        if reference_date and spec.get('referenceDate',reference_date)!=reference_date:raise ValueError('Reference date differs from the prepared fit; reprepare at that date')
        members=spec['members']
        if set(members)&used:raise ValueError('Overlapping reference constraints need an explicit joint reference contract')
        used.update(members);mean=spec['referenceShare'];sd=spec.get('sdPercentagePoints',0)
        if not 0<=mean<=1 or sd<0:raise ValueError('Invalid reference share/uncertainty')
        (uncertain if sd else fixed).append(spec)
    if len(uncertain)>1:raise ValueError('This research contract supports one uncertain share plus disjoint exact shares')
    exact_total=sum(x['referenceShare'] for x in fixed)
    if exact_total>1+1e-12:raise ValueError('Reference shares exceed 100 percent')
    rng=np.random.default_rng(seed);values={id(x):np.full(S,x['referenceShare']) for x in fixed}
    for spec in uncertain:values[id(spec)]=soft_reference(spec['referenceShare'],spec['sdPercentagePoints'],S,rng,upper=1-exact_total)
    if targets:
        free=[positions[x] for x in active if x not in used];total=sum(values[id(x)] for x in targets)
        free_mass=weights[:,free].sum(axis=1) if free else np.zeros(S)
        if free and (np.any(total>=1) or np.any(free_mass<=0)):raise ValueError('References leave no support for the remaining ballot')
        if not free and np.max(np.abs(total-1))>1e-8:raise ValueError('A fully specified ballot must have reference shares summing to 100 percent')
        political_mass=country[:,1:].sum(axis=1) if not free else free_mass/(1-total)
        for spec in targets:
            js=[positions[x] for x in spec['members']];old=weights[:,js].sum(axis=1)
            proportions=np.divide(weights[:,js],old[:,None],out=np.full((S,len(js)),1/len(js)),where=old[:,None]>0)
            weights[:,js]=proportions*(political_mass*values[id(spec)])[:,None]
    weights/=weights.sum(axis=1,keepdims=True)
    sigma=np.array([new[x].get('nationalInnovationLogitSd',default_sigma) if x in new else default_sigma for x in active])
    local_multiplier=np.ones(len(active))
    for item in lists:
        multiplier=item.get('nationalUncertaintyMultiplier',1);local=item.get('localUncertaintyMultiplier',1)
        if not np.isfinite(multiplier) or not np.isfinite(local) or multiplier<0 or local<0:raise ValueError('Invalid list uncertainty multiplier')
        for member in item['members']:sigma[positions[member]-1]*=multiplier;local_multiplier[positions[member]-1]=local
    if np.any(sigma<0) or not np.all(np.isfinite(sigma)):raise ValueError('Invalid innovation scale')
    return {'country':weights,'componentIds':active,'lists':[{'id':item['id'],'members':[positions[x] for x in item['members']]} for item in lists],
      'nationalInnovationLogitSd':sigma,'localUncertaintyMultiplier':local_multiplier,
      'newDefaults':{x:{'geographicLogProfile':new[x].get('geographicLogProfile',0),'profileSd':new[x].get('profileSd',.45),'localInnovationLogitSd':new[x].get('localInnovationLogitSd',.25)} for x in active if x in new}}

def aggregate_lists(component_counts,compiled):
    x=np.asarray(component_counts)
    if x.shape[-1]!=len(compiled['componentIds'])+1 or np.any(x<0) or not np.issubdtype(x.dtype,np.integer):raise ValueError('Expected nonnegative integer component counts')
    votes=np.stack([x[...,row['members']].sum(axis=-1) for row in compiled['lists']],axis=-1)
    if np.any(votes.sum(axis=-1)+x[...,0]!=x.sum(axis=-1)):raise AssertionError('List/elector conservation')
    return {'votes':votes,'nonValid':x[...,0],'listIds':[row['id'] for row in compiled['lists']]}

def generate_scenario(compiled,base_profile,known_ids,horizon,seed=20261027,turnout_sigma=.15,election_shock=0):
    """Headless browser-contract prototype; declared lists are nationally offered.
    Filing/geographic availability must be supplied separately before runtime use.
    """
    from fast_party_model import evolve
    from modular_geography import generate
    rng=np.random.default_rng(seed);country=compiled['country'];S=len(country);active=compiled['componentIds'];C=len(base_profile['cities']);J=len(active)+1
    q=country[:,1:]/country[:,1:].sum(axis=1,keepdims=True);v=np.c_[country[:,0],1-country[:,0]]
    q=evolve(q,np.tile(compiled['nationalInnovationLogitSd'],(S,1)),horizon,rng);v=evolve(v,np.full(S,turnout_sigma),horizon,rng)
    if election_shock:q=evolve(q,np.full(S,election_shock),1,rng)
    national=np.c_[v[:,0],v[:,1,None]*q];profile=dict(base_profile)
    offset=np.zeros((C,J));sd=np.zeros((C,J));scales=np.zeros(J);offset[:,0]=base_profile['offset'][:,0];sd[:,0]=base_profile['profileSd'][:,0];scales[0]=base_profile['scale'][0]
    for j,member in enumerate(active,1):
        if member in known_ids:
            old=known_ids.index(member)+1;offset[:,j]=base_profile['offset'][:,old];sd[:,j]=base_profile['profileSd'][:,old];scales[j]=base_profile['scale'][old]
        else:
            defaults=compiled['newDefaults'][member];offset[:,j]=defaults['geographicLogProfile'];sd[:,j]=defaults['profileSd'];scales[j]=defaults['localInnovationLogitSd']
        scales[j]*=compiled['localUncertaintyMultiplier'][j-1]
    profile.update(offset=offset,profileSd=sd,scale=scales,ballots=[[x['members'] for x in compiled['lists']] for _ in range(C)])
    counts,audit=generate(national,profile,horizon,seed=seed)
    return {**aggregate_lists(counts,compiled),'componentCounts':counts,'audit':audit}

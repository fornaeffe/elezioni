from __future__ import annotations
import copy,unittest
import numpy as np
from fast_scenario import reference_intervention,aggregate_lists,soft_reference,generate_scenario
from fast_party_model import political_precision,mode
from modular_geography import fit_profiles,rake

def fixture():
    rows=[{'id':f'c{i}','region':'r1' if i<2 else 'r2','electors':1000,'nonValid':n,'groups':[['a'],['b']],'counts':[a,1000-n-a]} for i,(n,a) in enumerate([(300,500),(400,300),(200,200)])]
    e={'id':'camera-2010','date':'2010-01-01','kind':'camera','rows':rows}
    e2=copy.deepcopy(e);e2.update(id='camera-2014',date='2014-01-01')
    for r in e2['rows']:r['nonValid']+=20;r['counts'][1]-=20
    def national(x):return{'id':x['id'],'date':x['date'],'kind':x['kind'],'electors':3000,'groups':[['a'],['b']],
      'counts':[sum(r['nonValid'] for r in x['rows']),sum(r['counts'][0] for r in x['rows']),sum(r['counts'][1] for r in x['rows'])]}
    return{'components':['a','b'],'cities':[r['id'] for r in rows],'cutoff':'2017-01-01','targetDate':'2018-01-01','targetKind':'camera',
      'elections':[e,e2],'nationalHistory':[national(e),national(e2)],'truth':copy.deepcopy(rows),'polls':[]}

class FastModel(unittest.TestCase):
    def test_active_set_qp_matches_independent_optimizer(self):
        from scipy.optimize import minimize
        rng=np.random.default_rng(20261027)
        for _ in range(8):
            M=rng.normal(size=(6,6));Q=M.T@M+np.eye(6);b=rng.normal(size=6)*3
            actual=mode(Q,b,3,2)
            reference=minimize(lambda x:.5*x@Q@x-b@x,np.ones(6)/3,jac=lambda x:Q@x-b,method='SLSQP',bounds=[(0,1)]*6,
                constraints=[{'type':'eq','fun':lambda x:sum(x[:3])-1},{'type':'eq','fun':lambda x:sum(x[3:])-1}],options={'ftol':1e-13,'maxiter':400})
            self.assertTrue(reference.success);np.testing.assert_allclose(actual,reference.x,atol=2e-7)
    def test_new_reference_and_joint_conservation(self):
        reference=np.array([[.3,.2,.5],[.4,.1,.5]])
        c=reference_intervention(reference,['a','b'],[{'id':'joint','members':['a','b']},{'id':'new-list','members':['n']}],
          [{'id':'n','referenceShare':.2}],reference_date='2026-01-01')
        np.testing.assert_allclose(c['country'][:,-1]/(1-c['country'][:,0]),.2,atol=1e-14)
        self.assertEqual(c['newDefaults']['n']['geographicLogProfile'],0)
        counts=np.array([[[30,10,40,20],[25,25,25,25]]],dtype=np.int64);out=aggregate_lists(counts,c)
        np.testing.assert_array_equal(out['votes'][...,0],counts[...,1]+counts[...,2]);self.assertTrue(np.all(out['votes'].sum(axis=-1)+out['nonValid']==100))
    def test_choice_set_rule_and_existing_override(self):
        p=np.array([[.3,.2,.5]])
        c=reference_intervention(p,['a','b'],[{'id':'b','members':['b']}]);np.testing.assert_allclose(c['country'],[[.375,.625]])
        c=reference_intervention(p,['a','b'],[{'id':'ab','members':['a','b']}],overrides=[{'listId':'ab','referenceShare':1}])
        np.testing.assert_allclose(c['country'],p)
    def test_unknown_splits_and_missing_reference_reject(self):
        p=np.array([[.3,.7]])
        with self.assertRaisesRegex(ValueError,'unsupported historical split'):reference_intervention(p,['atomic-joint'],[{'id':'split','members':['hidden-child']}])
        with self.assertRaisesRegex(ValueError,'reference share'):reference_intervention(p,['atomic-joint'],[{'id':'new','members':['n']}],[{'id':'n'}])
    def test_multiple_new_exact_shares(self):
        c=reference_intervention(np.array([[.3,.7]]),['a'],[{'id':'a','members':['a']},{'id':'n','members':['n']},{'id':'m','members':['m']}],
          [{'id':'n','referenceShare':.2},{'id':'m','referenceShare':.1}])
        np.testing.assert_allclose(c['country'][:,2:]/(1-c['country'][:,0,None]),[[.2,.1]])
    def test_soft_reference_units_and_moments(self):
        x=soft_reference(.2,5,150000,np.random.default_rng(20261027));self.assertLess(abs(x.mean()-.2),.001);self.assertLess(abs(x.std()-.05),.001)
    def test_heldout_counts_do_not_enter_fit_or_profiles(self):
        a=fixture();b=copy.deepcopy(a)
        for row in b['truth']:row['counts']=[999,0];row['nonValid']=1
        qa,ba,pa,_=political_precision(a,[]);qb,bb,pb,_=political_precision(b,[])
        np.testing.assert_array_equal(qa,qb);np.testing.assert_array_equal(pa,pb)
        ga=fit_profiles(a);gb=fit_profiles(b);np.testing.assert_array_equal(ga['offset'],gb['offset']);np.testing.assert_array_equal(ga['scale'],gb['scale'])
    def test_missing_history_retains_uncertainty(self):
        source=fixture();source['elections'][1]['rows']=[r for r in source['elections'][1]['rows'] if r['id']!='c0']
        a=fit_profiles(source);later=copy.deepcopy(source);later['cutoff']='2019-01-01';b=fit_profiles(later)
        self.assertTrue(np.all(b['profileSd']>=a['profileSd']))
    def test_impossible_geographic_margin_rejects(self):
        with self.assertRaises(ValueError):rake([[1,0],[1,0]],[100,100],[.5,.5])
    def test_joint_grouping_does_not_change_component_draws(self):
        profile=fit_profiles(fixture());p=np.tile([.3,.4,.3],(64,1))
        separate=reference_intervention(p,['a','b'],[{'id':'a','members':['a']},{'id':'b','members':['b']}])
        joint=reference_intervention(p,['a','b'],[{'id':'ab','members':['a','b']}])
        a=generate_scenario(separate,profile,['a','b'],.1);b=generate_scenario(joint,profile,['a','b'],.1)
        np.testing.assert_array_equal(a['componentCounts'],b['componentCounts']);np.testing.assert_array_equal(a['votes'].sum(axis=-1),b['votes'][...,0])
    def test_new_list_generates_integer_votes_in_every_city(self):
        profile=fit_profiles(fixture());p=np.tile([.3,.4,.3],(256,1))
        c=reference_intervention(p,['a','b'],[{'id':'ab','members':['a','b']},{'id':'n','members':['n']}],[{'id':'n','referenceShare':.2}])
        out=generate_scenario(c,profile,['a','b'],0)
        self.assertEqual(out['votes'].shape,(256,3,2));self.assertTrue(np.all(out['votes'].sum(axis=-1)+out['nonValid']==1000))
        self.assertLess(abs(out['votes'][...,1].sum()/out['votes'].sum()-.2),.005)

if __name__=='__main__':unittest.main()

"""Chronological research comparison; development selection precedes confirmation."""
from __future__ import annotations
import argparse,hashlib,json,time,gzip
from pathlib import Path
import numpy as np
from run_fast import fit,forecast

def native_path(s):
    if len(s)>2 and s[1]==':' and s[0].isalpha():return Path('/mnt/'+s[0].lower()+'/'+s[3:].replace('\\','/'))
    return Path(s)

def main():
    p=argparse.ArgumentParser();p.add_argument('index',type=Path);p.add_argument('--cache',type=Path,required=True);p.add_argument('--draws',type=int,default=128)
    p.add_argument('--output',type=Path,required=True);p.add_argument('--selected',help='Frozen development candidate, required for confirmation');args=p.parse_args()
    index=json.loads(args.index.read_text());phase=index['phase'];outputs=[];start=time.monotonic()
    variants=[args.selected] if args.selected else ['kind-common-regional','kind-common-conservative','kind-component-regional','pooled-common-regional'] if phase=='development' else [None]
    if phase!='development' and not args.selected:raise RuntimeError('Confirmation requires frozen development selection')
    args.output.mkdir(parents=True,exist_ok=True)
    source_hashes={s.name:hashlib.sha256(s.read_bytes()).hexdigest() for s in [Path(__file__),Path(__file__).with_name('run_fast.py'),Path(__file__).with_name('fast_party_model.py'),Path(__file__).with_name('modular_geography.py'),Path(__file__).with_name('simplex_gibbs.cpp')]}
    for path in index['directories']:
        directory=native_path(path);manifest=json.loads((directory/'manifest.json').read_text());source=json.loads((directory/'profiles.json').read_text())
        if hashlib.sha256((directory/'profiles.json').read_bytes()).hexdigest()!=manifest['profileHash']:raise RuntimeError('Prepared profile input changed')
        for item in manifest['sourceVersions']:
            if hashlib.sha256((Path(__file__).parent.parent/item['path']).read_bytes()).hexdigest()!=item['sha256']:raise RuntimeError('Re-export after code change: '+item['path'])
        fits={}
        for variant in variants:
            kinds=not variant.startswith('pooled');components='component' in variant
            if kinds not in fits:
                a,b,fd=fit(source,args.cache,kind_effect=kinds);fits[kinds]=(a,b,fd)
                np.savez_compressed(args.output/(manifest['version']+('-kind' if kinds else '-pooled')+'-posterior.npz'),political=a,turnout=b)
            a,b,fd=fits[kinds]
            row={'target':manifest['target'],'horizon':manifest['horizon'],'variant':variant,'fit':fd,'accepted':fd['accepted'],'inputDirectory':str(directory)}
            if fd['accepted']:
                row['forecast']=forecast(source,a,b,fd['audit'],draws=args.draws,component_scales=components,shock_factor=1.5 if 'conservative' in variant else 1)
                details=row['forecast']['scores'].pop('rows');filename=manifest['version']+'-'+variant+'-municipal.json.gz'
                with gzip.open(args.output/filename,'wt',encoding='utf-8',compresslevel=6) as handle:json.dump(details,handle)
                row['forecast']['scoreRowsArtifact']=filename
                groups={}
                for detail in details:
                    key='+'.join('non-valid' if j==0 else source['components'][j-1] for j in detail['members'])
                    group=groups.setdefault(key,[]);group.append(detail['meanBias'])
                row['forecast']['groupSignedBias']={key:float(np.mean(values)) for key,values in groups.items()}
            outputs.append(row)
            (args.output/'cases.json').write_text(json.dumps({'phase':phase,'draws':args.draws,'sourceHashes':source_hashes,'cases':outputs},indent=2)+'\n')
            print(json.dumps({'target':row['target'],'horizon':row['horizon'],'variant':variant,'fitAccepted':fd['accepted'],'fitSeconds':fd['fitSeconds'],
              'nationalCoverage90':row.get('forecast',{}).get('nationalScores',{}).get('validShares',{}).get('coverage90'),
              'municipalCoverage90':row.get('forecast',{}).get('scores',{}).get('municipalValidShares',{}).get('coverage90')}),flush=True)
    summaries=[]
    for variant in variants:
        rows=[r for r in outputs if r['variant']==variant];valid=[r['forecast'] for r in rows if r['accepted']]
        summary={'variant':variant,'cases':len(rows),'failedFits':sum(not r['accepted'] for r in rows)}
        if valid:
            summary['nationalCoverage90']=float(np.mean([x['nationalScores']['validShares']['coverage90'] for x in valid]));summary['municipalCoverage90']=float(np.mean([x['scores']['municipalValidShares']['coverage90'] for x in valid]))
            summary['coverageShortfall']=float(np.mean([max(0,.85-x[key][sub]['coverage90']) for x in valid for key,sub in [('nationalScores','validShares'),('scores','municipalValidShares')]]))
            summary['wis']=float(np.mean([x[key][sub]['wis'] for x in valid for key,sub in [('nationalScores','validShares'),('scores','municipalValidShares')]]))
            summary['meanMae']=float(np.mean([x[key][sub]['meanMae'] for x in valid for key,sub in [('nationalScores','validShares'),('scores','municipalValidShares')]]))
            summary['tailRate']=float(max(max(x['nationalStress'].values()) for x in valid));summary['maxFitSeconds']=float(max(r['fit']['fitSeconds'] for r in rows));summary['maxGenerationSeconds']=float(max(x['generationSeconds'] for x in valid))
        summaries.append(summary)
    # Frozen practical hierarchy: numerical validity, extreme national tails,
    # coverage shortfall, proper interval score, then predictive-mean accuracy.
    ranked=sorted(summaries,key=lambda s:(s['failedFits'],s.get('tailRate',1)>.005,round(s.get('coverageShortfall',1)/.01),s.get('wis',1),s.get('meanMae',1)))
    result={'phase':phase,'draws':args.draws,'summaries':summaries,'selected':ranked[0]['variant'],'seconds':time.monotonic()-start,'sourceHashes':source_hashes,
      'interpretation':'Research comparison only; 128 draws are screening, small election counts limit calibration claims; no production acceptance'}
    (args.output/'summary.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(result),flush=True)

if __name__=='__main__':main()

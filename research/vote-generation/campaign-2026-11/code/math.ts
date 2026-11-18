import {sum} from './data';
export const logistic = (x:number) => x >= 0 ? 1/(1+Math.exp(-x)) : Math.exp(x)/(1+Math.exp(x));
export const logit = (p:number) => Math.log(Math.max(1e-9,p)/Math.max(1e-9,1-p));
export const normalize = (x:ArrayLike<number>) => {const t=sum(x); if (!(t>0)) throw new Error('EMPTY_SIMPLEX'); return Float64Array.from(x,v=>v/t);};
export const softmax = (x:ArrayLike<number>) => {const m=Math.max(...Array.from(x)); return normalize(Float64Array.from(x,v=>Math.exp(v-m)));};
export function ilr(p:ArrayLike<number>) {
  const q=normalize(Float64Array.from(p,v=>Math.max(.005,v))), out:number[]=[];
  for (let i=1;i<q.length;i++) out.push((sum(Array.from(q.slice(0,i),Math.log))-i*Math.log(q[i]))/Math.sqrt(i*(i+1)));
  return out;
}
export function inverseIlr(z:number[]) {
  const x=new Float64Array(z.length+1);
  z.forEach((v,j)=>{const i=j+1,t=v/Math.sqrt(i*(i+1));for(let k=0;k<i;k++)x[k]+=t;x[i]-=i*t;});
  return softmax(x);
}
export const matrix = (n:number) => Array.from({length:n},()=>Array(n).fill(0) as number[]);
export function outerAdd(a:number[][],v:number[],w:number) {for(let i=0;i<v.length;i++)for(let j=0;j<v.length;j++)a[i][j]+=v[i]*v[j]*w;}
export function factor(a:number[][],denom=1) {
  const l=matrix(a.length);
  for(let i=0;i<a.length;i++)for(let j=0;j<=i;j++) {let x=a[i][j]/denom;for(let k=0;k<j;k++)x-=l[i][k]*l[j][k];if(i===j&&!(x>0))throw new Error('NON_POSITIVE_COVARIANCE');l[i][j]=i===j?Math.sqrt(x):x/l[j][j];}
  return l;
}
export function quantile(x:number[],p:number) {const t=(x.length-1)*p,a=Math.floor(t);return x[a]+(x[Math.ceil(t)]-x[a])*(t-a);}
export function score(values:number[],truth:number) {
  if(!values.length||!Number.isFinite(truth)||values.some(v=>!Number.isFinite(v)))throw new Error('INVALID_SCORE');
  const x=[...values].sort((a,b)=>a-b),mean=sum(x)/x.length, median=quantile(x,.5),lo=quantile(x,.05),hi=quantile(x,.95);
  let wis=.5*Math.abs(median-truth);
  for(const level of [.5,.8,.9,.95]) {const alpha=1-level,l=quantile(x,alpha/2),u=quantile(x,1-alpha/2);wis+=alpha/2*(u-l+2/alpha*(Math.max(0,l-truth)+Math.max(0,truth-u)));}
  return {error30:sum(x.map(v=>Number(Math.abs(v-truth)>.3)))/x.length,Coverage90:Number(lo<=truth&&truth<=hi),
    MAE:Math.abs(mean-truth),WIS:wis/4.5,width90:hi-lo,mean,truth};
}
export function average(rows:ReturnType<typeof score>[]) {
  if(!rows.length)throw new Error('EMPTY_SCORE');
  return Object.fromEntries(['error30','Coverage90','MAE','WIS','width90'].map(k=>[k,sum(rows.map(r=>r[k as 'WIS']))/rows.length]));
}
/** Positive-support IPF; forecast weights, never target votes, set margins. */
export function rake(rows:Float64Array[],weights:number[],target:Float64Array) {
  const total=sum(weights),k=target.length;
  for(let it=0;it<500;it++) {
    const current=new Float64Array(k); rows.forEach((p,i)=>p.forEach((v,j)=>current[j]+=weights[i]*v/total));
    if(Math.max(...current.map((v,j)=>Math.abs(v-target[j])))<1e-8)return;
    const factors=current.map((v,j)=>v?target[j]/v:0);
    rows.forEach(p=>{let t=0;p.forEach((v,j)=>{p[j]=v*factors[j];t+=p[j];});if(!(t>0))throw new Error('EMPTY_IPF_ROW');p.forEach((v,j)=>p[j]=v/t);});
  }
  throw new Error('IPF_NOT_CONVERGED');
}

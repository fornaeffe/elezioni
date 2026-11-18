// Exact coordinate Gibbs for a Gaussian density restricted to a product of
// simplexes: exp(-.5*x'Q*x+b'x). Pair transfers preserve every simplex sum.
// No external numerical dependencies. Binary doubles follow a text header/input.
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <iostream>
#include <random>
#include <stdexcept>
#include <vector>

struct Random {
  std::mt19937_64 engine;
  std::normal_distribution<double> normal{0,1};
  explicit Random(uint64_t seed):engine(seed){}
  double uniform(){return (static_cast<double>(engine()>>11)+.5)/9007199254740992.;}
  double truncated(double a,double b){
    if(!(a<b))throw std::runtime_error("Empty Gaussian conditional");
    if(b<0)return -truncated(-b,-a);
    if(a>0){
      if((b-a)*std::max(1.,a)<1.5){
        for(;;){double z=a+(b-a)*uniform();if(std::log(uniform())<=-.5*(z-a)*(z+a))return z;}
      }
      double rate=.5*(a+std::hypot(a,2.));
      for(;;){double z=a-std::log(uniform())/rate;if(z<=b&&std::log(uniform())<=-.5*(z-rate)*(z-rate))return z;}
    }
    if(b-a<1.5){for(;;){double z=a+(b-a)*uniform();if(std::log(uniform())<=-.5*z*z)return z;}}
    for(;;){double z=normal(engine);if(z>=a&&z<=b)return z;}
  }
};

int main(){try{
  int k,blocks,chains,burn,draws,thin;uint64_t seed;
  if(!(std::cin>>k>>blocks>>chains>>burn>>draws>>thin>>seed)||k<2||blocks<1||chains<1||draws<1||thin<1)throw std::runtime_error("Invalid sampler header");
  const int d=k*blocks;std::vector<double> Q(d*d),b(d),start(d);
  for(double&v:Q)std::cin>>v;for(double&v:b)std::cin>>v;for(double&v:start)std::cin>>v;
  if(!std::cin)throw std::runtime_error("Incomplete precision input");
  for(int chain=0;chain<chains;chain++){
    Random rng(seed+0x9e3779b97f4a7c15ULL*chain);std::vector<double>x(start),gradient(d);
    for(int block=0;block<blocks;block++){
      double sum=0;std::vector<double>u(k);for(double&v:u){v=-std::log(rng.uniform());sum+=v;}
      double mix=.02+.15*chain;
      for(int i=0;i<k;i++)x[block*k+i]=(1-mix)*std::max(1e-14,start[block*k+i])+mix*u[i]/sum;
      sum=0;for(int i=0;i<k;i++)sum+=x[block*k+i];for(int i=0;i<k;i++)x[block*k+i]/=sum;
    }
    for(int sweep=0;sweep<burn+draws*thin;sweep++){
      for(int a=0;a<d;a++){long double g=-b[a];for(int z=0;z<d;z++)g+=static_cast<long double>(Q[a*d+z])*x[z];gradient[a]=g;}
      for(int update=0;update<k*(blocks+1);update++){
        int i=rng.engine()%k,j=(i+1+rng.engine()%(k-1))%k;
        bool shared=blocks>1&&update%3==0;int block=rng.engine()%blocks;
        std::vector<int>indices;std::vector<double>signs;
        for(int h=0;h<blocks;h++)if(shared||h==block){indices.push_back(h*k+i);signs.push_back(1);indices.push_back(h*k+j);signs.push_back(-1);}
        double lower=-1e300,upper=1e300,linear=0,precision=0;
        for(size_t a=0;a<indices.size();a++){
          if(signs[a]>0)lower=std::max(lower,-x[indices[a]]);else upper=std::min(upper,x[indices[a]]);
          linear+=signs[a]*gradient[indices[a]];
          for(size_t z=0;z<indices.size();z++)precision+=signs[a]*signs[z]*Q[indices[a]*d+indices[z]];
        }
        if(!(precision>0)||!std::isfinite(precision))throw std::runtime_error("Nonpositive Gaussian direction precision");
        if(upper-lower<1e-16)continue;
        double sd=1/std::sqrt(precision),mean=-linear/precision;
        double move=mean+sd*rng.truncated((lower-mean)/sd,(upper-mean)/sd);
        move=std::clamp(move,lower,upper);
        for(size_t a=0;a<indices.size();a++)x[indices[a]]+=move*signs[a];
        for(int a=0;a<d;a++){double change=0;for(size_t z=0;z<indices.size();z++)change+=Q[a*d+indices[z]]*signs[z];gradient[a]+=move*change;}
      }
      if(sweep>=burn&&(sweep-burn)%thin==0)std::cout.write(reinterpret_cast<const char*>(x.data()),d*sizeof(double));
    }
  }
  return 0;
}catch(const std::exception&e){std::cerr<<e.what()<<'\n';return 1;}}

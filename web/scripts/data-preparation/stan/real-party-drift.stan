// Conditional reference scales, original grouped multinomial counts, exact
// backward Gaussian transitions, common estimated volatilities, literal polls.
// Within-list coordinates remove count-induced curved ridges. Their Jacobian
// to component log odds is constant; unavailable coordinates stay auxiliary.
functions {
  vector decode(vector v, array[] int group, real nonvalid_offset) {
    int J=num_elements(group);
    vector[J] score=append_row(0,v);
    for(g in 2:J) {
      int first=0;
      vector[J] within=rep_vector(negative_infinity(),J);
      for(j in 2:J) if(group[j]==g) {
        if(first==0) {first=j;within[j]=0;} else within[j]=v[j-1];
      }
      if(first>0) {
        real z=log_sum_exp(within);
        for(j in 2:J) if(group[j]==g) score[j]=v[first-1]+within[j]-z;
      }
    }
    score[1]-=nonvalid_offset;
    return score;
  }
}
data {
  int<lower=2> J; int<lower=2> C; int<lower=1> T; int<lower=2> U;
  int<lower=0> Q; int<lower=1> N; int<lower=0> B;
  matrix[J,J-1] H; real state_offset;
  vector<lower=0>[U-1] dt;
  array[T] int<lower=1,upper=U> count_time;
  matrix[T,T] backward_cov;
  array[N] int<lower=1,upper=C> cell_c;
  array[N] int<lower=1,upper=T> cell_t;
  array[N,J] int<lower=0,upper=J> cell_group;
  array[N] int<lower=2,upper=J> cell_groups;
  array[N,J] int<lower=0> y;
  array[Q] int<lower=1,upper=U> poll_time;
  array[Q,J] int<lower=0,upper=J> poll_group;
  array[Q] int<lower=2,upper=J> poll_groups;
  array[Q] int<lower=1> poll_bias;
  array[Q] vector[J] poll_log_shares;
  array[Q] matrix[J,J-1] poll_H;
  array[Q] matrix[J-1,J-1] poll_L;
  array[C,T,J] int<lower=0,upper=J> coord_group;
}
parameters {
  array[C] matrix[T,J-1] coordinate;
  matrix[U,J-1] national_raw;
  matrix[B,J-1] bias_raw;
  real log_sigma_n;
  real log_sigma_l;
}
model {
  real sigma_n=exp(log_sigma_n);
  real sigma_l=exp(log_sigma_l);
  matrix[T,T] L=cholesky_decompose(rep_matrix(.25,T,T)+square(sigma_l)*backward_cov);
  matrix[U,J-1] national;
  national[U]=1.5*national_raw[U];
  for(u in 1:(U-1)) {
    int i=U-u;
    national[i]=national[i+1]+sigma_n*sqrt(dt[i])*national_raw[i];
  }
  log_sigma_n ~ normal(log(.35),.7);
  log_sigma_l ~ normal(log(.25),.7);
  to_vector(national_raw) ~ std_normal();
  to_vector(bias_raw) ~ std_normal();
  for(c in 1:C) {
    matrix[T,J-1] residual;
    for(t in 1:T) residual[t]=(H'*decode(coordinate[c][t]',coord_group[c,t],state_offset))'-national[count_time[t]];
    // All normalization terms remain, including determinants depending on sigma.
    for(j in 1:(J-1)) residual[,j] ~ multi_normal_cholesky(rep_vector(0,T),L);
  }
  for(n in 1:N) {
    vector[J] answer=rep_vector(negative_infinity(),J);
    real z;
    answer[1]=0;
    // The first coordinate in a group is its exact log probability ratio to
    // non-valid voting. Within-group log ratios cancel from this likelihood.
    for(j in 2:J) if(cell_group[n,j]>1 && is_inf(answer[cell_group[n,j]]))
      answer[cell_group[n,j]]=coordinate[cell_c[n]][cell_t[n],j-1];
    z=log_sum_exp(answer);
    for(g in 1:cell_groups[n]) if(y[n,g]>0) target+=y[n,g]*(answer[g]-z);
  }
  for(q in 1:Q) {
    int G=poll_groups[q];
    vector[J] score=H*national[poll_time[q]]'+.12*H*bias_raw[poll_bias[q]]';
    vector[J] answer=rep_vector(0,J);
    for(g in 1:G) {
      vector[J] member=rep_vector(negative_infinity(),J);
      for(j in 2:J) if(poll_group[q,j]==g) member[j]=score[j];
      answer[g]=log_sum_exp(member);
    }
    // Orthonormal contrasts annihilate the common log-normalizer.
    target+=multi_normal_cholesky_lpdf((poll_H[q]'*(answer-poll_log_shares[q]))[1:(G-1)]|
      rep_vector(0,G-1),poll_L[q][1:(G-1),1:(G-1)]);
  }
}
generated quantities {
  real sigma_n=exp(log_sigma_n);
  real sigma_l=exp(log_sigma_l);
}

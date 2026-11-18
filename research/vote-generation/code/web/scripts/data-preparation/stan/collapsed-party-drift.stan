// Same conditional-scale probability model as real-party-drift.stan.
// Integrate the national Gaussian field out of the electoral-state prior, then
// reconstruct it conditionally for polls. No geographic mean-zero constraint:
// the municipal mean has covariance K_n + K_l/C, including local mean variance.
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
  matrix[U,U] backward_all;
  matrix[U,T] backward_cross;
  int<lower=0,upper=1> fit_scales;
  real<lower=0> fixed_sigma_n;
  real<lower=0> fixed_sigma_l;
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
transformed data {
  matrix[T,T] fixed_local=rep_matrix(.25,T,T)+square(fixed_sigma_l)*backward_cov;
  matrix[T,T] fixed_mean=rep_matrix(2.25,T,T)+square(fixed_sigma_n)*backward_cov+fixed_local/C;
  matrix[U,T] fixed_cross=rep_matrix(2.25,U,T)+square(fixed_sigma_n)*backward_cross;
  matrix[U,T] fixed_gain=mdivide_left_spd(fixed_mean,fixed_cross')';
  matrix[U,U] fixed_conditional=rep_matrix(2.25,U,U)+square(fixed_sigma_n)*backward_all-fixed_gain*fixed_cross';
  matrix[T,T] fixed_L_local=cholesky_decompose(fixed_local);
  matrix[T,T] fixed_L_mean=cholesky_decompose(fixed_mean);
  matrix[U,U] fixed_L_conditional=cholesky_decompose((fixed_conditional+fixed_conditional')/2);
}
parameters {
  array[C] matrix[T,J-1] coordinate;
  matrix[U,J-1] national_raw;
  matrix[B,J-1] bias_raw;
  real log_sigma_n;
  real log_sigma_l;
}
model {
  array[C] matrix[T,J-1] state;
  matrix[T,J-1] mean_state=rep_matrix(0,T,J-1);
  matrix[U,J-1] national;
  for(c in 1:C) {
    for(t in 1:T) state[c][t]=(H'*decode(coordinate[c][t]',coord_group[c,t],state_offset))';
    mean_state+=state[c]/C;
  }
  if(fit_scales) {
    real sigma_n=exp(log_sigma_n);
    real sigma_l=exp(log_sigma_l);
    matrix[T,T] D_local=rep_matrix(.25,T,T)+square(sigma_l)*backward_cov;
    matrix[T,T] K_count=rep_matrix(2.25,T,T)+square(sigma_n)*backward_cov;
    matrix[U,U] K_all=rep_matrix(2.25,U,U)+square(sigma_n)*backward_all;
    matrix[U,T] K_cross=rep_matrix(2.25,U,T)+square(sigma_n)*backward_cross;
    matrix[T,T] L_local=cholesky_decompose(D_local);
    matrix[T,T] L_mean=cholesky_decompose(K_count+D_local/C);
    matrix[U,T] conditional_gain=mdivide_left_spd(K_count+D_local/C,K_cross')';
    matrix[U,U] conditional_cov=K_all-conditional_gain*K_cross';
    conditional_cov=(conditional_cov+conditional_cov')/2;
    national=conditional_gain*mean_state+cholesky_decompose(conditional_cov)*national_raw;
    target+=-(J-1)*(C-1)*sum(log(diagonal(L_local)));
    for(c in 1:C) target+=-.5*sum(columns_dot_self(mdivide_left_tri_low(L_local,state[c]-mean_state)));
    for(j in 1:(J-1)) mean_state[,j] ~ multi_normal_cholesky(rep_vector(0,T),L_mean);
  } else {
    national=fixed_gain*mean_state+fixed_L_conditional*national_raw;
    // All Gaussian factors now use data-only Cholesky factors.
    for(c in 1:C) target+=-.5*sum(columns_dot_self(mdivide_left_tri_low(fixed_L_local,state[c]-mean_state)));
    for(j in 1:(J-1)) mean_state[,j] ~ multi_normal_cholesky(rep_vector(0,T),fixed_L_mean);
  }
  log_sigma_n ~ normal(log(.35),.7);
  log_sigma_l ~ normal(log(.25),.7);
  to_vector(national_raw) ~ std_normal();
  to_vector(bias_raw) ~ std_normal();
  // Exact marginal density of C exchangeable trajectories. Constants depending
  // only on C and dimensions cancel, all scale-dependent determinants remain.
  for(n in 1:N) {
    vector[J] answer=rep_vector(negative_infinity(),J);
    real z;
    answer[1]=0;
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
    target+=multi_normal_cholesky_lpdf((poll_H[q]'*(answer-poll_log_shares[q]))[1:(G-1)]|
      rep_vector(0,G-1),poll_L[q][1:(G-1),1:(G-1)]);
  }
}
generated quantities {
  real sigma_n=fit_scales?exp(log_sigma_n):fixed_sigma_n;
  real sigma_l=fit_scales?exp(log_sigma_l):fixed_sigma_l;
}

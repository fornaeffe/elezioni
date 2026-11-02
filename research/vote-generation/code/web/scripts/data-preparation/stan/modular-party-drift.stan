// Research alternative: dynamic national electoral propensities; election-level
// overdispersion is integrated exactly through grouped Dirichlet-multinomial
// counts. Municipal profiles/residuals are a separate predictive module.
data {
  int<lower=2> J; int<lower=2> U; int<lower=1> E;
  int<lower=0> Q; int<lower=0> B;
  matrix[J,J-1] H; real state_offset;
  vector<lower=0>[U-1] dt;
  array[E] int<lower=1,upper=U> event_time;
  array[E] int<lower=0,upper=1> event_kind;
  array[E] int<lower=2,upper=J> event_groups;
  array[E,J] int<lower=0,upper=J> event_group;
  array[E,J] int<lower=0> y;
  array[Q] int<lower=1,upper=U> poll_time;
  array[Q] int<lower=0,upper=1> poll_kind;
  array[Q] int<lower=1> poll_bias;
  array[Q] int<lower=2,upper=J> poll_groups;
  array[Q,J] int<lower=0,upper=J> poll_group;
  array[Q] vector[J] poll_log_shares;
  array[Q] matrix[J,J-1] poll_H;
  array[Q] matrix[J-1,J-1] poll_L;
  real<lower=0> concentration;
  int<lower=0,upper=1> use_kind;
}
parameters {
  matrix[U,J-1] national_raw;
  matrix[B,J-1] bias_raw;
  vector[J-1] kind_raw;
  real log_sigma_n;
}
model {
  real sigma=exp(log_sigma_n);
  matrix[U,J-1] national;
  national[U]=1.5*national_raw[U];
  for(u in 1:(U-1)) {
    int i=U-u;
    national[i]=national[i+1]+sigma*sqrt(dt[i])*national_raw[i];
  }
  to_vector(national_raw) ~ std_normal();
  to_vector(bias_raw) ~ std_normal();
  kind_raw ~ std_normal();
  log_sigma_n ~ normal(log(.35),.7);
  for(e in 1:E) {
    int G=event_groups[e];
    vector[J] score=H*(national[event_time[e]]'+use_kind*event_kind[e]*.5*kind_raw);
    vector[G] answer;
    score[1]+=state_offset;
    for(g in 1:G) {
      vector[J] member=rep_vector(negative_infinity(),J);
      for(j in 1:J) if(event_group[e,j]==g) member[j]=score[j];
      answer[g]=log_sum_exp(member);
    }
    // These are original elector counts, not scaled or fabricated respondents.
    target+=dirichlet_multinomial_lpmf(y[e,1:G]|concentration*softmax(answer));
  }
  for(q in 1:Q) {
    int G=poll_groups[q];
    vector[J] score=H*(national[poll_time[q]]'+use_kind*poll_kind[q]*.5*kind_raw+.12*bias_raw[poll_bias[q]]');
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
  real sigma_n=exp(log_sigma_n);
  vector[J-1] reference_state=1.5*national_raw[U]';
  vector[J-1] kind_effect=use_kind*.5*kind_raw;
}

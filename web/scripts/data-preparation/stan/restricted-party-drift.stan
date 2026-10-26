// Gold-standard reduced validation of the ORIGINAL component Gaussian prior,
// grouped multinomial and share-space reference information. Offline only.
data {
  int<lower=1> D;
  int<lower=1> C;
  int<lower=0,upper=1> hard;
  matrix[4*C,D] design;
  vector[4*C] state_offset;
  array[C,3] int<lower=0> counts;
  vector<lower=0>[C] electors;
  real<lower=0,upper=1> reference_mean;
  real reference_location;
  real<lower=0> reference_scale;
  matrix[D,D-hard] complement;
  vector[D] direction;
}
transformed data {
  if (hard && C != 1) reject("This reduced hard-reference parameterization is one-city only");
}
parameters {
  vector[D-hard] z;
}
transformed parameters {
  vector[D] x;
  real eliminated = 0;
  if (hard) {
    vector[D] base = complement * z;
    vector[4] scores = head(state_offset + design * base, 4);
    vector[4] slope = head(design * direction, 4);
    real rate = slope[2] - slope[3];
    if (abs(slope[3]-slope[4]) > 1e-8 || rate <= 0) reject("Not a component-specific reference direction");
    eliminated = (logit(reference_mean) + log_sum_exp(scores[3:4]) - scores[2])/rate;
    x = base + eliminated*direction;
  } else x = z;
}
model {
  vector[4*C] scores = state_offset + design*x;
  real numerator = 0;
  real denominator = 0;
  z ~ std_normal();
  if (hard) {
    // Integrating delta(F-a) leaves exp(-eliminated^2/2)/|dF/da|.
    // For this ONE-city valid-share shift the derivative is the constant
    // rate*reference_mean*(1-reference_mean), so only the Gaussian term varies.
    target += -0.5*square(eliminated);
  }
  for (c in 1:C) {
    vector[4] s = segment(scores, 4*(c-1)+1, 4);
    vector[3] grouped;
    vector[4] p = softmax(s);
    grouped[1] = s[1];
    grouped[2] = log_sum_exp(s[2:3]);
    grouped[3] = s[4];
    target += multinomial_logit_lpmf(counts[c] | grouped);
    numerator += electors[c]*p[2];
    denominator += electors[c]*(1-p[1]);
  }
  if (!hard) {
    real a = numerator/denominator;
    // Correct logistic-normal reference FACTOR ON SHARE SPACE.
    target += normal_lpdf(logit(a) | reference_location, reference_scale)
      - log(a) - log1m(a);
  }
}
generated quantities {
  vector[4*C] scores = state_offset + design*x;
  vector[4] national_elector = rep_vector(0,4);
  real national_valid_a;
  real national_within_joint;
  real local_log_a_b = scores[2]-scores[3];
  for (c in 1:C) national_elector += electors[c]*softmax(segment(scores,4*(c-1)+1,4))/sum(electors);
  national_valid_a = national_elector[2]/(1-national_elector[1]);
  national_within_joint = national_elector[2]/sum(national_elector[2:3]);
}

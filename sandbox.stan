data {
  int<lower=0> L; // Numero di dati
  int<lower=0> K; // Numero di aree
  int y[L, K]; // Dati
}

parameters {
  vector[K] theta;
}

model {
  for (i in 1:L) {
    y[i] ~ multinomial(softmax(theta));
  }
  
  theta ~ normal(0, 1);
}


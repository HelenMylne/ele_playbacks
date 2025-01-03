// generated with brms 2.19.0
functions {
  /* compute monotonic effects
   * Args:
   *   scale: a simplex parameter
   *   i: index to sum over the simplex
   * Returns:
   *   a scalar between 0 and rows(scale)
   */
  real mo(vector scale, int i) {
    if (i == 0) {
      return 0;
    } else {
      return rows(scale) * sum(scale[1:i]);
    }
  }
  /* integer sequence of values
   * Args:
   *   start: starting integer
   *   end: ending integer
   * Returns:
   *   an integer sequence from start to end
   */
  int[] sequence(int start, int end) {
    int seq[end - start + 1];
    for (n in 1:num_elements(seq)) {
      seq[n] = n + start - 1;
    }
    return seq;
  }
  // compute partial sums of the log-likelihood
  real partial_log_lik_lpmf(int[] seq, int start, int end, data int[] Y, data matrix X, vector b, int[] Xmo_1, vector simo_1, vector bsp, data int[] J_1, data vector Z_1_1, vector r_1_1, data int[] J_2, data vector Z_2_1, vector r_2_1, data int[] J_3, data vector Z_3_1, vector r_3_1) {
    real ptarget = 0;
    int N = end - start + 1;
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    for (n in 1:N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      mu[n] += (bsp[1]) * mo(simo_1, Xmo_1[nn]) + r_1_1[J_1[nn]] * Z_1_1[nn] + r_2_1[J_2[nn]] * Z_2_1[nn] + r_3_1[J_3[nn]] * Z_3_1[nn];
    }
    ptarget += bernoulli_logit_glm_lpmf(Y[start:end] | X[start:end], mu, b);
    return ptarget;
  }
}
data {
  int<lower=1> N;  // total number of observations
  int Y[N];  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Ksp;  // number of special effects terms
  int<lower=1> Imo;  // number of monotonic variables
  int<lower=1> Jmo[Imo];  // length of simplexes
  int Xmo_1[N];  // monotonic variable
  vector[Jmo[1]] con_simo_1;  // prior concentration of monotonic simplex
  int grainsize;  // grainsize for threading
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  int<lower=1> J_2[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_1;
  // data for group-level effects of ID 3
  int<lower=1> N_3;  // number of grouping levels
  int<lower=1> M_3;  // number of coefficients per level
  int<lower=1> J_3[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_3_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  int seq[N] = sequence(1, N);
}
parameters {
  vector[K] b;  // population-level effects
  simplex[Jmo[1]] simo_1;  // monotonic simplex
  vector[Ksp] bsp;  // special effects coefficients
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // standardized group-level effects
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  vector[N_2] z_2[M_2];  // standardized group-level effects
  vector<lower=0>[M_3] sd_3;  // group-level standard deviations
  vector[N_3] z_3[M_3];  // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_1;  // actual group-level effects
  vector[N_2] r_2_1;  // actual group-level effects
  vector[N_3] r_3_1;  // actual group-level effects
  real lprior = 0;  // prior contributions to the log posterior
  r_1_1 = (sd_1[1] * (z_1[1]));
  r_2_1 = (sd_2[1] * (z_2[1]));
  r_3_1 = (sd_3[1] * (z_3[1]));
  lprior += normal_lpdf(b[1] | -1, 1);
  lprior += normal_lpdf(b[2] | -1, 1);
  lprior += normal_lpdf(b[3] | -1, 1);
  lprior += normal_lpdf(b[4] | -1, 1);
  lprior += normal_lpdf(b[5] | -1, 1);
  lprior += normal_lpdf(b[6] | -1, 1);
  lprior += normal_lpdf(b[7] | -1, 1);
  lprior += normal_lpdf(b[8] | -1, 1);
  lprior += normal_lpdf(b[9] | -1, 1);
  lprior += dirichlet_lpdf(simo_1 | con_simo_1);
  lprior += normal_lpdf(bsp[1] | -1, 1);
  lprior += student_t_lpdf(sd_1 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_2 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_3 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, Y, X, b, Xmo_1, simo_1, bsp, J_1, Z_1_1, r_1_1, J_2, Z_2_1, r_2_1, J_3, Z_3_1, r_3_1);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(z_1[1]);
  target += std_normal_lpdf(z_2[1]);
  target += std_normal_lpdf(z_3[1]);
}
generated quantities {
}

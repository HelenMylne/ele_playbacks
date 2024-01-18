# stancode for crossed random effects: 
# 
# // generated with brms 2.19.0
# functions {
#   /* cumulative-logit log-PDF for a single response
#   * Args:
#     *   y: response category
#   *   mu: latent mean parameter
#   *   disc: discrimination parameter
#   *   thres: ordinal thresholds
#   * Returns:
#     *   a scalar to be added to the log posterior
#   */
#     real cumulative_logit_lpmf(int y, real mu, real disc, vector thres) {
#       int nthres = num_elements(thres);
#       if (y == 1) {
#         return log_inv_logit(disc * (thres[1] - mu));
#       } else if (y == nthres + 1) {
#         return log1m_inv_logit(disc * (thres[nthres] - mu));
#       } else {
#         return log_diff_exp(
#           log_inv_logit(disc * (thres[y] - mu)), 
#           log_inv_logit(disc * (thres[y - 1] - mu))
#         );
#       }
#     }
#   /* cumulative-logit log-PDF for a single response and merged thresholds
#   * Args:
#     *   y: response category
#   *   mu: latent mean parameter
#   *   disc: discrimination parameter
#   *   thres: vector of merged ordinal thresholds
#   *   j: start and end index for the applid threshold within 'thres'
#   * Returns:
#     *   a scalar to be added to the log posterior
#   */
#     real cumulative_logit_merged_lpmf(int y, real mu, real disc, vector thres, int[] j) {
#       return cumulative_logit_lpmf(y | mu, disc, thres[j[1]:j[2]]);
#     }
#   /* ordered-logistic log-PDF for a single response and merged thresholds
#   * Args:
#     *   y: response category
#   *   mu: latent mean parameter
#   *   thres: vector of merged ordinal thresholds
#   *   j: start and end index for the applid threshold within 'thres'
#   * Returns:
#     *   a scalar to be added to the log posterior
#   */
#     real ordered_logistic_merged_lpmf(int y, real mu, vector thres, int[] j) {
#       return ordered_logistic_lpmf(y | mu, thres[j[1]:j[2]]);
#     }
#   /* compute monotonic effects
#   * Args:
#     *   scale: a simplex parameter
#   *   i: index to sum over the simplex
#   * Returns:
#     *   a scalar between 0 and rows(scale)
#   */
#     real mo(vector scale, int i) {
#       if (i == 0) {
#         return 0;
#       } else {
#         return rows(scale) * sum(scale[1:i]);
#       }
#     }
# }
# data {
#   int<lower=1> N;  // total number of observations
#   int Y[N];  // response variable
#   int<lower=2> nthres;  // number of thresholds
#   int<lower=1> K;  // number of population-level effects
#   matrix[N, K] X;  // population-level design matrix
#   int<lower=1> Ksp;  // number of special effects terms
#   int<lower=1> Imo;  // number of monotonic variables
#   int<lower=1> Jmo[Imo];  // length of simplexes
#   int Xmo_1[N];  // monotonic variable
#   vector[Jmo[1]] con_simo_1;  // prior concentration of monotonic simplex
#   int Xmo_2[N];  // monotonic variable
#   vector[Jmo[2]] con_simo_2;  // prior concentration of monotonic simplex
#   // data for splines
#   int Ks;  // number of linear effects
#   matrix[N, Ks] Xs;  // design matrix for the linear effects
#   // data for spline s(after_stim)
#   int nb_1;  // number of bases
#   int knots_1[nb_1];  // number of knots
#   // basis function matrices
#   matrix[N, knots_1[1]] Zs_1_1;
#   // data for group-level effects of ID 1
#   int<lower=1> N_1;  // number of grouping levels
#   int<lower=1> M_1;  // number of coefficients per level
#   int<lower=1> J_1[N];  // grouping indicator per observation
#   // group-level predictor values
#   vector[N] Z_1_1;
#   // data for group-level effects of ID 2
#   int<lower=1> N_2;  // number of grouping levels
#   int<lower=1> M_2;  // number of coefficients per level
#   int<lower=1> J_2[N];  // grouping indicator per observation
#   // group-level predictor values
#   vector[N] Z_2_1;
#   // data for group-level effects of ID 3
#   int<lower=1> N_3;  // number of grouping levels
#   int<lower=1> M_3;  // number of coefficients per level
#   int<lower=1> J_3[N];  // grouping indicator per observation
#   // group-level predictor values
#   vector[N] Z_3_1;
#   int prior_only;  // should the likelihood be ignored?
# }
# transformed data {
#   int Kc = K;
#   matrix[N, Kc] Xc;  // centered version of X
#   vector[Kc] means_X;  // column means of X before centering
#   for (i in 1:K) {
#     means_X[i] = mean(X[, i]);
#     Xc[, i] = X[, i] - means_X[i];
#   }
# }
# parameters {
#   vector[Kc] b;  // population-level effects
#   ordered[nthres] Intercept;  // temporary thresholds for centered predictors
#   simplex[Jmo[1]] simo_1;  // monotonic simplex
#   simplex[Jmo[2]] simo_2;  // monotonic simplex
#   vector[Ksp] bsp;  // special effects coefficients
#   vector[Ks] bs;  // spline coefficients
#   // parameters for spline s(after_stim)
#   // standarized spline coefficients
#   vector[knots_1[1]] zs_1_1;
#   real<lower=0> sds_1_1;  // standard deviations of spline coefficients
#   vector<lower=0>[M_1] sd_1;  // group-level standard deviations
#   vector[N_1] z_1[M_1];  // standardized group-level effects
#   vector<lower=0>[M_2] sd_2;  // group-level standard deviations
#   vector[N_2] z_2[M_2];  // standardized group-level effects
#   vector<lower=0>[M_3] sd_3;  // group-level standard deviations
#   vector[N_3] z_3[M_3];  // standardized group-level effects
# }
# transformed parameters {
#   // actual spline coefficients
#   vector[knots_1[1]] s_1_1;
#   real disc = 1;  // discrimination parameters
#   vector[N_1] r_1_1;  // actual group-level effects
#   vector[N_2] r_2_1;  // actual group-level effects
#   vector[N_3] r_3_1;  // actual group-level effects
#   real lprior = 0;  // prior contributions to the log posterior
#   // compute actual spline coefficients
#   s_1_1 = sds_1_1 * zs_1_1;
#   r_1_1 = (sd_1[1] * (z_1[1]));
#   r_2_1 = (sd_2[1] * (z_2[1]));
#   r_3_1 = (sd_3[1] * (z_3[1]));
#   lprior += normal_lpdf(b[1] | 0, 1);
#   lprior += normal_lpdf(b[2] | 0, 1);
#   lprior += student_t_lpdf(Intercept | 3, 0, 2.5);
#   lprior += dirichlet_lpdf(simo_1 | con_simo_1);
#   lprior += dirichlet_lpdf(simo_2 | con_simo_2);
#   lprior += normal_lpdf(bsp[1] | 0, 0.25);
#   lprior += normal_lpdf(bsp[2] | 0, 0.333);
#   lprior += normal_lpdf(bs[1] | 0, 1);
#   lprior += student_t_lpdf(sds_1_1 | 3, 0, 2.5)
#   - 1 * student_t_lccdf(0 | 3, 0, 2.5);
#   lprior += student_t_lpdf(sd_1 | 3, 0, 2.5)
#   - 1 * student_t_lccdf(0 | 3, 0, 2.5);
#   lprior += student_t_lpdf(sd_2 | 3, 0, 2.5)
#   - 1 * student_t_lccdf(0 | 3, 0, 2.5);
#   lprior += student_t_lpdf(sd_3 | 3, 0, 2.5)
#   - 1 * student_t_lccdf(0 | 3, 0, 2.5);
# }
# model {
#   // likelihood including constants
#   if (!prior_only) {
#     // initialize linear predictor term
#     vector[N] mu = rep_vector(0.0, N);
#     mu += Xc * b + Xs * bs + Zs_1_1 * s_1_1;
#     for (n in 1:N) {
#       // add more terms to the linear predictor
#       mu[n] += (bsp[1]) * mo(simo_1, Xmo_1[n]) + (bsp[2]) * mo(simo_2, Xmo_2[n]) + r_1_1[J_1[n]] * Z_1_1[n] + r_2_1[J_2[n]] * Z_2_1[n] + r_3_1[J_3[n]] * Z_3_1[n];
#     }
#     for (n in 1:N) {
#       target += ordered_logistic_lpmf(Y[n] | mu[n], Intercept);
#     }
#   }
#   // priors including constants
#   target += lprior;
#   target += std_normal_lpdf(zs_1_1);
#   target += std_normal_lpdf(z_1[1]);
#   target += std_normal_lpdf(z_2[1]);
#   target += std_normal_lpdf(z_3[1]);
# }
# generated quantities {
#   // compute actual thresholds
#   vector[nthres] b_Intercept = Intercept + dot_product(means_X, b);
# }



# Family: cumulative 
# Links: mu = logit; disc = identity 
# Formula: age_diff_num ~ 1 + mo(f_age_num) + stim_type + s(after_stim) + mo(nn_tminus1_num) + (1 | focal_id) + (1 | stim_id) + (1 | playback_id) 
# Data: nn_no_na (Number of observations: 40010) 
# Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
# total post-warmup draws = 4000
# 
# Smooth Terms: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sds(safter_stim_1)     0.50      0.51     0.01     1.82 1.00     1896     2015
# 
# Group-Level Effects: 
#   ~focal_id (Number of levels: 140) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     6.17      0.66     4.99     7.58 1.00      796     1558
# 
# ~playback_id (Number of levels: 33) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.79      0.57     0.03     2.10 1.01      302      422
# 
# ~stim_id (Number of levels: 23) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.74      0.56     0.03     2.02 1.01      391      287
# 
# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept[1]         2.71      0.90     0.94     4.52 1.00      856     1663
# Intercept[2]        12.54      0.92    10.70    14.35 1.00      871     1680
# stim_typeh           0.26      0.81    -1.34     1.86 1.01     1223     2449
# stim_typel           0.04      0.77    -1.51     1.54 1.00     1392     2136
# safter_stim_1       -0.30      0.74    -1.65     1.32 1.00     3699     2828
# mof_age_num         -0.74      0.26    -1.23    -0.20 1.00     1001     1656
# monn_tminus1_num     7.98      0.10     7.79     8.17 1.00     3677     2429
# 
# Simplex Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# mof_age_num1[1]          0.20      0.13     0.02     0.52 1.00     1981     1724
# mof_age_num1[2]          0.58      0.19     0.17     0.88 1.01     1216     2174
# mof_age_num1[3]          0.22      0.14     0.03     0.55 1.00     2265     2061
# monn_tminus1_num1[1]     0.51      0.01     0.49     0.52 1.00     3818     2343
# monn_tminus1_num1[2]     0.49      0.01     0.48     0.51 1.00     3818     2343
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# disc     1.00      0.00     1.00     1.00   NA       NA       NA
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).
# Warning message:
#   There were 199 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
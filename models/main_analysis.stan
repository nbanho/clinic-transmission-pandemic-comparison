data {
  int<lower=1> N;
  int<lower=0,upper=1>[N] year;
  int<lower=1> N_dt;
  int<lower=0,upper=1>[N_dt] year_dt;
  int<lower=1> D1[N];
  int<lower=1> D2[N];
  int<lower=1> D1_dt[N_dt];
  int<lower=1> D2_dt[N_dt];
  int<lower=0,upper=1> dt;
  real<lower=-1> y_dt[N_dt];
  int<lower=0,upper=1> is_mis_y_dt[N_dt];
  int n_mis_y_dt;
  int<lower=2,upper=6> st[N];
  real<lower=0> mu_y_dt[2];
  real<lower=0> sigma_y_dt[2];
  real<lower=-1> pt_dt[N_dt];
  int<lower=0,upper=1> is_mis_pt_dt[N_dt];
  int n_mis_pt_dt;
  real<lower=0> mu_pt_dt;
  real<lower=0> sigma_pt_dt;
  real<lower=-1> rH[N];
  real<lower=-1> co2[N];
  real<lower=-1> np[N];
  int<lower=0,upper=1> is_mis_rH[N];
  int n_mis_rH;
  int<lower=0,upper=1> is_mis_co2[N];
  int n_mis_co2;
  int<lower=0,upper=1> is_mis_np[N];
  int n_mis_np;
  real<lower=0> mu_rH;
  real<lower=0> sigma_rH;
  real<lower=0> mu_co2;
  real<lower=0> sigma_co2;
  real<lower=0> mu_np;
  real<lower=0> sigma_np;
  real<lower=0> registered[N];
  real<lower=0,upper=1> p_diag[N];
  real<lower=0> alpha_p_undiag;
  real<lower=0> beta_p_undiag;
}

parameters {
  real<lower=0> y_dt_mis[n_mis_y_dt];
  real<lower=0> pt_mis[n_mis_pt_dt];
  real<lower=0> rH_mis[n_mis_rH];
  real<lower=0> co2_mis[n_mis_co2];
  real<lower=0> np_mis[n_mis_np];
  real<lower=0,upper=1> p_undiag[N];
  vector[5] a;
  vector[4] b;
}

transformed parameters {
  real y_imp[N];
  real y_dt_imp[N_dt];
  real pt_imp[N];
  real pt_dt_imp[N_dt];
  real temp_imp[N];
  real rH_imp[N];
  real co2_imp[N];
  real np_imp[N];
  real y_imp_trans[N];
  real rH_imp_trans[N];
  real ach[N];
  real<lower=0> ipt[N];

  for (i in 1:N_dt) {
    y_dt_imp[i] = is_mis_y_dt[i] ? y_dt_mis[i] : y_dt[i];
    pt_dt_imp[i] = is_mis_pt_dt[i] ? pt_mis[i] : pt_dt[i];
  }

  for (i in 1:N) {
    y_imp[i] = sum(y_dt_imp[D1_dt == D1[i] & year_dt == year[i]]);
    pt_imp[i] = sum(pt_dt_imp[D1_dt == D1[i] & year_dt == year[i]]);
    rH_imp[i] = is_mis_rH[i] ? rH_mis[i] : rH[i];
    co2_imp[i] = is_mis_co2[i] ? co2_mis[i] : co2[i];
    np_imp[i] = is_mis_np[i] ? np_mis[i] : np[i];
  }
  
  y_imp_trans = log(y_imp .* inv(st));
  rH_imp_trans = log(rH_imp);
  ach = (6 * 10 ^ 4 * np_imp * 0.4) .* inv(180.405 * (co2_imp - 450))
  ipt = (p_diag + p_undiag) .* ipt;
}

model {
  for (i in 1:n_mis_y_dt) {
    if (mis_y_dt == 0) {
      y_dt_mis[i] ~ normal(mu_y_dt[1], sigma_y_dt[1]);
    } else {
      y_dt_mis[i] ~ normal(mu_y_dt[2], sigma_y_dt[2]);
    } 
  }
  for (i in 1:n_mis_pt_dt) {
    if (mis_pt_dt == 0) {
      pt_mis[i] ~ normal(mu_pt_dt, sigma_pt_dt);
    } else {
      pt_mis[i] ~ normal(mu_pt_dt, sigma_pt_dt);
    }
  }

  temp_mis ~ normal(mu_temp, sigma_temp);
  rH_mis ~ normal(mu_rH, sigma_rH);
  co2_mis ~ normal(mu_co2, sigma_co2);
  np_mis ~ normal(mu_np, sigma_np);

  p_undiag ~ beta(alpha_p_undiag, beta_p_undiag);

  a ~ normal(0, 1);
  b ~ normal(0, 1);

  pt_imp ~ normal(m_pt + beta_pt1 * year + beta_pt2 * registered, s_pt);
  ach ~ normal(m_ach + beta_ach1 * year, s_ach);
  rH_imp ~ normal(m_rH + beta_rH1 * year, s_rH);
  y_imp ~ normal(m_y + beta_y1 * year + beta_y2 * registered, s_y); 
}
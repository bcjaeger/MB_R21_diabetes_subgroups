
library(coxed)
library(survival)
library(tidyverse)
# one hundred participants in each trt grp

# note: the pipe ( |> ) is only compatible with R version >= 4.1

x <- 1:10

a <- mean(x)

# 4 subgroups of participants with diabetes and 2 treatment groups
# group 1: 2178
# group 2: 716
# group 3: 1222
# group 4: 1029
# each group is split evenly into treatment and control

# TODO 1. set trt/control assignment to match observed data
# TODO 2. figure out effect sizes for the simulation.

grp_sizes <- c(2178, 716, 1222, 1029)

data_trtgrps <- 
  tibble(grp_diabetes = rep(c(1,2,3,4), times = grp_sizes)) |> 
  mutate(grp_tx = rbinom(n = 5145, size = 1, prob = 1/2)) |> 
  mutate(grp_diabetes = factor(grp_diabetes, labels = letters[1:4]),
         grp_tx = factor(grp_tx, labels = c('no', 'yes')))

X <- model.matrix(~grp_diabetes * grp_tx, data = data_trtgrps)

beta <- rep(0, ncol(X)) |> 
  set_names(colnames(X))

beta_vec_static <- c(-0.03382,
                     0.14296,
                     -0.05733,
                     -0.11079,
                     0.67809,
                     -0.13098,
                     -0.09134)

beta_vec_decay <- function(beta_vec, decay_coef){
  
  beta_vec_new = beta_vec
  beta_vec_new[c(4:7)] <- beta_vec_new[c(4:7)] * (1-decay_coef)
  return(beta_vec_new)
  
}

beta_vec_decay_10 <- beta_vec_decay(beta_vec = beta_vec_static, 
                                    decay_coef = 0.10)

sim_run <- function(beta, X, beta_vec, data_trtgrps, cens_prop){
  
  beta["grp_diabetesb"]           <- beta_vec[1]
  beta["grp_diabetesc"]           <- beta_vec[2]
  beta["grp_diabetesd"]           <- beta_vec[3]
  beta["grp_txyes"]               <- beta_vec[4]
  beta["grp_diabetesb:grp_txyes"] <- beta_vec[5]
  beta["grp_diabetesc:grp_txyes"] <- beta_vec[6]
  beta["grp_diabetesd:grp_txyes"] <- beta_vec[7]
  
  data_sim <- sim.survdata(N = nrow(data_trtgrps),
                           T = 9 * 365,
                           censor = cens_prop,
                           type = 'none',
                           X = X,
                           beta = beta)
  
  data_mdl <- data_trtgrps |> 
    mutate(time = data_sim$data$y,
           status = as.numeric(data_sim$data$failed))
  
  browser()
  
  mdl_cox <- 
    coxph(Surv(time, status) ~ grp_diabetes * grp_tx, data = data_mdl)
  
  anova(mdl_cox)
  
}

sim_run(beta, X, beta_vec = beta_vec_static, data_trtgrps, cens_prop = 0.84)







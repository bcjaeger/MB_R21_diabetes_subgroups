
library(coxed)
library(survival)
library(tidyverse)
# one hundred participants in each trt grp

# note: the pipe ( |> ) is only compatible with R version >= 4.1

# 4 subgroups of participants with diabetes and 2 treatment groups
# group 1: 2178
# group 2: 716
# group 3: 1222
# group 4: 1029
# each group is split evenly into treatment and control

# TODO 1. set trt/control assignment to match observed data
# TODO 2. figure out effect sizes for the simulation.

              # g1  g2  g3  g4
grp_sizes <- c(841,268,448,396,     # control
               844,230,481,433) |>  # treatment
  matrix(nrow = 2, byrow = TRUE)

grp_sizes_by_diab <- apply(grp_sizes, 2, sum)

data_trtgrps <- tibble(
  grp_diabetes = rep(c(1,2,3,4), times = grp_sizes_by_diab)
) |> 
  mutate(grp_tx = 0) %>%
  split(f = .$grp_diabetes) |> 
  map2_dfr(
    .y = grp_sizes[2, ],
    ~ {
      .x$grp_tx[sample(nrow(.x), .y)] = 1
      .x
    }
  ) |> 
  mutate(
    grp_tx = factor(grp_tx, labels = c('no', 'yes')),
    grp_diabetes = factor(grp_diabetes, labels = letters[1:4])
  )

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
  
  data_sim <- suppressWarnings(
    sim.survdata(N = nrow(data_trtgrps),
                 T = 9 * 365,
                 censor = cens_prop,
                 type = 'none',
                 X = X,
                 beta = beta)
  )
  
  data_mdl <- data_trtgrps |> 
    mutate(time = data_sim$data$y,
           status = as.numeric(data_sim$data$failed))
  
  mdl_cox <- 
    coxph(Surv(time, status) ~ grp_diabetes * grp_tx, data = data_mdl)
  
  mdl_cox_anova <- anova(mdl_cox)
  
  # mdl_cox_bias <- beta[-1] - coef(mdl_cox) 
  # list(
  #   anova = mdl_cox_anova,
  #   bias = mdl_cox_bias
  # )
  
  tidy(mdl_cox_anova)
  
  
}

results <- expand_grid(
  beta_vec = list(static = beta_vec_static, 
                  decay10 = beta_vec_decay_10),
  cens = c(0.6, 0.7, 0.8)
) |> 
  mutate(
    beta_type = names(beta_vec),
    results = map2(
      .x = beta_vec, 
      .y = cens, 
      .f = ~ {
        replicate(
          n = 1000,
          expr = sim_run(beta, X, .x, data_trtgrps, .y),
          simplify = FALSE
        )
      }
    )
  )

library(gt)

results |> 
  unnest(cols = results) |> 
  unnest(cols = results) |> 
  filter(term == 'grp_diabetes:grp_tx') |> 
  group_by(cens, beta_type) |> 
  summarize(power = mean(p.value < 0.05), .groups = 'drop') |> 
  pivot_wider(names_from = beta_type, values_from = power) |> 
  gt(rowname_col = 'beta_type')









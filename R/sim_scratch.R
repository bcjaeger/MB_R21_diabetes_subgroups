
library(coxed)
library(survival)
library(tidyverse)
# one hundred participants in each trt grp

data_trtgrps <- 
  expand.grid(i = 1:100, grp_1 = 1:4, grp_2 = 1:2) |> 
  as_tibble() |> 
  select(-i) |> 
  mutate(grp_1 = factor(grp_1, labels = letters[1:4]),
         grp_2 = factor(grp_2, labels = letters[5:6]))

X <- model.matrix(~grp_1 * grp_2, data = data_trtgrps)

beta <- rep(0, ncol(X)) |> 
  set_names(colnames(X))

eff <- 0.25
beta["grp_1b"] <- eff * 1
beta["grp_1c"] <- eff * 2
beta["grp_1d"] <- eff * 3
beta["grp_2f"] <- eff * (-1) 
beta["grp_1b:grp_2f"] <- eff * (-1) * 1
beta["grp_1c:grp_2f"] <- eff * (-1) * 2
beta["grp_1d:grp_2f"] <- eff * (-1) * 3


data_sim <- sim.survdata(N = 1000,
                         T = 5000,
                         censor = 1/2,
                         type = 'none',
                         X = X,
                         beta = beta)


data_mdl <- data_trtgrps |> 
  mutate(time = data_sim$data$y,
         status = as.numeric(data_sim$data$failed))

mdl_cox <- coxph(Surv(time, status) ~ grp_1 * grp_2, data = data_mdl)

anova(mdl_cox)

#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param beta 
#' @param data_init 
#' @param cens_prop 

sim_run <- function(beta, 
                    cens_prop, 
                    data_init, 
                    data_xmat, 
                    time_max = 10*365){

  
  cat("\n", "beta:", round(beta,2), "\n",
      "cens_prop:", 100*cens_prop, "\n")
  
  data_sim <- suppressWarnings(
    sim.survdata(N = nrow(data_init),
                 T = time_max,
                 censor = cens_prop,
                 type = 'none',
                 X = data_xmat,
                 beta = beta)
  )
  
  data_mdl <- data_init |> 
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


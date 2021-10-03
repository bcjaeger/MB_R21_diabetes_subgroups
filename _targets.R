## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

               # g1   g2   g3   g4
  grp_sizes_aim3 = c(841, 268, 448, 396,     # control
                     844, 230, 481, 433) |>  # treatment
    matrix(nrow = 2, byrow = TRUE),
  
  grp_sizes_aim3_by_diab = apply(grp_sizes_aim3, 2, sum),
  
  data_aim3_init = data_initialize(grp_sizes_aim3, 
                                   grp_sizes_aim3_by_diab),
  
  data_aim3_xmat = data_conv_to_mat(data_aim3_init),
  
  beta_aim3 = c(0.00000, -0.03382,  0.14296, -0.05733,
                -0.11079,  0.67809, -0.13098, -0.09134) |> 
    set_names(colnames(data_aim3_xmat)),
  
  beta_aim3_10 = beta_vec_decay(beta_aim3, .10),
  
  beta_aim3_20 = beta_vec_decay(beta_aim3, .20),
  
  results_aim3_init = expand_grid(
    beta_vec = list(static  = beta_aim3, 
                    decay10 = beta_aim3_10,
                    decay20 = beta_aim3_20),
    cens = c(0.6, 0.7, 0.8)
  ),
  
  results_aim3_filled = results_fill(results_aim3_init, 
                                     data_aim3_init, 
                                     data_aim3_xmat, 
                                     n_runs = 250),
  
  results_aim3_gt = results_tabulate(results_aim3_filled),
  
  tar_render(report, "docs/index.Rmd")
  
)

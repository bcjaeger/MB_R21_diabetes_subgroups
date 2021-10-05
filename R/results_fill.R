#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param results_init
results_fill <- function(results_init,
                         data_init,
                         data_xmat,
                         n_runs = 1,
                         time_max = 10*365) {

  mutate(
    results_init,
    beta_type = names(beta_vec),
    results = map2(
      .x = beta_vec, 
      .y = cens, 
      .f = ~ {
        replicate(
          n = n_runs,
          expr = sim_run(.x, .y,
                         data_init, 
                         data_xmat,
                         time_max = time_max),
          simplify = FALSE
        )
      }
    )
  )

}

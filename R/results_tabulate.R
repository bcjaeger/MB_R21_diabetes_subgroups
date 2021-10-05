#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param results_filled
results_tabulate <- function(...) {
  
  bind_rows(...) |> 
    unnest(results) |> 
    unnest(results) |>
    filter(term == 'grp_diabetes:grp_tx') |> 
    group_by(cens, beta_type) |> 
    summarize(power = mean(p.value < 0.05), .groups = 'drop') |> 
    pivot_wider(names_from = beta_type, values_from = power) |> 
    mutate(across(everything(), ~.x*100))
  
}

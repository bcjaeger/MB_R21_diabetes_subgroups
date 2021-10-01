#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param results_filled
results_tabulate <- function(results_filled) {
  
  results_filled |> 
    unnest(results) |> 
    unnest(results) |>
    filter(term == 'grp_diabetes:grp_tx') |> 
    group_by(cens, beta_type) |> 
    summarize(power = mean(p.value < 0.05), .groups = 'drop') |> 
    pivot_wider(names_from = beta_type, values_from = power) |> 
    mutate(across(everything(), ~.x*100)) |>
    gt(rowname_col = 'cens') |> 
    tab_stubhead(label = '% Censored') |> 
    cols_move_to_start('static') |> 
    cols_label(static = '0', decay10 = '10', decay20 = '20') |> 
    tab_spanner(columns = c('static', 'decay10', 'decay20'),
                label = '% decay in effect size')
  
}

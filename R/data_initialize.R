#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param grp_sizes
#' @param grp_sizes_by_diab
data_initialize <- function(grp_sizes, grp_sizes_by_diab) {

  tibble(
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

}

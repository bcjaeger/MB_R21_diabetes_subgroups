#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_init
data_conv_to_mat <- function(data_init) {

  model.matrix(~grp_diabetes * grp_tx, data = data_init)

}

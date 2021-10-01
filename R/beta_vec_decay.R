#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param beta_vec
#' @param decay_coef
beta_vec_decay <- function(beta_vec, decay_coef){
  
  beta_vec_new = beta_vec
  
  beta_vec_new[c(5:8)] <- beta_vec_new[c(5:8)] * (1-decay_coef)
  
  return(beta_vec_new)
  
}

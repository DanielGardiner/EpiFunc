#' Restrict the number of decimals places for a numeric value
#'
#' @import stringr
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param x a numeric vector
#' @param n number of decimals
#' @return a numeric vector
#' @export
#' @examples
#' x = c(1.001, 1.23232211, 1.211111, 1.00001, 1, 2, 3.1, 3.66)
#'
#' n_decimals(x, 0)
#'
#' n_decimals(x, 5)
#'
#' n_decimals(x, 10)
n_decimals = function(x, n){
  sprintf(paste0("%.", n, "f"), round(x, n))
}


#' Compare the set of differences between two vectors
#'
#' @param x a vector
#' @param y a vector
#'
#' @return a list of length 3 containing (X_not_in_Y, Y_not_in_X and in_X_and_y)
#' @export
#'
#' @examples
#' a = c("A", "B", "C")
#'
#' b = c("C", "D", "E")
#'
#' set_diff(a, b)
set_diff <- function(x, y) {
  Xdiff = setdiff(x, y)
  Ydiff = setdiff(y, x)
  list(X_not_in_Y = Xdiff, Y_not_in_X = Ydiff,
       in_X_and_y = unique(c(x,y)[(c(x,y) %in% x) &
                                    (c(x,y) %in% y)]))
}

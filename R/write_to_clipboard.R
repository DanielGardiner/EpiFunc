#' Write R data to clipboard
#'
#' @param x a data.frame to be copied to clipboard
#' @param row.names a logical indicating if row names should be included
#' @param col.names a logical indicating if column names should be included
#' @param ... other read.table options
#'
#' @return data.frame copied to clipboard
#' @export
#'
#' @description A function to easily copy data out of R into a spreadsheet.
#'              This should not be used within a reproducible work flow, instead
#'              it should only be used on an ad-hoc basis
#'              (note: I'm not aware of the original author of this function,
#'              it was taken directly from a stack overflow answer)
#'
#' @examples
#' # set dummy data
#'
#' set.seed(2)
#'
#' data = data.frame(date = sample(seq(as.Date("2017-03-01"), as.Date("2017-08-01"), 1), 20, replace = TRUE),
#'                   sex = sample(c("Female", "Male"), 20, replace = TRUE))
#'
#' # apply function to write data to clipboard
#'
#' write_to_clipboard(data)
#'
#' # now paste data into a spreadsheet (i.e. open a spreadsheet and press ctrl + v)
write_to_clipboard <- function(x, row.names=FALSE, col.names=TRUE, ...) {

  write.table(x, "clipboard", sep="\t", row.names = row.names,
              col.names = col.names, ...)

}





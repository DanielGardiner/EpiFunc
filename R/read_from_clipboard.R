#' Read clipboard data into R
#'
#' @param header a logical indicating if a header has been copied
#' @param ... other read.table options
#'
#' @return a data.frame consisting of data copied to clipboard
#' @export
#'
#' @description A function to easily copy data into R from a spreadsheet.
#'              This should not be used within a reproducible work flow, instead
#'              it should only be used on an ad-hoc basis
#'              (note: I'm not aware of the original author of this function,
#'              it was taken directly from a stack overflow answer)
#'
#' @examples
#' # first copy data to clipboard (i.e. highlight data in spreadsheet and press ctrl + c)
#'
#' # apply function to read data from clipboard into R
#'
#' data = read_from_clipboard()
read_from_clipboard <- function(header=TRUE, ...) {

  read.table("clipboard", sep = "\t", header = header, ...)

}



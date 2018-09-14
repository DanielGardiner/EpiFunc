#' Format column names
#'
#' @import stringr stringi
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param data a data.frame
#' @return a data.frame
#' @export
#' @examples
#' set dummy data with horrible column names
#'
#' data = data.frame(t1 = 1, t2 = 1, t3 = 1, t4 = 1, t5 = 1)
#'
#' colnames(data) = c("Col_$%1", "2Col2", "Column number 3....",
#'                    "Col___4  ", "Col!!!!!!!!!!5")
#'
#' # look at data
#'
#' data
#'
#' # apply function
#'
#' format_colnames(data)
format_colnames = function(data) {

  colnames = tolower(colnames(data))

  colnames = stringi::stri_trans_general(colnames, "latin-ASCII")

  colnames = str_replace_all(colnames, "[^a-z0-9]", ".")

  colnames[str_detect(colnames, "^[0-9]")] = paste0("x", colnames[str_detect(colnames, "^[0-9]")])

  while(any(str_detect(colnames, "\\.\\."))){
    colnames = str_replace(colnames, "\\.\\.", ".")
  }

  colnames(data) = colnames

  data

}

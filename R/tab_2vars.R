#' Tabulate 2 variables
#'
#' @import tidyverse
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param data a data.frame
#' @param row.var a character specifying the variable to tabulate along rows, if set to NULL the first variable will be used
#' @param cols.var a character specifying the variable to tabulate along columns, if set to NULL the second variable will be used
#' @param complete a logical specifying whether to use all levels for factor variables
#' @param include.row.total a logical specifying whether to include row totals
#' @param include.col.total a logical specifying whether to include column totals
#' @export
#' @examples
#' # set dummy data
#'
#' set.seed(4)
#'
#' data = data.frame(date = sample(seq(as.Date("2017-01-01"), as.Date("2018-06-01"), 1), 200, replace = TRUE),
#'                   sex = factor(c("M", "M", "F", NA, NA), c("F", "M", "Unk")),
#'                   conf = sample(c("Confirmed", "Probable", "Probable"), 200, replace = TRUE),
#'                   status = sample(c("Student", "Staff", NA), 200, replace = TRUE),
#'                   geog = sample(c("South", "North"), 200, replace = TRUE))
#'
#' # apply function
#'
#' tab_2vars(data, row.var = "sex", col.var = "conf")
#'
#' # using dplyr syntax
#'
#' data %>%
#' select(sex, conf) %>%
#'   tab_2vars()
tab_2vars = function(data, row.var = NULL, col.var = NULL,
                     complete = TRUE,
                     include.row.total = TRUE,
                     include.col.total = TRUE){

  data = as.data.frame(data)

  if(is.null(row.var) & is.null(col.var)){

    row.var = colnames(data)[1]

    col.var = colnames(data)[2]

  } else {

    NULL

  }

  temp = data.frame(x = data[, col.var],
                    y = data[, row.var])

  temp = temp %>%
    group_by(x, y) %>%
    tally() %>%
    ungroup()

  if(complete){
    temp = temp %>%
      complete(x, y, fill = list(n = 0))
  }

  temp = temp %>%
    spread(x, n)

  row.total = temp %>%
    select(-1) %>%
    apply(1, sum)

  col.total = temp %>%
    select(-1) %>%
    apply(2, sum) %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame() %>%
    add_column(y = "Total", .before = 1) %>%
    add_column(Total = sum(row.total))

  temp$Total = row.total

  temp = rbind(temp, col.total)

  colnames(temp)[1] = row.var

  if(!include.row.total){
    temp = temp[1:(nrow(temp)-1), ]
  }

  if(!include.col.total){
    temp = temp[, 1:(ncol(temp)-1)]
  }

  temp

}

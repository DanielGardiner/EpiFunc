#' Tabulate each variable separately
#'
#' @import tidyverse
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param data a data.frame
#' @param cols a character specifying the variables to, if set to NULL all variables will be used
#' @param trunc.length a numeric specificying the number of characters to use when truncating the output
#' @param row.break.value the break value to use to separate variables in the output
#' @param complete a logical specifying whether to use all levels for factor variables
#' @param arrange.factor.by a character with value either "level" or "value" describing how a factor variable should be ordered
#' @param show.percentage a logical specifying whether to show percentages in output
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
#' tab_each(data, cols = c("date", "sex", "conf", "geog"))
#'
#' # using dplyr syntax
#'
#' data %>%
#' select(date, sex, conf, geog) %>%
#'   tab_each()
tab_each = function(data,
                    cols = NULL,
                    trunc.length = NULL,
                    row.break.value = ".",
                    complete = FALSE,
                    arrange.factor.by = "value",
                    show.percentage = TRUE,
                    n.decimals = 0){

  # convert to data.frame

  data = as.data.frame(data)

  # if cols argument is supplied then restrict to only those cols

  if(is.null(cols)){

    NULL

  } else {

    data = data %>%
      select_(.dots = cols)

  }

  # create a vector of column names

  vars = colnames(data)

  # initialize temp object

  temp = NULL

  # loop over each value in var vector of column names

  for(i in seq_along(vars)){

    # apply tab_1var function amd add variable and type columns

    x = data %>%
      tab_1var(vars[i], complete,
               arrange.factor.by, show.percentage, n.decimals) %>%
      mutate(.variable = vars[i],
             .variable = ifelse(duplicated(.variable), "", .variable),
             .type = class(data[, vars[i]])[1],
             .type = ifelse(duplicated(.type), "", .type)) %>%
      select(.variable, .type, everything())

    # rename columns

    colnames(x) = c("variable", "type", "level", "value")

    # convert level and value to character

    x = x %>%
      mutate(level = as.character(level),
             value = as.character(value))

    # append each data.frame in loop onto the next (using the row.break.value
    # argument to specify value to seperate each data.frame)

    temp = rbind(temp, x, row.break.value)
  }

  # apply trunc.length argument to truncate level values

  if(is.null(trunc.length)){

    NULL

  } else {

    temp = temp %>%
      mutate(level = str_sub(level, 1, trunc.length))

  }

  # output data.frame

  temp

}

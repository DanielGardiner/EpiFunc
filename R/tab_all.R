#' Tabulate all variables
#'
#' @import tidyverse
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param data a data.frame
#' @param var a character specifying the variable to tabulate, if set to NULL the first variable will be used
#' @param by a character specifying the variable to stratify by, if set to NULL no stratification will be used
#' @param complete a logical specifying whether to use all levels for factor variables
#' @param arrange.factor.by a character with value either "level" or "value" describing how a factor variable should be ordered
#' @param show.percentage a character either 'column' or 'row' or NULL to indicate whether to show percentages in output
#' @param row.break.value a character specifying the value to use as row breaks
#' @param show.na.percentage a logical specifying whether to show percentages for NA values
#' @param n.decimals a numeric specifying the number of decimal places to show
#' @param trunc.length a numeric specifying the maximum character length to be shown in the output
#' @param plotit a logical specifying whether to plot the output
#'
#' @export
#' @examples
#'
#' # set dummy data
#'
#' set.seed(4)
#'
#' data = data.frame(onset.date = sample(seq(as.Date("2017-01-01"), as.Date("2018-06-01"), 1), 200, replace = TRUE),
#'                   sex = factor(c("M", "M", "F", NA, NA), c("F", "M", "Unk")),
#'                   conf = sample(c("Confirmed", "Probable", "Probable"), 200, replace = TRUE),
#'                   status = sample(c("Student", "Staff", NA), 200, replace = TRUE),
#'                   geog = sample(c("South", "North", NA), 200, replace = TRUE))
#'
#' # apply function
#'
#' tab_all(data, var = c("sex", "onset.date", "geog"))
#'
#' tab_all(data, var = c("sex", "onset.date", "geog"), by = "conf")
#'
#' # using dplyr syntax
#'
#' data %>%
#'   select(conf, sex, onset.date, geog) %>%
#'   tab_all(by = "conf")
#'
#' data %>%
#'   select(conf, sex, onset.date, geog) %>%
#'   tab_all(by = "conf",
#'           show.percentage = "row",
#'           row.break.value = "_____")
#'
#' data %>%
#'   select(conf, sex, onset.date, geog) %>%
#'   tab_all(by = "conf",
#'           show.percentage = "row",
#'           plotit = TRUE)
#'
tab_all = function(data,
                   var = NULL,
                   by = NULL,
                   complete = FALSE,
                   arrange.factor.by = "value",
                   show.percentage = "column",
                   show.na.percentage = TRUE,
                   row.break.value = " ",
                   trunc.length = 60,
                   n.decimals = 0,
                   plotit = FALSE){

  # convert to data.frame

  data = as.data.frame(data)

  # if var argument is supplied then restrict to only those var

  if(is.null(var)){

    NULL

  } else {

    data = data %>%
      select_(.dots = c(var, by))

  }

  # create a vector of column names to loop over (exclude by)

  vars = colnames(data)

  vars = vars[!(vars %in% by)]

  # initialize temp object

  temp = NULL

  # loop over each value in var vector of column names

  for(i in seq_along(vars)){

    # apply tab_1var function amd add variable and type columns

    x = data %>%
      tab_var(var = vars[i],
              by = by,
              complete = complete,
              arrange.factor.by,
              show.percentage,
              show.na.percentage,
              n.decimals,
              trunc.length,
              plotit = FALSE)

    colnames(x)[is.na(colnames(x))] = ".NA"

    x = x %>%
      mutate(.variable = vars[i],
             .variable = ifelse(duplicated(.variable), "", .variable),
             .type = class(data[, vars[i]])[1],
             .type = ifelse(duplicated(.type), "", .type)) %>%
      select(.variable, .type, everything())

    # rename columns

    colnames(x)[1:3] = c("variable", "type", "level")

    # convert level and value to character

    x = x %>%
      mutate_all(funs(as.character(.)))

    # append each data.frame in loop onto the next (using the row.break.value
    # argument to specify value to seperate each data.frame)

    temp = rbind(temp, x, row.break.value)

  }


  # output data.frame

  temp

  if(plotit) {

    output = list()

    output[["table"]] = temp

    for(i in seq_along(vars)){

      p = data %>%
        tab_var(var = vars[i],
                by = by,
                complete = complete,
                arrange.factor.by,
                show.percentage,
                show.na.percentage,
                n.decimals,
                trunc.length,
                plotit = TRUE)

      output[[names(p[2])]] = p[[2]]

    }

    return(output)

  } else {

    return(temp)

  }

}


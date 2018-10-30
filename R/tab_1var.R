#' Tabulate 1 variable
#'
#' @import tidyverse
#' @import stringr
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param data a data.frame
#' @param var a character specifying the variable to tabulate, if set to NULL the first variable will be used
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
#' tab_1var(data, "sex")
#'
#' # using dplyr syntax
#'
#' data %>%
#' select(sex) %>%
#'   tab_1var()
tab_1var = function(data,
                    var = NULL,
                    complete = FALSE,
                    arrange.factor.by = "value",
                    show.percentage = TRUE,
                    n.decimals = 0,
                    include.na.percentage = TRUE,
                    include.total = TRUE){

  # check arguments are valid

  if(!is.logical(complete)){
    stop("complete must be a logical either: TRUE or FALSE")
  }

  if(!(arrange.factor.by == "value" | arrange.factor.by == "level")){
    stop("arrange.factor.by must be a character either: 'value' or 'level'")
  }

  # convert data to data.frame

  data = as.data.frame(data)

  # extract out the variable to tabulate as a vector and assign to 'x'
  # if var argument is null use first column in data

  if(is.null(var)){

    var = colnames(data)[1]

  } else {

    NULL

  }

  x = data[, var]

  if(is.factor(x) | is.character(x)){

    temp = x %>%
      table(exclude = FALSE) %>%
      data.frame()


    if(show.percentage){

      if(include.na.percentage){

        temp = temp %>%
          mutate(value = paste0(Freq, " (", n_decimals(100*Freq/sum(Freq), n = n.decimals), "%)"))

      } else {

        temp = temp %>%
          mutate(non.na.Freq = ifelse(is.na(.),
                                      0,
                                      Freq),
                 value = ifelse(is.na(.),
                                paste0(Freq, " (-)"),
                                paste0(Freq, " (", n_decimals(100*Freq/sum(non.na.Freq),
                                                              n = n.decimals), "%)"))) %>%
          select(-non.na.Freq)

      }

      colnames(temp) = c(var, "Freq", "n (%)")

    } else {

      temp = temp %>%
        mutate(value = Freq)

      colnames(temp) = c(var, "Freq", "n")

    }


    # if the variable is not a factor reorder the table from largest to smallest

    if(is.factor(x) & arrange.factor.by == "level") {

      NULL

    } else {

      temp = temp %>%
        arrange(desc(Freq))

    }

    # rearrange table so that NA is at the bottom

    temp.na = temp[is.na(temp[, 1]), ]

    temp.non.na = temp[!is.na(temp[, 1]), ]

    temp = rbind(temp.non.na, temp.na)

    # keep only rows with non-zero values if complete = FALSE
    # otherwise leave unchanged (i.e. include all factor levels with 0 values)

    if(complete){

      NULL

    } else {

      temp = temp %>%
        filter(Freq != 0)

    }

    temp = temp %>%
      select(-Freq)

    if(include.total){

      total = data.frame(v1 = ".Total") %>%
        mutate(v2 = sum(as.numeric(str_split(temp[, 2], " ", simplify = TRUE)[, 1]),
                        na.rm = TRUE)) %>%
        mutate(v2 = ifelse(colnames(temp)[2] == "n (%)",
                           paste0(v2, " (", n_decimals(100, n.decimals), "%)"),
                           v2))

      colnames(total) = colnames(temp)

      temp = rbind(temp, total)

    } else {

      NULL

    }

  } else if(is.logical(x)){

    x = paste0(".", as.character(x))

    # the variable is logical rename TRUE to .TRUE and FALSE to .FALSe
    # then use the cross_tab function to tabulate data

    temp = x %>%
      table(exclude = FALSE) %>%
      data.frame()

    temp[, 1] = str_replace(temp[, 1], "\\\\.", "")

    if(show.percentage){

      temp = temp %>%
        mutate(`n (%)` = paste0(Freq, " (", n_decimals(100*Freq/sum(Freq), n = n.decimals), "%)")) %>%
        select(-Freq)

    } else {

      temp = temp %>%
        mutate(n = Freq) %>%
        select(-Freq)
    }

    # rearrange table so that NA is at the bottom

    temp.na = temp[temp[, 1] == "NA", ]

    temp.non.na = temp[temp[, 1] != "NA", ]

    temp = rbind(temp.non.na, temp.na)

    colnames(temp) = c(var, "value")

  } else if(any(class(x) %in% c("Date", "POSIXct", "POSIXt"))){

    # if the variable is a date, POSIXct or POSIXc summarise the the number
    # of valid/NA dates and caluclate earliest, median, mean and latest date
    # then transpose the data.frame

    temp = data.frame(n.valid = sum(!is.na(x)),
                      n.NA = sum(is.na(x)),
                      Earliest = min(x, na.rm = TRUE),
                      Median = median(x, na.rm = TRUE),
                      Mean = mean(x, na.rm = TRUE),
                      Latest = max(x, na.rm = TRUE)) %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column()

    colnames(temp) = c(var, "value")

  } else if(is.numeric(x)){

    # if the variable is numeric summarise the the number of valid/NA values,
    # and caluclate min, median, mean and max value
    # then transpose the data.frame

    temp = data.frame(n.valid = sum(!is.na(x)),
                      n.NA = sum(is.na(x)),
                      Min = n_decimals(min(x), n = 1),
                      Median = n_decimals(median(x), n = 1),
                      Mean = n_decimals(mean(x), n = 1),
                      Max = n_decimals(max(x), n = 1)) %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column()

    colnames(temp) = c(var, "value")

  } else {

    # give a warning if an unexpected data type is recieved

    stop("Unrecognised data type")

  }


  temp

}

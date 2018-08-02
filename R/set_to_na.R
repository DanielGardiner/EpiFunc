#' Set values to NA s
#'
#' @import tidyverse
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param data a data.frame
#' @param values.to.set.to.na a character vector specifying values to set to NA
#'
#' @return a dataframe with specified values set to NA
#'
#' @export
#'
#' @description This function takes a data.frame and sets specified values to NA
#'
#' @examples
#' # set dummy data
#'
#' set.seed(2)
#'
#' data = data.frame(dates = sample(seq(as.Date('2014-01-01'), as.Date('2016-04-01'), by="day"), 200, replace = TRUE),
#'                   sex = c("Male", "Female"),
#'                   conf = sample(c("Confirmed", "Probable", "possible"), 200, replace = TRUE),
#'                   status = sample(c("Student", "Staff"), 200, replace = TRUE))
#'
#' # apply function
#'
#' set_to_na(data, c("Male", "Student"))
set_to_na = function(data, values.to.set.to.na){
  data %>%
    lapply(function(x)
      replace(x, tolower(x) %in% tolower(values.to.set.to.na), NA)) %>%
    as_tibble()
}





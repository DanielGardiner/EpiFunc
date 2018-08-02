#' Set NAs to a value
#'
#' @import tidyverse
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param data a data.frame
#' @param value.to.set.na.to a character vector specifying the value to set NA to
#' @export
#' @examples
#' # set dummy data
#'
#' set.seed(4)
#'
#' data = data.frame(dates = sample(seq(as.Date('2014-01-01'), as.Date('2016-04-01'), by="day"), 20, replace = TRUE),
#'                   sex = factor(c("Male", "Female", "Female", NA)),
#'                   conf = factor(sample(c("Confirmed", "Probable", "Possible"), 20, replace = TRUE)),
#'                   status = sample(c("Student", "Staff", NA), 20, replace = TRUE),
#'                   age = sample(c(1:20, NA), 20, replace = TRUE),
#'                   stringsAsFactors = FALSE)
#'
#' # apply function
#'
#' set_na_to(data, c("Unknown"))
set_na_to = function(data, value.to.set.na.to){
  data %>%
    lapply(function(x){
      if(class(x) == "character"){
        replace(x, is.na(x), value.to.set.na.to)
      } else if(class(x) == "factor" & any(is.na(x))){
        temp = levels(x)
        x = as.character(x)
        x = replace(x, is.na(x), value.to.set.na.to)
        x = factor(x, c(temp, value.to.set.na.to))
        x
      } else {
        x
      }
    }) %>%
    as_tibble()
}



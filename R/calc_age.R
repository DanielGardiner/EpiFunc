#' Calculate age in years
#'
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param dob a vector of dates (should be date of births)
#' @param ref.date a vector of dates (should be the reference date to calculate age)
#' @return a numeric vector containing age in years
#' @export
#' @examples
#' # set dummy data
#'
#' # set.seed(2)
#'
#' data = data.frame(date.of.birth = sample(seq(as.Date('1900-01-01'), as.Date('1990-12-31'), by="day"), 30, replace = TRUE),
#'                   onset.date = sample(seq(as.Date('1991-01-01'), as.Date('2010-12-31'), by="day"), 30, replace = TRUE))
#'
#' # use function
#'
#' calc_age(dob = data$date.of.birth,
#'          ref.date = data$onset.date)
calc_age = function (dob, ref.date) {
  age = floor(as.numeric((ref.date - dob)/365.25))
  return(age)
}

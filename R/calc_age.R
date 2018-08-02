#' Calculate ages in years and age groups
#'
#' @import stringr
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param dob a vector of dates (should be date of births)
#' @param ref.date a vector of dates (should be the reference date to calculate age)
#' @param breaks a numeric vector defining break points for age groups (the final value must be infinite)
#' @return a data.frame containing a column of ages in years and a column of age groups
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
#'
#' # append results of function onto original data
#'
#' cbind(data, calc_age(dob = data$date.of.birth,
#'                      ref.date = data$onset.date,
#'                      breaks = c(0, 10, 20, 30, 40, 50, 60, Inf)))
calc_age = function(dob, ref.date, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, Inf)){

  # check last value of breaks is infinite

  if(!is.infinite(breaks[length(breaks)])){
    stop("the last value of the breaks argument must be Inf")
  }

  # define a function to format age groups

  format_agegrp = function(agegrp){

    agegrp = as.character(agegrp)

    agegrp = str_replace(agegrp, "\\[", "")

    agegrp = str_replace(agegrp, "\\]", "")

    agegrp = str_replace(agegrp, "\\(", "")

    agegrp = str_replace(agegrp, "\\)", "")

    temp = as.data.frame(str_split(agegrp, ",", simplify = TRUE),
                         stringsAsFactors = FALSE)

    temp$V2 = as.character(as.numeric(temp$V2) - 1)

    temp$V3 = apply(temp, 1, function(x) paste0(x[1], "-", x[2]))

    temp$V3 = str_replace(temp$V3, "-Inf", "+")

    temp$V3

  }

  # calculate number of years between dob and ref.date

  age = floor(as.numeric((ref.date - dob)/365.25))

  # calculate agegrp levels

  agegrp.levels = unique(format_agegrp(cut(1:20000, breaks, include.lowest = T, right = FALSE)))

  # cut age into groups, apply function to format age groups and apply levels

  agegrp = factor(format_agegrp(cut(age, breaks, include.lowest = T, right = FALSE)),
                  levels = agegrp.levels)

  # output results as data.frame

  return(data.frame(age, agegrp))

}


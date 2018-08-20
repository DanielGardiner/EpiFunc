#' Calculate age groups in years
#'
#' @import stringr
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param age a numeric vector of ages
#' @param breaks a numeric vector defining break points for age groups (the final value must be infinite)
#' @return a factor vector containing age groups in years
#' @export
#' @examples
#' # set dummy data
#'
#' set.seed(2)
#'
#' age = sample(0:120, 30, replace = TRUE)
#'
#' # use function
#'
#' calc_agegroup(age , breaks = c(0, 20, 40, 60, Inf))
calc_agegroup = function (age, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, Inf)) {
  format_agegrp = function(agegrp) {
    agegrp = as.character(agegrp)
    agegrp = str_replace(agegrp, "\\[", "")
    agegrp = str_replace(agegrp, "\\]", "")
    agegrp = str_replace(agegrp, "\\(", "")
    agegrp = str_replace(agegrp, "\\)", "")
    temp = as.data.frame(str_split(agegrp, ",", simplify = TRUE),
                         stringsAsFactors = FALSE)
    temp$V2 = as.character(as.numeric(temp$V2) - 1)
    temp$V3 = apply(temp, 1, function(x) paste0(x[1], "-",
                                                x[2]))
    temp$V3 = str_replace(temp$V3, "-Inf", "+")
    temp$V3
  }
  agegrp.levels = unique(format_agegrp(cut(1:20000, breaks,
                                           include.lowest = TRUE,
                                           right = FALSE)))
  agegrp.levels = ifelse(agegrp.levels == "NA-NA", NA, agegrp.levels)
  agegrp = factor(format_agegrp(cut(age, breaks,
                                    include.lowest = TRUE,
                                    right = FALSE)),
                  levels = agegrp.levels)

  return(agegrp)
}

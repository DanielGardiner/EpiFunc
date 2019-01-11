#' Transpose a data.frame
#'
#' @import tidyverse
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param data a data.frame
#' @return a data.frame
#' @export
#' @examples
#' data = data.frame(geog = c("A", "B", "C", "D"),
#'                   count = c(43, 21, 19, 44),
#'                   rate = c(1.4, 2.1, 2.0, 3.3))
#'
#' trans(data)
trans = function(data){
  temp = data %>%
    t() %>%
    data.frame() %>%
    rownames_to_column()

  colnames(temp) = temp[1, ] %>%
    mutate_all(funs(as.character(.))) %>%
    as.character()

  temp = temp[-1, ]

  temp = temp %>%
    data.frame()

  for(i in 1:ncol(temp)){

    attr(temp[, i], "names") = NULL

  }

  temp

}


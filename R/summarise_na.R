#' Summarise NA values
#'
#' @import tidyverse
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param data a data.frame
#' @return a data.frame
#' @export
#' @examples
#' data = data.frame(geog = c("A", "B", "C", "D", NA, "E"),
#'                   count = c(43, 21, 19, 44, NA, NA),
#'                   rate = c(1.4, 2.1, 2.0, 3.3, 4.1, 1.0))
#'
#' summarise_na(data, plotit = TRUE)
#'
#' summarise_na(data)
summarise_na = function(data, plotit = FALSE){

  table = data %>%
    summarise_all(funs(sum(is.na(.)))) %>%
    mutate(variable = "n.missing") %>%
    select(variable, everything()) %>%
    trans() %>%
    mutate(n.missing = as.numeric(as.character(n.missing))) %>%
    arrange(desc(n.missing)) %>%
    mutate(percent.missing = as.numeric(n_decimals(100*n.missing/nrow(data), 1)),
           variable = factor(variable, variable))

  if(plotit){

    fig = ggplot(table %>%
                   mutate(percent.missing = as.numeric(percent.missing)),
                 aes(x = variable, y = percent.missing)) +
      geom_bar(stat = "identity", colour = "black", fill = "#00B092") +
      theme_bw() +
      coord_flip()

    return(list(table = table, fig = fig))

  } else {

    return(table)

  }

}


#' Summarise NA values
#'
#' @import tidyverse
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param data a data.frame
#' @param arrange.by.value a logical specifying whether to arrange by value
#' @param plotit a logical specifying whether to plot the output
#' @return either (1) a data.frame or (2) a list including a data.frame and a ggplot figure
#'
#' @export
#' @examples
#'
#' data = data.frame(geog = c("A", "B", "C", "D", NA, "E", "F", NA),
#'                   count = c(43, 21, 19, 44, NA, NA, 11, NA),
#'                   rate = c(1.4, 2.1, 2.0, 3.3, 4.1, 1.0, 3.1, 0.4))
#'
#' summarise_na(data)
#'
#' summarise_na(data, arrange.by.value = FALSE)
#'
#' summarise_na(data, plotit = TRUE)
summarise_na = function(data, arrange.by.value = TRUE, plotit = FALSE){

  table = data %>%
    summarise_all(funs(sum(is.na(.)))) %>%
    mutate(variable = "n.missing") %>%
    select(variable, everything()) %>%
    trans() %>%
    mutate(n.missing = as.numeric(as.character(n.missing)),
           percent.missing = as.numeric(n_decimals(100*n.missing/nrow(data), 1))) %>%
    mutate(variable = factor(variable, variable))

  if(arrange.by.value){

    table = table %>%
      arrange(desc(n.missing)) %>%
      mutate(variable = factor(variable, variable))

  } else {

    NULL

  }

  if(plotit){

    fig = ggplot(table %>%
                   mutate(percent.missing = as.numeric(percent.missing)),
                 aes(x = variable, y = percent.missing)) +
      geom_bar(stat = "identity", colour = "black", fill = "#00B092") +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
      theme_bw() +
      coord_flip()

    return(list(table = table, fig = fig))

  } else {

    return(table)

  }

}


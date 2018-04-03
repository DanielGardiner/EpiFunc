#' Tile plot
#'
#' @import ggplot2
#' @import reshape2
#' @import scales
#' @import dplyr
#' @importFrom stats xtabs
#'
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param data a data frame
#' @param x.col a character containing the column to be used along the x-axis of the tile plot
#' @param y.col a character containing the column to be used along the y-axis of the tile plot
#' @param x.lab a character containing the label for the x-axis
#' @param y.lab a character containing the label for the y-axis
#' @param text a logical specifying if numeric values should be overlayed as text
#' @param colour a character containing the colour for the tile plot
#' @param angle a numeric to specify the x-axis label angel for the epicurve
#' @param label.breaks a numeric specifying x-axis label breaks
#' @param rescale.by.row a logical specifying if the colours on the tile plot should be relative to all tiles or relative to other tiles within a single row
#' @param keep.row.order a logical specifying if the order of the rows should be kept as is or changed to most common
#'
#'
#' @return a tile plot
#' @export
#'
#'
#' @examples
#' # create dummy data
#'
#' set.seed(3)
#'
#' data = data.frame(geog = sample(c("Vienna", "Vienna", "Vienna", "Vienna",
#'                                   "Salzburg", "Innsbruck", "Graz", "Graz",
#'                                   "Linz", "Klagenfurt", "Villach"), 5000, replace = TRUE),
#'                   age.group = sample(c("0-19", "20-39", "40-59", "60+", "60+"), 5000, replace = TRUE),
#'                   week = factor(sample(paste0("week.", 1:30), 5000, replace = TRUE),
#'                                 levels = paste0("week.", 1:30)))
#'
#' # example plot 1
#'
#' tile_plot(data, x.col = "week", y.col = "geog", keep.row.order = TRUE)
#'
#' # example plot 2
#'
#' tile_plot(data, x.col = "week", y.col = "geog", keep.row.order = FALSE)
#'
#' # example plot 3
#'
#' tile_plot(data, x.col = "week", y.col = "geog", text = TRUE)
#'
#' # example plot 4
#'
#' tile_plot(data, x.col = "week", y.col = "geog", text = TRUE, label.breaks = 2)
#'
#' # example plot 5
#'
#' tile_plot(data, x.col = "week", y.col = "geog", text = TRUE, label.breaks = 2,
#'           rescale.by.row = TRUE)
tile_plot = function(data, x.col, y.col, x.lab = "", y.lab = "", text = FALSE,
                     colour = "red", angle=0, label.breaks = 0, rescale.by.row = FALSE,
                     keep.row.order = FALSE){

  # convert to data frame

  data = as.data.frame(data)

  # assign x and y columns

  data$y = data[, y.col]

  data$x = data[, x.col]

  # tabulate cases  x by y

  temp = as.data.frame.array(xtabs(~ y + x, data,
                                   drop.unused.levels = FALSE))

  # order data

  if(keep.row.order){
    NULL
  } else {
    temp = temp[order(apply(temp, 1, sum)), ]
  }

  # add y column

  temp$y = row.names(temp)

  temp.factor = rev(unique(temp$y))

  # melt the dataset

  temp = melt(temp, id.vars = "y")

  temp$y = factor(temp$y,
                  levels = temp.factor)

  temp$rescale = temp$value

  if(rescale.by.row)  temp = temp %>% group_by(y) %>% mutate(rescale = rescale(value))

  # plot temp

  p = ggplot(temp, aes(x = variable, y = y, size = rescale, fill = rescale))

  p = p + geom_tile(colour = "white", size = 1)

  if(text) p = p + geom_text(aes(label = value), vjust = 0.4, size = 4, colour = "black")

  p = p + ylab(y.lab)

  p = p + xlab(x.lab)

  p = p + theme(title = element_text(size = 16, colour = "black", face="bold"),
                axis.text.x = element_text(angle = angle, hjust = 1, vjust = 0, size = 10,
                                           colour = "black"),
                axis.text.y = element_text(hjust = 1, size = 9,
                                           colour = "black"),
                legend.text= element_text(hjust = 1, size = 16,
                                          colour = "black", face="bold"),
                axis.title = element_text(size=16, face="bold"),
                strip.text.y = element_text(hjust = 1, size = 16,
                                            colour = "black", face="bold"),
                legend.position="bottom",
                panel.border = element_rect(colour = "grey", fill = NA),
                panel.background = element_blank())

  p = p + scale_fill_continuous(low = "white", high = colour)

  p = p + scale_x_discrete(breaks = levels(temp$variable)[c(T, rep(F, label.breaks))],
                           drop=FALSE)

  p = p + theme(legend.position="none")

  p

}


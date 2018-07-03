#' Create age-sex pyramid
#'
#' @import scales
#' @import ggplot2
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param data a data.frame
#' @param age.grp.col a character defining the age-group column within the data.frame
#' @param sex.col a character defining the sex column within the data.frame
#' @param lower.limit a numeric defining the lower limit for the x-axis
#' @param upper.limit a numeric defining the upper limit for the x-axis
#' @param split.by a character defining a column to facet by within the data.frame
#' @param col.pal a numeric defining the colour palette to use
#' @param blank.background a logical  specifying if the figure background should be blank
#'
#' @return an age-sex pyramid
#' @export
#'
#' @examples
#' # set dummy data
#'
#' set.seed(5)
#'
#' data = data.frame(sex = sample(c("Male", "Female", "Unknown"), 200, replace = TRUE),
#'                   age = sample(c(NA, 1:100), 200, replace = TRUE),
#'                   status = sample(c("Confirmed", "Probable"), 200, replace = TRUE))
#'
#' data$age.grp = cut(as.numeric(data$age), breaks = c(0, 5, 15, 25, 45, 65, Inf),
#'                    include.lowest = TRUE)
#'
#' # age sex pyramid
#'
#' age_sex_pyramid(data, age.grp.col = "age.grp", sex.col = "sex", split.by = NULL)
#'
#' # age sex pyramid with lower/upper limit and colour palette
#'
#' age_sex_pyramid(data, age.grp.col = "age.grp", sex.col = "sex", split.by = NULL,
#'                 lower.limit = -50, upper.limit = 40, col.pal = 4)
#'
#' # age sex pyramid with facet
#'
#' age_sex_pyramid(data, age.grp.col = "age.grp", sex.col = "sex", split.by = "status")
age_sex_pyramid = function(data, age.grp.col, sex.col,
                           lower.limit = NULL, upper.limit = NULL,
                           split.by = NULL,
                           col.pal = 1, blank.background = FALSE) {


  # make sure data is a data.frame

  data = as.data.frame(data)

  # check values supplied to col.pal and time.period arguments

  if(!(col.pal == "phe" | (col.pal >= 0 & col.pal <= 8))) {

    col.pal = "phe"

    warning("col.pal must either be an integer from 1 to 8 or 'phe',
            setting col.pal='phe'")
  }

  # assign age.grp and sex columns within the function

  data$age.grp = data[, age.grp.col]

  data$sex = data[, sex.col]

  data$split.by = data[, split.by]

  if(is.null(split.by)) data$split.by = "dummy"

  # format sex column

  data$sex = as.character(data$sex)

  data$sex[grep("^M", toupper(data$sex))] = "Male"

  data$sex[grep("^F", toupper(data$sex))] = "Female"

  data = data[!is.na(data$sex) & data$sex %in% c("Male", "Female"), ]

  data$sex = factor(data$sex,
                    levels = c("Male", "Female"))


  # make table of age.grp vs sex

  table.to.plot = as.data.frame(xtabs(~ age.grp + sex + split.by,
                                      data))

  # create axis limits to ensure vertical symmetry (using additional 10%)

  if(is.null(lower.limit)) lower.limit = round(-max(table.to.plot$Freq)*1.1, 0)

  if(is.null(upper.limit)) upper.limit = round(max(table.to.plot$Freq)*1.1, 0)

  temp.limits = c(lower.limit, upper.limit)

  # create pretty breaks

  temp.breaks = pretty(temp.limits)

  # plot data

  p = ggplot(data = table.to.plot,
             aes(x = age.grp, y = Freq, fill = sex))


  p = p + geom_bar(data = subset(table.to.plot, sex=="Female"),
                   stat = "identity", colour = "black")

  p = p + geom_bar(data = subset(table.to.plot, sex=="Male"),
                   stat = "identity",
                   position = "identity",
                   mapping = aes(y = -Freq), colour = "black")


  p = p + scale_y_continuous(labels = abs, limits = temp.limits, breaks = temp.breaks)

  p = p + coord_flip()

  # add the phe colour palette or a generic colour palette

  if(col.pal == "phe"){

    phe.cols = c("#822433", "#00B092", "#002776", "#EAAB00", "#8CB8C6",
                 "#E9994A",  "#00A551", "#A4AEB5", "#00549F", "#DAD7CB")

    p = p + scale_fill_manual(values = phe.cols, drop = FALSE)

  } else if(!is.null(col.pal)){

    p = p + scale_fill_brewer(type = "qual",
                              palette = col.pal, drop = FALSE)

  } else {

    NULL

  }

  p = p + xlab("Age group")

  p = p + ylab("count")

  p = p + theme(title = element_text(size = 16, colour = "black", face="bold"),
                axis.text.x = element_text(angle = 0, hjust = 1, size = 16,
                                           colour = "black"),
                axis.text.y = element_text(hjust = 1, size = 16,
                                           colour = "black"),
                legend.text= element_text(hjust = 1, size = 16,
                                          colour = "black", face="bold"),
                axis.title = element_text(size=16, face="bold"),
                strip.text.y = element_text(hjust = 1, size = 16,
                                            colour = "black", face="bold"),
                legend.position="bottom")

  if(!is.null(split.by)) p = p + facet_grid("split.by~.",
                                            drop = FALSE)

  # remove background if specified in blank.background argument

  if(blank.background){

    p = p + theme(panel.background = element_blank())

  } else {

    NULL

  }

  return(p)

}

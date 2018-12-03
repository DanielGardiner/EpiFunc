#' Tabulate 1 variable
#'
#' @import tidyverse
#' @import stringr
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param data a data.frame
#' @param var a character specifying the variable to tabulate, if set to NULL the first variable will be used
#' @param by a character specifying the variable to stratify by, if set to NULL no stratification will be used
#' @param complete a logical specifying whether to use all levels for factor variables
#' @param arrange.factor.by a character with value either "level" or "value" describing how a factor variable should be ordered
#' @param show.percentage a character either 'column' or 'row' or NULL to indicate whether to show percentages in output
#' @param show.na.percentage a logical specifying whether to show percentages for NA values
#' @param n.decimals a numeric specifying the number of decimal places to show
#' @param trunc.length a numeric specifying the maximum character length to be shown in the output
#' @param plotit a logical specifying whether to plot the output
#'
#' @export
#' @examples
#' # set dummy data
#'
#' set.seed(4)
#'
#' data = data.frame(onset.date = sample(seq(as.Date("2017-01-01"), as.Date("2018-06-01"), 1), 200, replace = TRUE),
#'                  sex = factor(c("M", "M", "F", NA, NA), c("F", "M", "Unk")),
#'                  conf = sample(c("Confirmed", "Probable", "Probable"), 200, replace = TRUE),
#'                  status = sample(c("Student", "Staff", NA), 200, replace = TRUE),
#'                  geog = sample(c("South", "North", NA), 200, replace = TRUE))
#'
#' # apply function
#'
#' tab_var(data, var = "sex")
#'
#' # using dplyr syntax
#'
#' data %>%
#'  tab_var(var = "sex", by = "conf")
#'
#' data %>%
#'  tab_var("geog", "conf", show.percentage = NULL)
#'
#' data %>%
#'  tab_var("geog", "conf",
#'           show.percentage = "row",
#'           n.decimals = 2,
#'           plotit = TRUE)
#'
tab_var = function(data,
                   var = NULL,
                   by = NULL,
                   complete = FALSE,
                   arrange.factor.by = "value",
                   show.percentage = "column",
                   show.na.percentage = TRUE,
                   n.decimals = 0,
                   trunc.length = 60,
                   plotit= FALSE){

  # check arguments are valid

  if(!is.logical(complete)){
    stop("complete must be a logical either: TRUE or FALSE")
  }

  if(!(arrange.factor.by == "value" | arrange.factor.by == "level")){
    stop("arrange.factor.by must be a character either: 'value' or 'level'")
  }

  # convert data to data.frame

  data = as.data.frame(data)

  # if var argument is null use first column and give a dummy variable name

  if(is.null(var)){

    data = as.data.frame(data[, 1])

    colnames(data) = ".var"

    var = ".var"

  } else {

    NULL

  }

  # if by argument is null create a dummy 'by' variable

  if(is.null(by)){

    data$.dummy = ".value"

    by = ".dummy"

  } else {

    NULL

  }

  # check if variable name is same as any values within the variable (if so change)

  while(any(var %in% data[, by])){

    data[, paste0("_", var)] = data[, var]

    var = paste0("_", var)

  }

  # create .var and .by variables to be used within this function

  data$.var = data[, var]

  data$.by = data[, by]

  # if the .by variable is a logical convert to a character

  if(is.logical(data$.by)){

    data$.by[!is.na(data$.by)] = paste0(".", data$.by[!is.na(data$.by)])

  } else {

    NULL

  }

  # produce table and plot for factor, character or logical variables

  if(is.factor(data$.var) | is.character(data$.var) | is.logical(data$.var)){

    # produce raw table

    temp = data[, c(".var", ".by")] %>%
      table(exclude = FALSE) %>%
      data.frame() %>%
      spread(.by, Freq)

    # add column total

    by.values = colnames(temp)[colnames(temp) != ".var"]

    temp = temp %>%
      mutate(.Total = rowSums(select(., by.values), na.rm = TRUE))

    # add row total (including NA)

    temp = rbind(temp,
                 temp %>%
                   mutate_at(vars(c(by.values, ".Total")),
                             funs(sum(., na.rm = TRUE))) %>%
                   head(1) %>%
                   mutate(.var = ".Total"))

    # add row total (excluding NA)

    temp = rbind(temp,
                 temp %>%
                   filter(is.na(.var) | .var == ".Total") %>%
                   mutate_at(vars(c(by.values, ".Total")),
                             funs(ifelse(length(.) == 2,
                                         .[2] - .[1],
                                         .))) %>%
                   head(1) %>%
                   mutate(.var = ".Total.non.na"))


    # if complete is true then leave the data.frame as is

    if(complete){

      NULL

    } else {

      # if complete is false remove variables with no data if complete == FALSE

      temp = temp %>%
        filter(.Total != 0)

    }

    # if show.percentage is null leave data.frame as is, otherwise append the
    # appropriate percentage info onto the data.frame (either column or row percentages)

    if(is.null(show.percentage)){

      NULL

    } else if(show.percentage == "column"){

      if(show.na.percentage){

        # add column percentages (including NA)

        temp = temp %>%
          mutate_at(vars(c(by.values, ".Total")),
                    funs(paste0(., " (",
                                n_decimals(100*./max(., na.rm = TRUE), n.decimals),
                                "%)")))

      } else {

        # add column percentages (excluding NA)

        temp = temp %>%
          mutate_at(vars(c(by.values, ".Total")),
                    funs(paste0(., " (",
                                n_decimals(100*./.[nrow(temp)], n.decimals),
                                "%)"))) %>%
          mutate_at(vars(c(by.values, ".Total")),
                    funs(ifelse(is.na(.var),
                                paste0(str_split(., " ", simplify = TRUE)[, 1],
                                       " (-)"),
                                .))) %>%
          mutate_at(vars(c(by.values, ".Total")),
                    funs(ifelse(.var == ".Total" & !is.na(.var),
                                paste0(str_split(., " ", simplify = TRUE)[, 1],
                                       " (100%)"),
                                .)))

      }

    } else if(show.percentage == "row"){

      if(show.na.percentage){

        # add row percentages (including NA)

        temp[, c(by.values, ".Total")] = temp %>%
          select(c(by.values, ".Total")) %>%
          apply(1, function(x) {
            paste0(x, " (",
                   n_decimals(100*x/max(x, na.rm = TRUE), n.decimals),
                   "%)")
          }) %>%
          t() %>%
          data.frame()

      } else {

        # add row percentages (excluding NA)

        by.values.non.na = by.values[!(by.values %in% "<NA>")]


        temp[, c(by.values.non.na, ".Total")] = temp %>%
          select(c(by.values.non.na, ".Total")) %>%
          apply(1, function(x) {
            paste0(x, " (",
                   n_decimals(100*x/max(x, na.rm = TRUE), n.decimals),
                   "%)")
          }) %>%
          t() %>%
          data.frame()

        if(any(by.values == "<NA>")){

          # replace percentages for NA values with (-)

          temp[, "<NA>"] = paste0(temp[, "<NA>"], " (-)")

        }



      }

    } else {

      stop("show.percentage must either be: NULL, 'column' or 'row'")

    }

    # reorder factor variable according to either level or value

    if(is.factor(data$.var) & arrange.factor.by == "level") {

      NULL

    } else {

      temp = temp %>%
        mutate(.Total2 = str_split(.Total, " ", simplify = TRUE)[ ,1]) %>%
        arrange(desc(.Total2)) %>%
        select(-.Total2)

    }

    # rearrange table so that NA and .Total is at the bottom

    temp.na = temp[is.na(temp$.var), ]

    temp.total = temp[!is.na(temp$.var) & temp$.var == ".Total", ]

    temp.non.na.total = temp[!(is.na(temp$.var) | temp$.var == ".Total"), ]

    temp = rbind(temp.non.na.total, temp.na)

    temp = rbind(temp, temp.total)

    # remove .Total.non.na row

    temp = temp %>%
      filter(.var != ".Total.non.na" | is.na(.var))

    # replace '0 (NaN%)' with '0 (0%)'

    temp = temp %>%
      mutate_all(funs(str_replace_all(., "0 \\(NaN%\\)", "0 (0%)")))

    # if no by variable is supplied remove the .Total column

    if(by == ".dummy"){

      temp = temp %>%
        select(-.Total)

    } else {

      temp

    }

    # truncate values if they exceed trunc.length

    if (is.null(trunc.length)) {

      NULL

    } else {

      longer.than.trunc = nchar(temp$.var) > trunc.length & !is.na(temp$.var)

      temp$.var[longer.than.trunc] = paste0(str_sub(temp$.var[longer.than.trunc],
                                                    1,
                                                    trunc.length),
                                            "...")

    }


    # plot figure

    if(plotit){

      temp.for.plot = temp %>%
        mutate(.var = factor(.var, .var)) %>%
        format_colnames()

      values = colnames(temp.for.plot)[-1]

      title = paste0(var, "_vs_", by)

      title = str_replace(title, "_vs_.dummy", "")

      p = temp.for.plot %>%
        mutate_at(vars(values),
                  funs(as.numeric(str_split(., " ", simplify = TRUE)[, 1]))) %>%
        gather(key, value, -.var) %>%
        filter(.var != ".Total" | is.na(.var)) %>%
        filter(key != ".total") %>%
        ggplot(aes(x = .var, y = value, fill = key)) +
        geom_bar(colour = "black",
                 stat = "identity", position = position_dodge()) +
        xlab(var) +
        ggtitle(title)

      if(by == ".dummy"){

        p = p + theme(legend.position = "none")

      } else {

        NULL

      }

      p

    } else {

      NULL

    }

    # rename .var to original variable name

    colnames(temp)[1] = var


    # produce table and plot for date, POSIXct and POSIXct variables

  } else if(any(class(data$.var) %in% c("Date", "POSIXct", "POSIXt")) |
            is.numeric(data$.var)){

    # produce summary data.frame (this comes out in wide format), then convert
    # to long format

    temp = data %>%
      group_by(.by) %>%
      summarise(n.valid = sum(!is.na(.var)),
                n.NA = sum(is.na(.var)),
                Earliest = min(.var, na.rm = TRUE),
                Median = median(.var, na.rm = TRUE),
                Mean = mean(.var, na.rm = TRUE),
                Latest = max(.var, na.rm = TRUE)) %>%
      t() %>%
      data.frame() %>%
      rownames_to_column() %>%
      mutate_all(funs(as.character(.)))

    colnames(temp) = temp[1, ]

    colnames(temp)[is.na(colnames(temp))] = "<NA>"

    temp = temp[-1, ]

    # append on a .Total column

    if(by == ".dummy"){

      temp$.Total = 1

    } else {

      temp$.Total = data %>%
        summarise(n.valid = sum(!is.na(.var)),
                  n.NA = sum(is.na(.var)),
                  Earliest = min(.var, na.rm = TRUE),
                  Median = median(.var, na.rm = TRUE),
                  Mean = mean(.var, na.rm = TRUE),
                  Latest = max(.var, na.rm = TRUE)) %>%
        t() %>%
        as.character()


    }

    # plot figure with flipped coordinates (for dates)

    p = data %>%
      ggplot(aes(x = .by, y = .var)) +
      geom_boxplot() +
      coord_flip()

    # if var is a numeric rather than date then rename Earliest/Latest to Min/Max
    # and apply the required number of decimal places

    if(is.numeric(data$.var)){

      temp = temp %>%
        mutate_at(vars(colnames(temp)[colnames(temp) != ".by"]),
                  funs(n_decimals(as.numeric(as.character(.)), n.decimals)))

      temp[, 1] = recode(temp[, 1],
                         "Earliest" = "Min",
                         "Latest" = "Max")

      # plot figure without flipped coordinates (for numeric)

      p = data %>%
        ggplot(aes(x = .by, y = .var)) +
        geom_boxplot()

    }

    # add x and y labels and title to figure

    title = paste0(var, "_vs_", by)

    title = str_replace(title, "_vs_.dummy", "")

    p = p +
      xlab(by) +
      ylab(var) +
      ggtitle(title)

    # remove total column and legend from plot if no by variable

    if(by == ".dummy"){

      temp$.Total = NULL

      p = p + theme(legend.position = "none")

    } else {

      NULL

    }

    # rename variable column

    colnames(temp)[1] = var

  } else {

    # give a warning if an unexpected data type is recieved

    stop("Unrecognised data type")

  }

  # return output (either a list containing a table and a figure or just a table)

  if(plotit){

    output = list()

    output[["table"]] = temp

    output[[paste0("fig_", title)]] = p

    return(output)

  } else {

    return(temp)

  }

}

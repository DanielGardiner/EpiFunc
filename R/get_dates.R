#' Get dates
#'
#' @import ISOweek
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param x a vector of dates in date format
#' @param factor.start a character in format "yyyy-mm-dd" specifying the first level of the factor
#' @param factor.end a character in format "yyyy-mm-dd" specifying the last level of the factor
#'
#' @return a dataframe including day, year, quarter, month, year.quarter, 
#         year.month, iso.year, iso.week, iso.year.week  
#' @export 
#'
#' @description This function converts a vector of dates into 
#'              data.frame where each column is a useful date type (year, 
#'              quarter, month, year.quarter, year.month, iso.year, iso.week, 
#'              iso.year.week)
#'
#' @examples
#' # set dummy data
#'
#' set.seed(2)
#' 
#' data = data.frame(date = sample(seq(as.Date("2017-03-01"), as.Date("2017-08-01"), 1), 20, replace = TRUE),
#'                   sex = sample(c("Female", "Male"), 20, replace = TRUE))
#' 
#' 
#' # apply function
#' 
#' get_dates(data$date)
#' 
#' # append results of function to original data.frame
#' 
#' cbind(data, get_dates(data$date))
#' 
#' # apply function, specifying the output should be a factor (providing a start and end date)
#' 
#' get_dates(data$date, factor.start = "2017-01-01", factor.end = "2017-08-31")
#' 
#' # tabulate number of cases by year.month (this includes all year.months over the period defined by the factor.start anf factor.end arguments)
#' 
#' table(get_dates(data$date, factor.start = "2017-01-01", factor.end = "2017-08-31")$year.month)
get_dates = function(x, factor.start = NULL, factor.end = NULL){
  
  # check a date has been supplied 
  
  if(class(x) == "Date"){
    
    NULL
    
  } else{
    
    stop("x is not a date")
    
  } 
  
  
  # generate function to convert vector of dates into a dataframe containing 
  # multiple date types
  # (note: columns are not factors)
  
  non.factor.dates = function(x){  
    
    df = data.frame(day = as.character(x),
                    year = format(x, "%Y"), 
                    month = format(x, "%m"), stringsAsFactors = FALSE)
    
    df$year.month = paste0(df$year, df$month)
    
    df$iso.year = sapply(strsplit(ISOweek(x), "-W"), function(x) x[1])
    
    df$iso.week = sapply(strsplit(ISOweek(x), "-W"), function(x) x[2])
    
    df$iso.year.week = gsub("-W", "", ISOweek(x))
    
    df$quarter = NA
    
    df$quarter[!is.na(df$day)] = sprintf("%02d", ceiling(as.numeric(as.character(df$month[!is.na(df$day)]))/3))
    
    df$year.quarter = paste0(df$year, df$quarter)
    
    df[is.na(df$day), ] = NA
    
    df  
  }
  
  # apply function to generate a data frame of dates
  
  df = non.factor.dates(x)
  
  # convert columns into a factor format ranging from factor.start to factor.end
  # provided by the user (if applicable) 
  
  if(is.null(factor.start) | is.null(factor.end)){
    
    NULL
    
  } else {
    
    temp.date.range = non.factor.dates(seq(as.Date(factor.start), as.Date(factor.end), 1))
    
    for(i in 1:ncol(df)){
      
      df[, i] = factor(df[, i], sort(unique(temp.date.range[, i])))
      
    }
    
    # make sure all columns are set to NA if the date is outside the factor.start
    # and factor.end date
    
    df[is.na(df[, 1]), ] = NA
    
  }
  
  # return output
  
  df
  
}




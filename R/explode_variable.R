#' Explode a single variable into multiple variables 
#'
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param data a data.frame
#' @param id.col a character specifying the column containing ID's
#' @param vars a character vector specifying the columns to 'explode' into multiple seperate columns  
#' @param break.value a character specifying the value to insert as a break between collapsed variables  
#' @return a numeric vector containing age in years
#' @export
#' @examples
#' # define dummy data 
#' 
#' data = data.frame(id = c("A", "B", "C", "D"),
#'                   activities = c("gym, cell 1, workshop a",
#'                                  "workshop a, cell 2, NA",
#'                                  "gym, NA, NA",
#'                                  "NA, NA, NA"))
#' 
#' # apply explode_variable function 
#' 
#' explode_variable(data, vars = "activities")

explode_variable = function(data, 
                            vars,
                            break.value = ", "){
  
  library(EpiFunc)
  # create dummy date column 
  
  data$.id = 1:nrow(data)
  
  # loop over each varibale to explode 
  
  for(i in seq_along(vars)){
    
    # define dummy columns to split values into 
    
    new.cols = paste0(".col", 
                      1:ncol(str_split(data[, vars[i]], 
                                       ", ", 
                                       simplify = TRUE)))
    
    # expand variable into seperate variables 
    
    temp = data %>% 
      separate(vars[i], 
               new.cols, 
               sep = break.value, 
               extra = "drop") %>% 
      select(.id, new.cols) %>% 
      gather(key, value, -.id) %>%
      group_by(.id, value) %>%
      tally() %>%
      ungroup() %>%
      filter(value != "" | is.na(.id)) %>% 
      mutate(n = ifelse(n >= 1, "Yes", "No"),
             value = paste0(vars[i], "_", value)) %>%
      spread(value, n) %>% 
      mutate_all(funs(ifelse(is.na(.), "No", .))) %>% 
      format_colnames()
    
    # merge tabulated data onto original data 
    
    data = left_join(data, temp, by = c(".id" = ".id")) 
    
  }
  
  # remove .id variable 
  
  data$.id = NULL
  
  # output data 
  
  data
  
}



#' Tabulate values for multiple variables and stack ontop of one another 
#'
#' @import tidyverse
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param data a data.frame
#' @param fill.value a value used to represent missing values
#'
#' @return a data.frame 
#' @export
#'
#' @examples
#' # set dummy data
#'
#' set.seed(2)
#'
#' data = data.frame(id = 1:30,
#'                   pizza = sample(c("No", "Yes", NA), 30, replace = TRUE),
#'                   pasta = sample(c("No", "Yes", NA), 30, replace = TRUE),
#'                   salad = sample(c("No", "Yes"), 30, replace = TRUE),
#'                   sex = sample(c("Male", "Female", NA), 30, replace = TRUE))
#'
#' # use function
#'
#' tab_stack_values(data, cols = c("pizza", "pasta", "salad"))
#' 
#' # using dplyr syntax
#' 
#' data %>%
#'   select(pizza, pasta, salad) %>%
#'   tab_stack_values() 
tab_stack_values = function(data, cols = NULL, fill.value = "."){
  
  # convert to data.frame
  
  data = as.data.frame(data)
  
  # if cols argument is supplied then restrict to only those cols
  
  if(is.null(cols)){
    
    NULL
    
  } else {
    
    data = data %>%
      select_(.dots = cols)
    
  }
  
  # identify logical variables in data.frame 
  
  cols.logical = data %>% 
    sapply(class) %>% 
    as.vector() == "logical"
  
  cols.logical = colnames(data)[cols.logical]
    
  # format logical variables into character 
  
  temp = data %>% 
    mutate_at(.vars = cols.logical, 
              .funs = funs(paste0(".", as.character(.)))) %>% 
    mutate_at(.vars = cols.logical, 
              .funs = funs(ifelse(. == ".NA", NA, .))) %>% 
    lapply(function(x) table(x, exclude = FALSE))
  

  # tabulate values for each variable    
  
  for(i in 1:length(temp)){
    temp[[i]] = as.data.frame(temp[[i]]) %>% spread(x, Freq)
    temp[[i]]$.variable = names(temp[i])
  }
  
  # convert list into data.frame 
  
  temp %>% 
    bind_rows() %>% 
    select(.variable, everything()) %>% 
    rename("variable" = ".variable") %>% 
    set_na_to(fill.value)
}

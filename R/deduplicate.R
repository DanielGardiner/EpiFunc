#' Deduplicate records
#'
#' @author Daniel Gardiner (daniel.gardiner@phe.gov.uk)
#'
#' @param data a data.frame
#' @param id.col a character specifying the column containing ID's
#' @param date.col a character specifying the column containing dates
#' @param vars.collapse a character vector specifying the columns to collapse together if there is differing information between records 
#' @param episode.length a numeric specifying the number of days a single episode lasts 
#' @param break.value a character specifying the value to insert as a break between collapsed variables  
#' @return a numeric vector containing age in years
#' @export
#' @examples
#' # define dummy data 
#' 
#' set.seed(7)
#' 
#' data = data.frame(id = c(1, 1, 2, 2, 3, 4, 5, 5, NA, NA),
#'                   date = sample(seq(as.Date("2019-01-01"), 
#'                                     as.Date("2019-03-01"), 
#'                                     by = 1), size = 10),
#'                   spec = c("arm", "leg", "hand", "arm", "head", "hand", "blood", "blood", "arm", "arm"),
#'                   lab = c("Lab A", "Lab D",  "Lab B", "Lab C", "Lab A", "Lab D", "Lab F", "Lab D", "Lab B", "Lab A"))
#' 
#' 
#' # apply deduplicate function
#' 
#' deduplicate(data, 
#'             id.col = "id", 
#'             date.col = "date")
#' 
#' deduplicate(data, 
#'             id.col = "id", 
#'             date.col = "date", 
#'             vars.collapse = "lab")
#' 
#' deduplicate(data, 
#'             id.col = "id", 
#'             date.col = "date", 
#'             vars.collapse = "lab", 
#'             episode.length = 4)
#' 
#' deduplicate(data, 
#'             id.col = "id", 
#'             date.col = "date", 
#'             vars.collapse = "lab", 
#'             episode.length = 4, 
#'             break.value = " | ")

deduplicate = function(data, 
                       id.col,
                       date.col,
                       vars.collapse = "all", 
                       episode.length = Inf, 
                       break.value = ", "){

  # initialise id and date columns 
  
  data = data.frame(data)
  
  data$.id = data[, id.col]
  
  data$.date = data[, date.col]
  
  data[, id.col] = NULL
  
  data[, date.col] = NULL
  
  # create new id column where NA id's are given unique id 
  
  data = data %>% 
    group_by(.id) %>% 
    mutate(id.new = as.character(.id),
           id.new = ifelse(is.na(id.new),
                           paste0(".NA.", 1:n()),
                           id.new))
  
  
  
  # identify difference between dates for each id and 
  # add episodes variable 
  
  data = data %>% 
    arrange(id.new, .date) %>%
    group_by(id.new) %>% 
    mutate(date.diff = c(NA, diff(.date)),
           episode = ifelse(is.na(date.diff),
                            TRUE,
                            date.diff >= episode.length),
           episode = cumsum(episode)) %>% 
    ungroup()
  

  if(vars.collapse == "all"){
    
    # collapse together all variables where there is differing 
    # information between deduplicated records
    
    data = data %>%
      group_by(id.new, episode) %>%
      mutate_at(vars(-id.new, -.date, -date.diff, -episode),
                funs(paste0(unique(.), collapse = break.value)))
    
    
  } else if(vars.collapse == "none"){
    
    # dont collapse together any variables, just keep the information 
    # held in the earliest record 
    
    NULL
    
  } else {
    
    # collapse together variables, defined by user, where there is differing 
    # information between deduplicated records
    
    data = data %>% 
      group_by(id.new, episode) %>% 
      mutate_at(vars(vars.collapse),
                funs(paste0(unique(.), collapse = break.value)))
    
    
  }
  
  # perform deduplication 
  
  data = data %>% 
    filter((date.diff >= episode.length | is.na(date.diff))) %>% 
    ungroup() %>% 
    select(-date.diff) %>% 
    data.frame()
  
  
  # add id.episode column and remove id.new column 
  
  data = data %>% 
    mutate(id.episode = paste0(id.new,
                               ", ep:", episode)) %>% 
    select(-id.new)
  
  # rename variables back to original 
  
  data = data %>% 
    select(.id, .date, episode, id.episode, everything()) 
  
  colnames(data)[1:2] = c(id.col, date.col)
  
  # return deduplicated data
  
  data
  
}
















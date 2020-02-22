# remotes::install_github("r-lib/later")
library(later)
library(lubridate)

run_times <- seq.POSIXt(from = as.POSIXct("2020-02-22 14:10:00"), 
                        to =  as.POSIXct("2020-02-23 14:05:00"), 
                        length.out = 24*12)

check_for_updates <- function(){
  
  file_info <- file.info(list.files("nevada_data/cleaned_timestamped", full.names = TRUE))
  latest_file <- rownames(file_info)[which.max(file_info$mtime)]
  
  
  d <- suppressMessages(read_csv(latest_file, progress = FALSE))
  
  last_precincts_reporting <- d %>% 
    group_by(GEOID10) %>%
    summarize(votes = sum(votes, na.rm = TRUE)) %>%
    filter(votes > 0) %>% nrow()
  
  
  last_total_votes <- sum(d$votes)
  
  precincts <- GET('https://int.nyt.com/applications/elections/2020/data/api/2020-02-22/precincts/NevadaDemPrecinctsGcs-latest.json') %>%
    content()
  
  precincts_reporting <- precincts$meta$precincts_reporting
  total_votes <- precincts$meta$total_votes
  
  
  return(((last_precincts_reporting < precincts_reporting) | (last_total_votes < total_votes)))   
  
}


repeat{
  
  if(round_date(Sys.time(), "minutes") %in% run_times){
    
    new_data <- check_for_updates()
    
    if(new_data){
      
      message("New data found.")
      
      source("./nevada_scripts/nevada_full_analysis_script.R")
      
    } else {
      
      message("No updates.")
      
    }
    
    Sys.sleep(60)
    
    
  }
  
}



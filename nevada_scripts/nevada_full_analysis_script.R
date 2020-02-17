# this script will run all of the necessary error catching functions, caucus math functions, append our Google Sheets comments, and write a csv output

library(tidyverse)
source("nevada_scripts/nevada_scrape_clean.R")
source("nevada_scripts/nevada_error_catching.R")
source("nevada_scripts/nevada_caucus_math_functions.R")

# run the script from "nevada_scripts/nevada_scrape_clean.R" to scrape, clean, and write cleaned data to timestamped CSV
scrape_clean_write()

# read in the latest cleaned timestamped CSV

file_info <- file.info(list.files("nevada_data/cleaned_timestamped", full.names = TRUE))

latest_file <- rownames(file_info)[which.max(file_info$mtime)]

d <- read_csv(latest_file)

# run the dataframe through the error testing and caucus math functions

d <- d %>% 
  find_all_errors() %>% 
  do_caucus_math()

# append the Google Sheets comments


join_comments_and_push <- function(data){
  
  ### Will update this a bit when Creed gives us the update endpoint
  
  test_comment_sheet <- '1ZHW-A8ScqzJyiGl9C3LF5WCj9rck6mgZqcVQYK1HqLk' %>% 
    read_sheet()
  
  file_dir <- ifelse(test_run, "./nevada_data/test/", "./nevada_data/prod/")
  file_name <- paste0("nevada_caucus_data-", strftime(Sys.time(), format = "%Y-%m-%d_%H%M%S"), ".csv")
  
  
  data %>% 
    left_join(comment_sheet, by = c("county", "precinct_full")) %>%
    write_csv(paste0(file_dir, file_name))
  
}

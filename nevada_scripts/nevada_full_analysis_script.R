# this script will run all of the necessary error catching functions, caucus math functions, append our Google Sheets comments, and write a csv output

library(tidyverse)
library(googlesheets4)
library(keyring)
library(aws.s3)
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
  do_caucus_math() %>% 
  join_comments()

# append the Google Sheets comments

## NOTE: since we're adding a tie_winner and tie_loser column to the google sheet, we should also do one last thing before pushing:

# if a given precinct has too_many_del_tie AND the google sheets tie_loser column has a name in it, we should do:

d <- d %>%
  mutate(final_del = case_when(
    candidate == tie_loser &
      game_of_chance == "too_many_del_tie" &
      final_del > 1 ~ final_del - 1,
    TRUE ~ final_del
  ))

# and the same thing for too_few_del_tie

d %>% push()


#### testing all the functions together ####
ds <- read_csv("./iowa_data/cleaned_nyt_results2020-02-06_113324.csv") %>%
  select(county = County, precinct, precinct_full, precinct_delegates, candidate, round, result) %>%
  group_by(precinct_full, candidate) %>%
  pivot_wider(names_from = round, values_from = result) %>%
  rownames_to_column(var = "row_id") %>% mutate(row_id = as.numeric(row_id)) %>% 
  ungroup()

ds <- ds %>% 
  find_all_errors() %>% 
  do_caucus_math()

# check for cases where there's no tie, the delegate totals aren't right, and there's no errors
ds %>% 
  filter(precinct_delegates != total_final_del, game_of_chance == "no_tie") %>% 
  filter(!viable_loss,
         !more_final_votes,
         !nonviable_no_realign,
         final_del > 1)

# check for cases where there's a "too many del" tie but there are fewer tied candidates than the difference (like if there are 2 too many dels and a 2-way tie for farthest, it's clear that they should each lose one)
ds %>% 
  filter(precinct_delegates != total_final_del, game_of_chance == "too_many_del_tie") %>% 
  select(precinct_full, candidate, final_del, total_final_del, precinct_delegates, is_farthest, game_of_chance, how_many_farthest) %>% 
  filter(how_many_farthest < total_final_del - precinct_delegates)

# check the same thing but for "too few del" ties
ds %>% 
  filter(precinct_delegates != total_final_del, game_of_chance == "too_few_del_tie") %>% 
  select(precinct_full, candidate, final_del, total_final_del, precinct_delegates, is_closest, game_of_chance, how_many_closest) %>% 
  filter(how_many_closest < precinct_delegates - total_final_del)


# so there are no cases where total_final_del is not equal to precinct_del
ds %>%
  filter(total_final_del != precinct_delegates,
         game_of_chance == "no_tie",
         total_final_del > 0,
         !viable_loss,
         !more_final_votes,
         !nonviable_no_realign,
         !extra_del_given)

ds %>% filter(game_of_chance != "no_tie") %>% 
  select(precinct_full, candidate, after_rounding, final_del, 
         game_of_chance, total_final_del, precinct_delegates)

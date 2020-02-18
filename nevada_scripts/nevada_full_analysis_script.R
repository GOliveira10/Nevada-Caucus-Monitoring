# this script will run all of the necessary error catching functions, caucus math functions, append our Google Sheets comments, and write a csv output

library(tidyverse)
library(googlesheets4)
library(keyring)
library(aws.s3)
source("nevada_scripts/nevada_scrape_clean.R")
source("nevada_scripts/nevada_error_catching.R")
source("nevada_scripts/nevada_caucus_math_functions.R")

#### run the script from "nevada_scripts/nevada_scrape_clean.R" to scrape, clean, and write cleaned data to timestamped CSV ####
scrape_clean_write()

#### read in the latest cleaned timestamped CSV ####

file_info <- file.info(list.files("nevada_data/cleaned_timestamped", full.names = TRUE))
latest_file <- rownames(file_info)[which.max(file_info$mtime)]

d <- read_csv(latest_file)

#### run the dataframe through the error testing and caucus math functions ####

d <- d %>% 
  find_all_errors() %>% 
  do_caucus_math() %>% 
  join_comments()

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
=======
#### Try to compare THEIR delegate count to OUR delegate count ####

# so we want to flag the delegate counts as different only if there is no tie OR there is a tie BUT we have marked the outcome of the game of chance. So basically if we have a game of chance we don't know the outcome of, it will almost certainly show up as a difference between the reported delegate count and ours. however, if we KNOW the winner/loser and update the final_del to reflect that but it STILL doesn't match the reported delegates, that should be flagged too
d <- d %>% 
  mutate(del_counts_diff = case_when(
    final_del != reported_del &
      (game_of_chance == "no_tie" | (!is.na(tie_winner) | !is.na(tie_loser))) ~ TRUE,
    TRUE ~ FALSE
  ))


#### append the Google Sheets comments ####

# first we need to only select the columns we are planning on reporting to the site
ds %>% 
  select(county, precinct, candidate, precinct_delegates, align1, alignfinal, final_del, reported_del, game_of_chance, viable_loss, nonviable_no_realign, alpha_shift, has_alpha_shift, more_final_votes, fewer_final_votes, del_counts_diff, extra_del_given)



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


### probable column names and descriptions ####
columns <- c("county", "precinct", "candidate", "precinct_delegates", "align1", "alignfinal", "final_del", "reported_del", "game_of_chance", "tie_winner", "tie_loser", "viable_loss", "nonviable_no_realign", "alpha_shift", "has_alpha_shift", "more_final_votes", "fewer_final_votes", "del_counts_diff", "extra_del_given")

descriptions <- c("", "", "", "# of delegates to be given by precinct", "# of votes for candidate in 1st alignment", "# of votes for candidate in final alignment", "our calculated # of delegates earned", "the reported # of delegates earned", "string: type of game of chance required", "string: winner of an extra delegate in a tie", "string: loser of an extra delegate in a tie", "logical: if a candidate was viable in 1st round and lost votes going to final round", "logical: if a nonviable candidate from 1st round did not realign in final round", "string: name of candidate that had alphabetical shift", "logical: alphabetical shift in vote reporting detected", "logical: more votes in final alignment than 1st alignment", "logical: fewer votes in final alignment than 1st. warning, not error", "logical: our delegate counts differ from those reported", "logical: too many delegates given out but all candidates had 1 delegate, so an extra delegate was given")

d <- tibble(colnames = columns, description = descriptions)
d

d %>% 
  pivot_wider(names_from = colnames, values_from = description)
d

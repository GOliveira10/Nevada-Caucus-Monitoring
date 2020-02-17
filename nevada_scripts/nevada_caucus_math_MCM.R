library(tidyverse)
library(googlesheets4)


test_run <- TRUE

ds <- read_csv("nevada_data/test/nevada_caucus_data_input_test.csv")  %>% 
  rename(county = County) %>% mutate(precinct = as.character(precinct))
ds

ds <- read_csv("./iowa_data/cleaned_nyt_results2020-02-06_113324.csv") %>%
  select(-weird) %>% distinct() %>%
  select(county = County, precinct, precinct_full, precinct_delegates, candidate, round, result) %>%
  group_by(precinct_full, candidate) %>%
  pivot_wider(names_from = round, values_from = result) %>%
  rownames_to_column(var = "row_id") %>% mutate(row_id = as.numeric(row_id))

ds

rank_distances <- function(data){
  data %>% 
    group_by(precinct_full) %>% 
    mutate(distance_next = case_when(
      total_final_del != precinct_delegates & viablefinal ~ final_del+1 - caucus_formula_result,
      TRUE ~ NA_real_
    )) %>% 
    mutate(farthest_rank = case_when(
      total_final_del != precinct_delegates & viablefinal & final_del > 1 ~ rank(desc(distance_next), ties.method = "min"),
      TRUE ~ NA_integer_
    )) %>% 
    mutate(closest_rank = case_when(
      total_final_del != precinct_delegates & viablefinal ~ rank(distance_next, ties.method = "min"),
      TRUE ~ NA_integer_
    )) %>% 
    ungroup()
}

find_first_last_ties <- function(data){
  data %>% 
    group_by(precinct_full) %>% 
    mutate(min_far_rank = min(farthest_rank, na.rm = T),
           is_farthest = farthest_rank == min_far_rank,
           min_close_rank = min(closest_rank, na.rm = T),
           is_closest = closest_rank == min_close_rank) %>% 
    mutate(how_many_farthest = sum(is_farthest, na.rm = T),
           how_many_closest = sum(is_closest, na.rm = T)) %>% 
    mutate(game_of_chance = case_when(
      total_del_after_rounding > precinct_delegates & how_many_farthest > 1 ~ "too_many_del_tie",
      total_del_after_rounding < precinct_delegates & how_many_closest > 1 ~ "too_few_del_tie",
      TRUE ~ "no_tie"
    )) %>% 
    ungroup()
}

remove_too_many_dels <- function(data){
  data %>% 
    mutate(final_del = case_when(
      total_final_del > precinct_delegates & is_farthest & final_del > 1 & game_of_chance != "too_many_del_tie" ~ final_del - 1,
      TRUE ~ final_del
    )) %>% 
    group_by(precinct_full) %>% 
    mutate(total_final_del = sum(final_del, na.rm = T)) %>% 
    ungroup()
}

add_too_few_dels <- function(data){
  data %>% 
    mutate(final_del = case_when(
      total_final_del < precinct_delegates & is_closest & game_of_chance != "too_few_del_tie" ~ final_del + 1,
      TRUE ~ final_del
    )) %>% 
    group_by(precinct_full) %>% 
    mutate(total_final_del = sum(final_del, na.rm = T)) %>% 
    ungroup()
}

too_many_process <- function(data){
  data %>% 
    rank_distances() %>% 
    find_first_last_ties() %>% 
    remove_too_many_dels()
}

too_few_process <- function(data){
  data %>% 
    rank_distances() %>% 
    find_first_last_ties() %>% 
    add_too_few_dels()
}

do_caucus_math <- function(data){
  
  data <- data %>% 
    mutate(caucus_formula_result = case_when(
      viablefinal ~ round((alignfinal * precinct_delegates) / total_align1, digits = 4),
      TRUE ~ 0
    )) %>% 
    mutate(after_rounding = round(caucus_formula_result)) %>% 
    group_by(precinct_full) %>% 
    mutate(total_del_after_rounding = sum(after_rounding)) %>% 
    ungroup()
  
  data <- data %>% 
    group_by(precinct_full) %>% 
    mutate(final_del = after_rounding,
           total_final_del = sum(final_del)) %>% 
    ungroup()
  
  data <- data %>% rank_distances() %>% find_first_last_ties()
  
  max_del_diff <- data %>% 
    filter(total_final_del != precinct_delegates,
           game_of_chance == "no_tie",
           total_final_del > 0,
           !viable_loss,
           !more_final_votes,
           !nonviable_no_realign,
           final_del > 1) %>% 
    mutate(del_diff = abs(precinct_delegates - total_final_del)) %>% 
    select(del_diff) %>% 
    max(na.rm = TRUE)
  
  for (i in 1:(max_del_diff+1)) {
    data <- data %>% 
      too_many_process() %>% 
      too_few_process()
  }
  
  data <- data %>% 
    group_by(precinct_full) %>% 
    mutate(extra_del_given = case_when(
      max(final_del, na.rm = T) == 1 & total_final_del > precinct_delegates ~ TRUE,
      TRUE ~ FALSE
    ))
  
  return(data)
}

ds <- ds %>% 
  find_all_errors() %>% 
  do_caucus_math()

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

#   rownames_to_column(var = "row_id") %>% mutate(row_id = as.numeric(row_id))


# IN THE ACTUAL NEVADA DATA: the total # of votes in round 1 will also include all the early voters, so we'll simply have to deal with this as we get the actual data.

## other to-do: figure out how to handle uncommitted votes. do we just filter them out of the final alignment positions?

# I think for uncommitted, we leave them in as 1st round votes, since these contribute to the vote total that's relevant for caucus math, but then after we calculate total 1st round votes we just remove all rows for uncommitted? because I don't think they earn delegates in any sense...

# one other thing I noticed is that there was a case where 0 delegates were given out after rounding, but this case pops out because of an error. there were 4 votes total in a 1 delegate precinct. one candidate had 2 and two candidates had 1 each. the candidate with 2 should have been viable and received the delegate, but the votes in the final alignment didn't reflect this, which is a legit error


ds <- ds %>% 
  group_by(precinct_full) %>% 
  mutate(total_align1 = sum(align1),
         total_alignfinal = sum(alignfinal)) %>% 
  mutate(viability_threshold = case_when(
    precinct_delegates >= 4 ~ ceiling(0.15*total_align1),
    precinct_delegates == 3 ~ ceiling((1/6)*total_align1),
    precinct_delegates == 2 ~ ceiling(0.25*total_align1),
    precinct_delegates == 1 ~ ceiling(0.5*total_align1),
    TRUE ~ NA_real_
  )) %>% 
  mutate(viable1 = align1 >= viability_threshold, viablefinal = alignfinal >= viability_threshold) %>% 
  ungroup()


# with this information we should be able to use all the rules to figure out delegate appointments OTHER THAN the cases where a card draw is needed

# ok I have to double check this but I'm PRETTY sure that the formula results are always rounded to 4 decimals
ds <- ds %>% 
  mutate(caucus_formula_result = case_when(
    viablefinal ~ round((alignfinal * precinct_delegates) / total_align1, digits = 4),
    TRUE ~ 0
  )) %>% 
  mutate(after_rounding = round(caucus_formula_result)) %>% 
  group_by(precinct_full) %>% 
  mutate(total_del_after_rounding = sum(after_rounding)) %>% 
  ungroup()

# we actually need to add a final_del and total_final_del column which will get updated as we proceed through the rules. we don't wanna touch the after_rounding column
ds <- ds %>% 
  group_by(precinct_full) %>% 
  mutate(final_del = after_rounding,
         total_final_del = sum(final_del)) %>% 
  ungroup()


# actually using two different ranking methods since it's just way easier. For "farthest" rank you just don't ever include candidates with 1 delegate, but for the "closest" rank you do



# this is kinda stupid, you could probably do a while loop, but honestly just running the "too many" and "too few" processes a bunch of times in a row essentially does the trick. Instead of updating some while loop a bunch of times, we're just saying "ok I doubt there are cases where there are like 
# ds %>% 
#   too_many_process() %>% 
#   too_many_process() %>% 
#   too_many_process() %>% 
#   too_many_process() %>% 
#   too_few_process() %>% 
#   too_few_process() %>% 
#   too_few_process() %>% 
#   too_few_process() %>% 
#   too_few_process() %>% 
#   filter(total_final_del != precinct_delegates) %>% 
#   select(precinct, candidate, after_rounding, final_del, 
#          precinct_delegates, total_final_del, game_of_chance) %>% 
#   print(n = Inf)

# there's actually a case where 0 delegates were given....

ds %>% 
  filter(precinct == "38")

# oh shit, something is going very very wrong here. I think it's because of errors coming in before doing caucus math. so we gotta include "no prior errors" as part of the caucus math prerequisites

weird_cases <- ds %>% 
  find_all_errors() %>% 
  filter(precinct_delegates != total_final_del) %>% 
  mutate(del_diff = abs(precinct_delegates - total_final_del)) %>% 
  filter(del_diff > 10) %>% 
  select(precinct_full, candidate, after_rounding, precinct_delegates, total_final_del, del_diff, viable_loss, more_final_votes, nonviable_no_realign) %>% 
  arrange(desc(del_diff))

weird_cases

ds %>% 
  filter(precinct_full %in% weird_cases$precinct_full) %>% 
  select(precinct_full, candidate, alignfinal, caucus_formula_result, after_rounding, total_del_after_rounding) %>% 
  print(n = Inf)

max(abs(ds$precinct_delegates - ds$total_final_del), na.rm = T)

ds <- ds %>% find_all_errors() %>% rank_distances() %>% find_first_last_ties()

# ds %>%
#   filter(total_final_del != precinct_delegates,
#          game_of_chance == "no_tie",
#          total_final_del > 0,
#          !viable_loss,
#          !more_final_votes,
#          !nonviable_no_realign) %>%
#   mutate(del_diff = abs(precinct_delegates - total_final_del)) %>%
#   select(del_diff) %>%
#   max(na.rm = TRUE)

max_del_diff <- ds %>% 
  filter(total_final_del != precinct_delegates,
         game_of_chance == "no_tie",
         total_final_del > 0,
         !viable_loss,
         !more_final_votes,
         !nonviable_no_realign,
         final_del > 1) %>% 
  mutate(del_diff = abs(precinct_delegates - total_final_del)) %>% 
  select(del_diff) %>% 
  max(na.rm = TRUE)

for (i in 1:(max_del_diff+1)) {
  ds <- ds %>% 
    too_many_process() %>% 
    too_few_process()
}

ds %>%
  filter(total_final_del != precinct_delegates,
         game_of_chance == "no_tie",
         total_final_del > 0,
         !viable_loss,
         !more_final_votes,
         !nonviable_no_realign,
         final_del > 1) %>%
  mutate(del_diff = abs(precinct_delegates - total_final_del)) %>% 
  select(precinct_full, candidate, alignfinal, caucus_formula_result, after_rounding, total_del_after_rounding, precinct_delegates, del_diff, game_of_chance) %>% 
  arrange(desc(del_diff))
  


while(length(ds$row_id[ds$total_final_del != ds$precinct_delegates & 
                         ds$game_of_chance == "no_tie" & 
                         ds$total_final_del > 0] &
             !ds$viable_loss &
             !ds$more_final_votes &
             !ds$nonviable_no_realign) > 0) {
  ds <- ds %>% 
    too_many_process() %>% 
    too_few_process()
}

ds %>% 
  filter(total_final_del != precinct_delegates) %>% 
    select(precinct, candidate, after_rounding, final_del,
           precinct_delegates, total_final_del, game_of_chance) %>%
    print(n = Inf)

## let's add in a thing where we have a google sheet where we can put comments for each precinct (like explanations for what happened in a given precinct), and then we read that in here and then left_join it to our data before we write the full output csv
## https://docs.google.com/spreadsheets/d/1ZHW-A8ScqzJyiGl9C3LF5WCj9rck6mgZqcVQYK1HqLk/edit?usp=sharing

comment_sheet <- '1ZHW-A8ScqzJyiGl9C3LF5WCj9rck6mgZqcVQYK1HqLk' %>% 
  read_sheet()

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

library(tidyverse)

ds <- read_csv("iowa_data/iowa_data_subset_for_testing.csv") %>% 
  rownames_to_column(var = "row_id") %>% mutate(row_id = as.numeric(row_id))
# IN THE ACTUAL NEVADA DATA: the total # of votes in round 1 will also include all the early voters, so we'll simply have to deal with this as we get the actual data.

## other to-do: figure out how to handle uncommitted votes. do we just filter them out of the final alignment positions?

# I think for uncommitted, we leave them in as 1st round votes, since these contribute to the vote total that's relevant for caucus math, but then after we calculate total 1st round votes we just remove all rows for uncommitted? because I don't think they earn delegates in any sense...

# one other thing I noticed is that there was a case where 0 delegates were given out after rounding, but this case pops out because of an error. there were 4 votes total in a 1 delegate precinct. one candidate had 2 and two candidates had 1 each. the candidate with 2 should have been viable and received the delegate, but the votes in the final alignment didn't reflect this, which is a legit error


ds <- ds %>% 
  group_by(precinct_full) %>% 
  mutate(votes_align1 = sum(align1)) %>% 
  mutate(viability_threshold = case_when(
    precinct_delegates >= 4 ~ round(0.15*votes_align1),
    precinct_delegates == 3 ~ round((1/6)*votes_align1),
    precinct_delegates == 2 ~ round(0.25*votes_align1),
    precinct_delegates == 1 ~ round(0.5*votes_align1),
    TRUE ~ NA_real_
  ))%>% 
  mutate(viable1 = align1 >= viability_threshold, viablefinal = alignfinal >= viability_threshold) %>% 
  ungroup()


# with this information we should be able to use all the rules to figure out delegate appointments OTHER THAN the cases where a card draw is needed

ds <- ds %>% 
  mutate(caucus_formula_result = case_when(
    viablefinal ~ (alignfinal * precinct_delegates) / votes_align1,
    TRUE ~ 0
  )) %>% 
  #mutate(formula_decimal = round(caucus_formula_result - floor(caucus_formula_result), 4)) %>% 
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

# this is kinda stupid, you could probably do a while loop, but honestly just running the "too many" and "too few" processes a bunch of times in a row essentially does the trick. Instead of updating some while loop a bunch of times, we're just saying "ok I doubt there are cases where there are like 
ds2 %>% 
  too_many_process() %>% 
  too_many_process() %>% 
  too_many_process() %>% 
  too_many_process() %>% 
  too_few_process() %>% 
  too_few_process() %>% 
  too_few_process() %>% 
  too_few_process() %>% 
  too_few_process() %>% 
  filter(total_final_del != precinct_delegates) %>% 
  select(precinct, candidate, after_rounding, final_del, 
         precinct_delegates, total_final_del, game_of_chance) %>% 
  print(n = Inf)

# there's actually a case where 0 delegates were given....

ds %>% 
  filter(precinct == "38")

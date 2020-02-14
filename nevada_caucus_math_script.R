library(tidyverse)

# the goal here is to essentially calculate the full table here in the examples here https://nvdems.com/wp-content/uploads/2020/02/Caucus-Memo_-Delegate-Count-Scenarios-and-Tie-Breakers.pdf

# Then we'll delineate each of the specific rules for appointing delegates to determine delegate appointments, and where possible determine the final number of delegates for each candidate in each precinct.

# In the case of an actual TIE, where there is a card draw to determine who loses a delegate, we will determine that ONE of the tied candidates has their delegate appointment shift by 1, and neither of the other candidates shifts at all. We will also flag these cases as those where a card draw should have taken place, so we can check manually

# let's just use a subset of some Iowa data to test

ds <- read_csv("iowa_data/iowa_data_subset_for_testing.csv")

# calculate total votes and figure out thresholds based on delegate counts
# IN THE ACTUAL NEVADA DATA: the total # of votes in round 1 will also include all the early voters, so we'll simply have to deal with this as we get the actual data


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

ds %>% 
  print(n = Inf)

# now calculate the rest of the columns in these little tables: https://nvdems.com/wp-content/uploads/2020/02/Caucus-Memo_-Delegate-Count-Scenarios-and-Tie-Breakers.pdf


# with this information we should be able to use all the rules to figure out delegate appointments OTHER THAN the cases where a card draw is needed

ds <- ds %>% 
  mutate(caucus_formula_result = case_when(
    viablefinal ~ (alignfinal * precinct_delegates) / votes_align1,
    TRUE ~ 0
  )) %>% 
  #select(precinct_full, candidate, viablefinal, caucus_formula_result) %>% 
  #mutate(caucus_formula_result = (alignfinal * precinct_delegates) / votes_align1) %>% 
  mutate(formula_decimal = round(caucus_formula_result - floor(caucus_formula_result), 4)) %>% 
  mutate(after_rounding = round(caucus_formula_result)) %>% 
  group_by(precinct_full) %>% 
  mutate(total_del_after_rounding = sum(after_rounding, na.rm = TRUE)) %>% 
 # filter(precinct_delegates != total_del_after_rounding) %>% This clause breaks scenario #1 down there
  ungroup()


#### delegate appointment scenarios ####

# 1) number of delegates after rounding is equal to number of precinct delegates: no further action required

do_nothing <- ds %>% 
  filter(total_del_after_rounding == precinct_delegates, viablefinal) %>% 
  select(precinct_full, candidate, viablefinal, caucus_formula_result, after_rounding, total_del_after_rounding, precinct_delegates,final_del = after_rounding, total_final_del = total_del_after_rounding) %>% # duplicate columns to align with processed data
mutate(game_of_chance = FALSE)

### ok there is a precinct in here where total delegates after rounding is 2 higher than precinct delegates

ds %>% 
  filter(precinct == "ANK 05") %>% 
  select(County, precinct, candidate, alignfinal, votes_align1, viablefinal, caucus_formula_result, after_rounding, total_del_after_rounding, precinct_delegates)


# 2) number of delegates after rounding is HIGHER than number of precinct delegates: need to calculate how far the formula result is from after_rounding + 1. Then figure out which candidate is FARTHEST from after_rounding + 1:
  # a) if there is a decimal tie, then a game of chance occurs. We should just flag these scenarios and check to see that out of the tied candidates, only 1 of them has exactly 1 delegate taken away
  # b) if this candidate only has 1 delegate, then NO delegates are lost and you actually give out precinct_delegates + 1
  # c) if the farthest candidate has 2 or more delegates, then subtract 1 delegate from this candidate


too_many_dels <- ds %>% 
  filter(total_del_after_rounding > precinct_delegates, viablefinal) %>% 
  mutate(distance_next = ceiling(caucus_formula_result) - caucus_formula_result) %>% 
  group_by(precinct_full) %>% 
  mutate(farthest_rank = rank(desc(distance_next))) %>% # Changed to average ranking
  mutate(game_of_chance = ifelse(farthest_rank %% 1 != 0 & sum(viablefinal) %% delegates_remaining != 0, TRUE, FALSE))  %>% # Follow me on this. If rank with average tie method isn't equally divisible, then that's a tie, and it only matters if the number of remaining delegates can't be equally divided among viable candidates
  ungroup() %>% 
  arrange(precinct_full, farthest_rank)

too_many_dels <- too_many_dels %>% 
  mutate(final_del = case_when(
    farthest_rank == 1 & after_rounding > 1 & !game_of_chance ~ after_rounding - 1, # Don't allocate if game of chance is required, handle later
    TRUE ~ after_rounding
  )) %>% 
  group_by(precinct_full) %>% 
  mutate(total_final_del = sum(final_del)) %>% 
  ungroup() %>% 
  select(precinct_full, candidate, caucus_formula_result, after_rounding, precinct_delegates, 
         total_del_after_rounding, game_of_chance, farthest_rank, final_del, total_final_del)


# 3) number of delegates after rounding is LOWER than the number of precinct delegates: need to calculate how far the formula result is from after_rounding + 1. Then figure out which candidate is CLOSEST to after_rounding + 1:
  # a) if there is only 1 extra delegate, it goes to the CLOSEST candidate
  # b) if there are more than 1 extra delegates, they go in order of descending CLOSENESS and loop back to the CLOSEST if need be
  ### it's never mentioned what happens if there is a TIE for CLOSEST in this scenario- presumably it's a game of chance but it's never explicitly stated


## This function successfully gets the right number of total final delegates in each precinct, seems to allocate correctly when I spot check,
## and correctly solves Principle 3 Example B in the handbook. I think it's good but I can barely explain it to myself lol. Check my logic here.

allocate <- function(viable_candidates, delegates_remaining, farthest_rank){
  
  if (delegates_remaining < viable_candidates) {
    if (farthest_rank %in% seq(from = 1, to = delegates_remaining, by = 1)) {
      return(1)
    } else{
      return(0)
    }
  }
  else{
    divisibility <- delegates_remaining %% viable_candidates
    
    if (divisibility == 0) {
      return(delegates_remaining / viable_candidates)
      
    } else{
      even_split <- floor(delegates_remaining / viable_candidates)
      
      if (farthest_rank <= divisibility) {
        even_split <- even_split + 1
      }
      
      return(even_split)
      
    }
  }
  
}

ds %>% group_by(precinct_full) %>% 
  filter(total_del_after_rounding < precinct_delegates, viablefinal) %>% 
  mutate(distance_next = ceiling(caucus_formula_result) - caucus_formula_result) %>% 
  group_by(precinct_full) %>% 
  mutate(delegates_remaining = precinct_delegates - total_del_after_rounding,
         viable_candidates = sum(viablefinal),
         farthest_rank = rank(desc(distance_next))) %>% # Changed to average ranking
  mutate(game_of_chance = ifelse(farthest_rank %% 1 != 0 & sum(viablefinal) %% delegates_remaining != 0, TRUE, FALSE))  %>% # Follow me on this. If rank with average tie method isn't equally divisible, then that's a tie, and it only matters if the number of remaining delegates can't be equally divided among viable candidates
  ungroup() %>% 
  group_by(precinct_full, candidate) %>%
  mutate(delegates_to_allocate = allocate(viable_candidates, delegates_remaining, farthest_rank)) %>% 
  mutate(final_del = ifelse(!game_of_chance, after_rounding + delegates_to_allocate, after_rounding)) %>%
  group_by(precinct_full) %>%
  mutate(total_final_del = sum(final_del)) %>%
  select(precinct_delegates, after_rounding, final_del, total_del_after_rounding, distance_next, farthest_rank, delegates_to_allocate, delegates_remaining,  total_final_del) %>%
  arrange(precinct_full, farthest_rank) 
  



#### Validate that after splitting all these groups off every viable candidate in every precinct makes it to the end


processed_ds <- bind_rows(do_nothing, too_many_dels) %>% bind_rows(not_enough_dels)

viable_after_filtering <- processed_ds %>% select(precinct_full, candidate) %>% distinct()

viable_original_set <- ds %>% 
  filter(viablefinal) %>% 
  select(precinct_full, candidate) %>% distinct()

viable_original_set %>% anti_join(viable_after_filtering) # all viable candidates present after processing. dope.



#### Games of Chance ####

# Need a function here that updates games of chance as we get results

processed_ds %>%
  group_by(game_of_chance) %>%
  summarize(n())

## Only two rows (one game two candidates) where this is an issue

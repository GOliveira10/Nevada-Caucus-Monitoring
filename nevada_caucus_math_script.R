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

ds %>% 
  filter(viablefinal) %>% 
  mutate(caucus_formula_result = (alignfinal * precinct_delegates) / votes_align1) %>% 
  mutate(formula_decimal = round(caucus_formula_result - floor(caucus_formula_result), 4)) %>% 
  select(precinct_full, precinct_delegates, candidate, alignfinal, caucus_formula_result, formula_decimal) %>% 
  mutate(after_rounding = round(caucus_formula_result)) %>% 
  group_by(precinct_full) %>% 
  mutate(total_del_after_rounding = sum(after_rounding)) %>% 
  filter(precinct_delegates != total_del_after_rounding) %>% # start here
  arrange(precinct_full, desc(caucus_formula_result)) %>%
  mutate(candidate_rank_before_rounding = rank(caucus_formula_result),
         unallocated_delegates = first(precinct_delegates) - first(total_del_after_rounding)) %>%
  group_by(precinct_full, candidate) %>%
  mutate(game_of_chance = ifelse((candidate_rank_before_rounding %% 1 != 0 && unallocated_delegates != 0), TRUE, FALSE)) %>%
  group_by(precinct_full) %>%
  mutate(precinct_game_of_chance = max(game_of_chance)) ## To operate only on cases without game of chance
  

#### delegate appointment scenarios ####

# 1) number of delegates after rounding is equal to number of precinct delegates: no further action required

# 2) number of delegates after rounding is HIGHER than number of precinct delegates: need to calculate how far the formula result is from after_rounding + 1. Then figure out which candidate is FARTHEST from after_rounding + 1:
  # a) if this candidate only has 1 delegate, then NO delegates are lost and you actually give out precinct_delegates + 1
  # b) if the farthest candidate has 2 or more delegates, then subtract 1 delegate from this candidate
  # c) if there is a decimal tie, then a game of chance occurs. We should just flag these scenarios and check to see that out of the tied candidates, only 1 of them has exactly 1 delegate taken away

# 3) number of delegates after rounding is LOWER than the number of precinct delegates: need to calculate how far the formula result is from after_rounding + 1. Then figure out which candidate is CLOSEST to after_rounding + 1:
  # a) if there is only 1 extra delegate, it goes to the CLOSEST candidate
  # b) if there are more than 1 extra delegates, they go in order of descending CLOSENESS and loop back to the CLOSEST if need be
  ### it's never mentioned what happens if there is a TIE for CLOSEST in this scenario- presumably it's a game of chance but it's never explicitly stated





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
  mutate(prop_align1 = round(align1 / votes_align1, digits = 4), 
         prop_alignfinal = round(alignfinal / votes_align1, digits = 4)) %>% 
  mutate(viability_threshold = case_when(
    precinct_delegates >= 4 ~ 0.15,
    precinct_delegates == 3 ~ 1/6,
    precinct_delegates == 2 ~ 0.25,
    precinct_delegates == 1 ~ 0.5,
    TRUE ~ NA_real_
  )) %>% 
  mutate(viable1 = prop_align1 >= viability_threshold, viablefinal = prop_alignfinal >= viability_threshold) %>% 
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
  

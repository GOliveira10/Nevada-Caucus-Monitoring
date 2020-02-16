library(tidyverse)


## Switching to the full dataset cause it's not that big and gives us more test data
ds <- read_csv("./iowa_data/cleaned_nyt_results2020-02-06_113324.csv") %>% 
  select(-weird) %>% distinct() %>%
  select(county = County, precinct, precinct_full, precinct_delegates, candidate, round, result) %>%
  group_by(precinct_full, candidate) %>%
  pivot_wider(names_from = round, values_from = result) %>% 
  rownames_to_column(var = "row_id") %>% mutate(row_id = as.numeric(row_id))

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

ds <- ds %>% 
  mutate(caucus_formula_result = case_when(
    viablefinal ~ round((alignfinal * precinct_delegates) / total_align1, digits = 4),
    TRUE ~ 0
  )) %>% 
  mutate(after_rounding = round(caucus_formula_result)) %>% 
  group_by(precinct_full) %>% 
  mutate(total_del_after_rounding = sum(after_rounding)) %>% 
  ungroup()

ds <- ds %>% 
  group_by(precinct_full) %>% 
  mutate(final_del = after_rounding,
         total_final_del = sum(final_del)) %>% 
  ungroup()

ds
# 1) cannot be viable in round 1 and lose votes going to round 2
ds %>% 
  filter(viable1 & alignfinal < align1)

# 2) cannot be nonviable in round 1 and REMAIN nonviable without votes going to 0. This is because you either combine with another group to BECOME viable, or everyone has to realign. They may, however, realign with uncommitted I think.

ds %>% 
  filter(!viable1 & !viablefinal & alignfinal > 0 & candidate != "uncommitted")

# ok another thing to be clear on: the uncommitted preference CAN receive delegates, BUT it is not subject to the same errors above. well actually I'm not sure. if uncommitted in 1st round is viable, can it become nonviable? It seems like the second error would not apply to uncommitted. Like uncommitted voters in the first round certainly don't have to realign

# 3) alphabetical errors where candidate results are entered in wrong
# Grant I'm gonna let you take this one on

alpha_shift <- ds %>% 
  group_by(precinct_full) %>%
  filter(!candidate %in% c("uncommitted", "other")) %>%
  mutate(align1_lag = lag(align1),
         alignfinal_lag = lag(alignfinal),
         align1_lead = lead(align1),
         alignfinal_lead = lead(alignfinal),
         viable1_lag = lag(viable1),
         viable1_lead = lead(viable1)) %>% 
  select(precinct_full, viablefinal, candidate, align1, 
         alignfinal, align1_lag, viable1_lag,
         alignfinal_lag, align1_lead, 
         alignfinal_lead, viable1_lead) %>%
  ungroup() %>% mutate(diff_sd = sd(alignfinal-align1)) %>%
#  group_by(precinct_full, candidate) %>%
  mutate(alpha_shift = case_when(
    alignfinal-align1 > (3*diff_sd) & align1_lag-alignfinal_lag > (3*diff_sd) & viable1_lag ~ lag(candidate), 
    alignfinal-align1 > (3*diff_sd) & align1_lead-alignfinal_lead > (3*diff_sd) & viable1_lead ~ lead(candidate)
  )) %>% 
  group_by(precinct_full) %>% 
  mutate(has_alpha_shift = max(case_when(!is.na(alpha_shift) ~ TRUE,
                                         is.na(alpha_shift) ~ FALSE), na.rm = TRUE))
  

alpha_shift %>% filter(has_alpha_shift == 1) %>% select(-contains("lag"), -contains("lead"), -diff_sd) %>% view()



# 4) can't have more votes in the final round than in the 1st round

ds %>% 
  filter(total_alignfinal > total_align1)

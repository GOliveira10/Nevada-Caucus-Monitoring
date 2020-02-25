library(tmaptools)


for_vis <- d %>% 
  mutate(final_del = case_when(game_of_chance != "no_tie" & final_del != reported_del_given ~ reported_del_given,
                               TRUE ~ final_del))

for_vis %>% filter(is_complete) %>%
  group_by(candidate) %>%
  summarize(diff = sum(final_del) - sum(reported_del_given)) %>%
  ggplot(aes(x=candidate, y = diff, fill = candidate)) + 
  geom_col(position = "dodge") + coord_flip() + 
  ggtitle("Actual Delegates Awarded - Expected Delegates") +
  theme_minimal() + scale_fill_viridis_d()


for_vis %>% filter(is_complete) %>%
  group_by(candidate) %>%
  summarize(awarded = sum(reported_del_given),
            expected = sum(final_del)) %>%
  ungroup() %>%
  mutate(total = sum(awarded)) %>%
  mutate(`Percent Expected` = expected/total,
         `Percent Awarded` = awarded/total) %>%
  pivot_longer(cols = c(`Percent Awarded`, `Percent Expected`)) %>%
  ggplot(aes(x= candidate, y = value, fill = name)) + 
  geom_col(position = "dodge") + coord_flip() + 
  ggtitle("Percent of Delegates Awarded vs Expected") +
  theme_minimal() + scale_fill_viridis_d(begin = 0, end = 0.4) + 
  scale_y_continuous(labels = scales::percent) +
  geom_hline(aes(yintercept = .15)) + labs(fill = "", 
                                           y = "percent of county delegates")



d %>% 
  filter(is_complete & game_of_chance == "no_tie") %>%
  
  group_by(candidate) %>%
  
  summarize(diff = sum(final_del) - sum(reported_del_given)) 
  ggplot(aes(x=candidate, y = diff, fill = candidate)) + 
  geom_col(position = "dodge") + coord_flip() + 
  ggtitle("Actual Delegates Awarded - Expected Delegates") +
  theme_minimal() + scale_fill_viridis_d()


d %>%
  group_by(candidate) %>%
  select_if(is.logical) %>% select(-c(1:3), -comments, 
                                   -tie_loser, -tie_winner, 
                                   -is_farthest, -is_closest,  -viablefinal) %>%
  summarize_all(sum, na.rm = TRUE) %>% pivot_longer(-1) %>%
  mutate(name = case_when(name == "viable_loss" ~ "Viable Candidate Lost a Delegate \nin Later Rounds",
                          name == "nonviable_no_realign" ~ "Voters for a Nonviable Candidate \nFailed to Realign in Next Round",
                          name == "more_final_votes" ~ "More Final Votes Than Round 1 Votes",
                          name == "fewer_final_votes" ~ "Fewer Final Votes Than Round 1",
                          name == "extra_del_given" ~ "Extra Delegate Was Given to Candidate",
                          name == "del_count_diff" ~ "Difference Between the Number of \nDelegates Given and Number Required \nBy Delegate Math",
                          name == "has_alpha_shift" ~ "Votes appear to have shifted alphabetically \nbetween candidates due to data entry error")) %>%
  ggplot(aes(x = name, y = value, fill = name)) + 
  geom_col(position = "dodge") + coord_flip() + 
  ggtitle("Most Common Error Types (By Occurrence)") +
  theme_minimal() + scale_fill_viridis_d() + theme(legend.position = "bottom") +
  labs(x = "Error", y= "Count", fill = "Error")

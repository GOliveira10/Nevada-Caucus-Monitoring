library(tmaptools)

d %>% filter(is_complete & game_of_chance == "no_tie") %>%
  group_by(candidate) %>%
  summarize(diff = sum(final_del) - sum(reported_del_given)) %>%
  ggplot(aes(x=candidate, y = diff, fill = candidate)) + 
  geom_col(position = "dodge") + coord_flip() + 
  ggtitle("Difference Between Expected Delegates and Actuals") +
  theme_minimal() + scale_fill_viridis_d()


d %>%
  group_by(candidate) %>%
  select_if(is.logical) %>% select(-c(1:3), -comments, 
                                   -tie_loser, -tie_winner, 
                                   -is_farthest, -is_closest, 
                                   -has_alpha_shift, -viablefinal) %>%
  summarize_all(sum) %>% pivot_longer(-1) %>%
  mutate(name = case_when(name == "viable_loss" ~ "Viable Candidate Lost a Delegate \nin Later Rounds",
                          name == "nonviable_no_realign" ~ "Voters for a Nonviable Candidates \nFailed to Realign in Next Round",
                          name == "more_final_votes" ~ "More Final Votes Than Round 1 Votes",
                          name == "fewer_final_votes" ~ "Fewer Final Votes Than Round 1",
                          name == "extra_del_given" ~ "Extra Delegate Was Given to Candidate",
                          name == "del_count_diff" ~ "Difference Between the Number of \nDelegates Given and Number Required \nBy Delegate Math")) %>%
  ggplot(aes(x = name, y = value, fill = name)) + 
  geom_col(position = "dodge") + coord_flip() + 
  ggtitle("Most Common Error Types (By Occurrence)") +
  theme_minimal() + scale_fill_viridis_d() + theme(legend.position = "bottom") +
  labs(x = "Error", y= "Count", fill = "Error")

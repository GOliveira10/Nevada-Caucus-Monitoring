# here are all the caucus math functions

library(tidyverse)

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
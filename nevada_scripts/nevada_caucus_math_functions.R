# here are all the caucus math functions

library(tidyverse)
library(aws.s3)
# the general order of events here is like this:
# 1) in cases where the delegates given do not add up to the delegates for the precinct, rank the candidates based off of their distance to rounding up to the next whole number after rounding
# 2) now go through these rankings and find ties for closest to rounding up and farthest from rounding up. if there is a tie, mark it
# 3) if there is no tie, take away or add delegates to the winning/losing candidates in the rankings
# 4) repeat the process, so if there are still too many/few delegates given, find those cases and re-rank the candidates. a candidate who won/lost last time should now be ranked differently because they're closer/farther than before. then repeat the finding of ties, etc.

# the only issue is that a tie at any stage is marked the same. So let's say there are 2 extra candidates given. there could be a tie for the 1st one to be taken away, which would be marked "

rank_distances <- function(data){
  data %>% 
    group_by(precinct_full) %>% 
    mutate(distance_next = case_when(
      total_final_del != precinct_delegates & 
        viablefinal &
        !viable_loss &
        !more_final_votes &
        !nonviable_no_realign ~ final_del+1 - caucus_formula_result,
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
      total_final_del > precinct_delegates & 
        is_farthest & final_del > 1 & 
        game_of_chance != "too_many_del_tie" ~ final_del - 1,
      how_many_farthest <= (total_final_del - precinct_delegates) &
        is_farthest &
        final_del > 1 &
        game_of_chance == "too_many_del_tie" ~ final_del - 1,
      TRUE ~ final_del
    )) %>% 
    group_by(precinct_full) %>% 
    mutate(total_final_del = sum(final_del, na.rm = T)) %>% 
    ungroup()
}

add_too_few_dels <- function(data){
  data %>% 
    mutate(final_del = case_when(
      total_final_del < precinct_delegates & 
        is_closest & 
        game_of_chance != "too_few_del_tie" ~ final_del + 1,
      how_many_closest <= precinct_delegates - total_final_del &
        is_closest &
        game_of_chance == "too_few_del_tie" ~ final_del + 1,
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
    group_by(precinct_full) %>%
    mutate(unviable_unallocated = sum(alignfinal[!viablefinal])) %>%
    mutate(expected_align_final = case_when(!viablefinal & candidate != "Uncommitted" ~ 0,
                                            viablefinal & candidate != "Uncommitted" ~ alignfinal,
                                            candidate == "Uncommitted" ~ alignfinal + unviable_unallocated)) %>%
    ungroup() %>%
    mutate(caucus_formula_result = case_when(
      viablefinal ~ round((expected_align_final * precinct_delegates) / total_align1, digits = 4),
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
  
  data <- data %>% 
    group_by(precinct_full) %>% 
    mutate(del_count_diff = case_when(
      reported_del_given != final_del ~ TRUE,
      TRUE ~ FALSE
    ))
  
  return(data)
}

join_comments<- function(data){
  
  comment_sheet <- key_get("nv_caucus_comment_sheet")  %>% 
    read_sheet()
  
  comment_sheet <- comment_sheet %>%
    mutate(precinct_full = as.character(precinct_full))

  data <- data %>% 
    left_join(comment_sheet, by = c("precinct_full", "county"))
 
  return(data)
  
}  
  

push <- function(data, test_run = TRUE){
  
  file_dir <- ifelse(test_run, "./nevada_data/test/", "./nevada_data/prod/")
  file_name <- paste0("nevada_caucus_data-", strftime(Sys.time(), format = "%Y-%m-%d_%H%M%S"), ".csv")
  
  
  data %>%
    write_csv(paste0(file_dir, file_name))
  
  put_object(file = paste0(file_dir, file_name), object = file_name, bucket = key_get("nv_caucus_data_bucket"))
  

}

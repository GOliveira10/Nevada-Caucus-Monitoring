library(rvest)
library(tidyverse)
library(httr)

# For questions contact @MCulshawMaurer on twitter or @MCMaurer on GitHub
scrape_clean_write <- function(){
# pull the data in and get the relevant results
nyt_precincts <- GET('https://int.nyt.com/applications/elections/2020/data/api/2020-02-22/precincts/NevadaDemPrecinctsGcs-latest.json') %>%
   content()

nyt_precincts <- nyt_precincts$precincts %>%
  map_if(negate(is.list), function(x) flatten_chr(x)) %>% enframe()

# making the results into something legible
nyt_results <- nyt_precincts$value %>%
  map(unlist) %>%
  map(map, as.character) %>%
  map(bind_rows) %>%
  bind_rows() %>%
  rename(county = locality_name)


nyt_results <- nyt_results %>%
  pivot_longer(cols = contains("results"), names_to = "result_type", values_to = "result") %>%
  mutate(result_type = stringr::str_remove_all(result_type, "results_")) %>%
  separate(col = result_type, into = c("round", "candidate")) %>%
  mutate_at(vars(votes, votes_align1, votes_alignfinal, result), as.numeric) %>%
  filter(county != "Sample") %>%
  select(-c(locality_fips, locality_type, is_geographic, votes_alignfinal, votes_align1, votes)) %>%
  mutate(precinct = precinct_name) %>%
  filter(round != "results") %>%
  mutate(is_complete = as.logical(is_complete)) %>%
  mutate(precinct_full = paste(precinct, precinct_id, sep = "_")) %>%
  pivot_wider(names_from = round, values_from = result) %>%
  rename(GEOID10 = geo_id)
# mutate(GEOID10 = str_remove(GEOID10, "-"))

complete <- nyt_results %>% select(county, precinct = precinct_name, is_complete) %>% distinct()


precincts <- GET('https://nevadacaucusresults.com/results/nv_caucus_precinct_results.json') %>% content()

results <- precincts %>% 
  map(bind_rows) %>% 
  bind_rows() %>%
  separate(precinct_id, into = c("county", "precinct")) %>% 
  pivot_longer(cols = 4:ncol(.)) %>%
  separate(name, into = c("candidate", "round")) %>%
  mutate(round = case_when(round == "first" ~ "align1",
                           round == "final" ~ "alignfinal",
                           round == "county" ~ "reported_del_given")) %>%
  mutate(value = as.numeric(value)) %>%
  pivot_wider(names_from = round, values_from = value)





delegates <- read_csv("nevada_dems_docs/nevada_delegate_apportionment.csv") %>% 
  rename(precinct_delegates = delegates) %>% 
  select(county, precinct, congressional_district, precinct_delegates) %>%
    mutate(precinct = as.character(precinct))

strip_delegates <- tibble(county = "Clark", 
                          precinct = c("Bellagio",
                                       "Harrah's",
                                       "Mandalay Bay",
                                       "Paris",
                                       "Park MGM",
                                       "Rio",
                                       "Wynn"),
                          precinct_delegates = c(
                            51,
                            34,
                            33,
                            33,
                            33,
                            13,
                            11
                          ))

delegates <- delegates %>% 
  bind_rows(strip_delegates)
  
delegates <- delegates %>% mutate(county = str_extract(county, "([^\\s]+)"))

results <- results %>% mutate(county = str_extract(county, "([^\\s]+)"))

complete <- complete %>% mutate(county = toupper(str_extract(county, "([^\\s]+)")))


results <- results %>% 
  left_join(delegates)


# results <- results %>% arrange(desc(precinct_name))

timestamped_name <- paste0("./nevada_data/cleaned_timestamped/cleaned_timestamped_", strftime(Sys.time(), format = "%Y-%m-%d_%H%M%S"), ".csv")

results <- results %>% mutate(county = toupper(county),
                              precinct_full = paste0(county, "-", precinct),
                              candidate = str_to_title(candidate)) %>%
  left_join(complete)





results %>% write_csv(timestamped_name)

}

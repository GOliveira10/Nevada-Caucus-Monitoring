library(rvest)
library(tidyverse)
library(httr)

# For questions contact @MCulshawMaurer on twitter or @MCMaurer on GitHub
scrape_clean_write <- function(){
# pull the data in and get the relevant results
precincts <- GET('https://int.nyt.com/applications/elections/2020/data/api/2020-02-22/precincts/NevadaDemPrecinctsGcs-latest.json') %>%
  content()


precincts <- precincts$precincts %>% 
  map_if(negate(is.list), function(x) flatten_chr(x)) %>% enframe()

# making the results into something legible
results <- precincts$value %>% 
  map(unlist) %>% 
  map(map, as.character) %>% 
  map(bind_rows) %>% 
  bind_rows() %>% 
  rename(county = locality_name)


results <- results %>% 
  pivot_longer(cols = contains("results"), names_to = "result_type", values_to = "result") %>% 
  mutate(result_type = stringr::str_remove_all(result_type, "results_")) %>% 
  separate(col = result_type, into = c("round", "candidate")) %>% 
  mutate_at(vars(votes, votes_align1, votes_alignfinal, result), as.numeric) %>% 
  filter(county != "Sample") %>%
  select(-c(locality_fips, locality_type, is_geographic)) %>% 
  mutate(precinct = precinct_name) %>%
  filter(round != "results") %>% 
  mutate(is_complete = as.logical(is_complete)) %>% 
  mutate(precinct_full = paste(precinct, precinct_id, sep = "_")) %>% 
  pivot_wider(names_from = round, values_from = result) %>%
  rename(GEOID10 = geo_id)


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
  
results <- results %>% 
  select(-county) %>% 
  left_join(delegates)


results <- results %>% arrange(desc(precinct_name))

timestamped_name <- paste0("./nevada_data/cleaned_timestamped/cleaned_timestamped_", strftime(Sys.time(), format = "%Y-%m-%d_%H%M%S"), ".csv")

results %>% write_csv(timestamped_name)

}

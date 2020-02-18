library(rvest)
library(tidyverse)
library(httr)

# For questions contact @MCulshawMaurer on twitter or @MCMaurer on GitHub

# pull the data in and get the relevant results
precincts <- GET("https://int.nyt.com/applications/elections/2020/data/api/2020-02-03/precincts/IowaDemPrecinctsSFTP-latest.json") %>%
  content()
precincts
precincts$precincts
precincts <- precincts$precincts %>% 
  map_if(negate(is.list), function(x) flatten_chr(x)) %>% enframe()

# making the results into something legible
results <- precincts$value %>% 
  map(unlist) %>% 
  map(map, as.character) %>% 
  map(bind_rows) %>% 
  bind_rows() %>% 
  rename(county = locality_name)

delegates <- read_csv("nevada_dems_docs/nevada_delegate_apportionment.csv") %>% 
  rename(precinct_delegates = delegates) %>% 
  select(county, precinct, congressional_district, precinct_delegates)

delegates %>% 
  print(n = 100)

library(rvest)
library(tidyverse)
library(httr)




nevada_geo_ids <- jsonlite::read_json("./nevada_dems_docs/geoids_from_geojson.js") %>%
  map(map, "properties") %>% 
  flatten() %>% map(`[`, c("GEOID10", "COUNTYFP10", "VTDST10")) %>% bind_rows() %>%
  rename(county_fips = COUNTYFP10)




fips <- 'https://web.archive.org/web/20121008031808/http://www.epa.gov/enviro/html/codes/nv.html' %>% read_html() %>% html_nodes(xpath = "//table") %>% html_table()

county_fips <- fips[[2]] %>% rename(county = `County Name`)
state_fips <- fips[[1]]

county_fips %>% mutate(county_fips = `FIPS Code`,
                       state_fips = 32) %>%
  select(-`FIPS Code`) %>% mutate(county_fips = case_when(nchar(county_fips) == 1 ~ paste0("00", county_fips),
                                                          nchar(county_fips) == 2 ~ paste0("0", county_fips),
                                                          TRUE ~ as.character(county_fips))) %>%
  write_csv("./nevada_dems_docs/nevada_fips_codes.csv")

county_fips <- read_csv("./nevada_dems_docs/nevada_fips_codes.csv")

nevada_geo_ids <- nevada_geo_ids %>% right_join(county_fips) %>%
  rename(precinct = VTDST10)

precincts <- read_csv("./nevada_dems_docs/nevada_delegate_apportionment.csv")

results <- read_csv("./iowa_data/cleaned_nyt_results2020-02-06_113324.csv")

iowa_data <- results %>%
  rename(county = County) %>%
  pivot_wider(values_from = result, names_from = round) %>%
  select(county, precinct, precinct_full, precinct_delegates, 
         candidate, align1, alignfinal) %>%
  mutate(row_id = row_number()) %>%
  select(row_id, everything())

nevada_precincts <- precincts %>% 
  select(county, precinct) %>% 
  mutate(county = toupper(county)) %>%
  distinct() %>% 
  arrange(desc(county))





iowa_precincts <- results %>% 
  select(county = County, precinct) %>% 
  distinct()

nevada_precincts <- nevada_precincts %>% 
  slice(1:nrow(iowa_precincts)) %>%
  mutate(precinct = as.character(precinct))



for(i in 1:nrow(iowa_precincts)){
  
  iowa_data$precinct[which(iowa_data$county == iowa_precincts$county[i] & 
                             iowa_data$precinct == iowa_precincts$precinct[i])] <- nevada_geo_ids$precinct[i]

  iowa_data$county[which(iowa_data$county == iowa_precincts$county[i] & 
                            iowa_data$precinct == nevada_geo_ids$precinct[i])] <- nevada_geo_ids$county[i]
  
  
  
  iowa_data$GEOID10[which(iowa_data$county == nevada_geo_ids$county[i] & 
                           iowa_data$precinct == nevada_geo_ids$precinct[i])] <- nevada_geo_ids$GEOID10[i]
  

}

nevada_data <- iowa_data %>% 
  mutate(precinct_full = precinct) %>%
  mutate(county = toupper(county))


## Some validations

# All GEOIDs in the original GEOID set (no NAs or other weirdness)
nevada_data[which(nevada_data$GEOID10 %in% geo_ids == FALSE),]

### Make sure there are no repeats

nevada_data <- nevada_data %>% group_by(GEOID10) %>% mutate(n = n()) %>% filter(n == 14) %>% select(-n)


nevada_data %>% 
  write_csv("./nevada_data/test/nevada_caucus_data_input_geo.csv")





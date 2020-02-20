library(rvest)
library(tidyverse)
library(httr)



fips <- 'https://web.archive.org/web/20121008031808/http://www.epa.gov/enviro/html/codes/nv.html' %>% read_html() %>% html_nodes(xpath = "//table") %>% html_table()

county_fips <- fips[[2]] %>% rename(county = `County Name`)
state_fips <- fips[[1]]

county_fips %>% mutate(county_fips = `FIPS Code`,
                       state_fips = 32) %>%
  select(-`FIPS Code`) %>% write_csv("./nevada_dems_docs/nevada_fips_codes.csv")

county_fips <- read_csv("./nevada_dems_docs/nevada_fips_codes.csv")


precincts <- read_csv("./nevada_dems_docs/nevada_delegate_apportionment.csv")

results <- read_csv("./iowa_data/cleaned_nyt_results2020-02-06_113324.csv")

iowa_data <- results %>%
  rename(county = County) %>%
  pivot_wider(values_from = result, names_from = round) %>%
  select(county, precinct, precinct_full, precinct_delegates, candidate, align1, alignfinal) %>%
  mutate(row_id = row_number()) %>%
  select(row_id, everything())

iowa_counties <- unique(iowa_data$county)
nevada_counties <- unique(precincts$county)


for(i in 1:length(iowa_counties)){
  
  
  iowa_precincts <- unique(iowa_data$precinct[iowa_data$county == iowa_counties[i]])
  iowa_data$county[which(iowa_data$county == iowa_counties[i])] <- nevada_counties[i]
  
  nevada_precincts <- precincts$precinct[which(precincts$county == nevada_counties[i])]
  
  for(g in 1:length(iowa_precincts)){
    
    iowa_data$precinct[which(iowa_data$precinct == iowa_precincts[g])] <- nevada_precincts[g]
    
  }
  

}

nevada_data <- iowa_data %>% 
  mutate(precinct_full = precinct) %>%
  mutate(county = toupper(county)) %>%
  left_join(county_fips) %>%
  mutate(GEOID10 = paste0(state_fips, county_fips, precinct))  %>% 
  write_csv("./nevada_data/test/nevada_caucus_data_input_geo.csv")




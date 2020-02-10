library(tidyverse)
library(pdftools)


pdf_file <- "nevada_precinct_apportionment.pdf"

data <- pdf_data(pdf_file)

data.x <- data[[41]] 


pivot_to_table <- function(data){
  
  if(max(data$space) == 1){
  
  table <- data %>% 
    group_by(y) %>%
    mutate(space_before = lag(space)) %>%
    summarize(county = paste0(text[(space|space_before) | (!space & is.na(space_before))], collapse = " "),
              cols = str_remove(paste0(text[!space & !space_before], collapse = ";"), "NA;")) %>%
    filter(cols != "NA") %>%
    separate(cols, sep = ";", into = c("precinct", "congressional_district", 
                           "democrats_in_precinct", "apportionment_clause", "delegates", "alternates")) %>%
    mutate_at(vars(precinct, congressional_district, 
                   democrats_in_precinct, delegates, alternates), as.double) %>%
    select(-y)
    
  } else{
    
  table <- data %>% 
      group_by(y) %>%
      summarize(cols = paste(text, collapse = ";"),
                rows = n()) %>%
      filter(cols != "NA" | rows < 3) %>%
      separate(cols, sep = ";", into = c("county", "precinct", "congressional_district", 
                                         "democrats_in_precinct", "apportionment_clause", "delegates", "alternates")) %>%
      mutate_at(vars(precinct, congressional_district, 
                     democrats_in_precinct, delegates, alternates), as.double) %>%
      select(-y, -rows)
    
    
  }
    
  table 
  
}

map(data, pivot_to_table) %>% 
  bind_rows() %>% 
  drop_na() %>% write_csv(path = "nevada_delegate_apportionment.csv")

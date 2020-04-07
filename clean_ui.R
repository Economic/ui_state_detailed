library(googlesheets4)
library(tidyverse)

basesheet <- "https://docs.google.com/spreadsheets/d/1FAgigJSpBcmpV70rRJfzzj4EzhkvXExwVyTTiqZb2lE"

# generalized cleaning code
clean_industry_state <- function(stateabb) {
  # run the clean code for the state
  eval(parse(text=paste0("clean_industry_", stateabb, "()")))
}

# Massachusetts
clean_industry_ma <- function() {
  read_sheet(basesheet, sheet = "ma_industry", col_types = "ciii") %>% 
    rename(industry = Industry) %>% 
    mutate(
      sector = case_when(
        industry == "Agriculture, Forestry, Fishing, and Hunting" ~ "11",
        industry == "Mining" ~ "21",
        industry == "Utilities" ~ "22",
        industry == "Construction" ~ "23",
        industry == "Manufacturing" ~ "31-33",
        industry == "Wholesale Trade" ~ "42",
        industry == "Retail Trade" ~ "44-45",
        industry == "Transportation & Warehouse" ~ "48-49",
        industry == "Information" ~ "51",
        industry == "Finance & Insurance" ~ "52",
        industry == "Real Estate" ~ "53",
        industry == "Professional and Technical Services" ~ "54",
        industry == "Management of Companies" ~ "55",
        industry == "Administrative & Waste Services" ~ "56",
        industry == "Education" ~ "61",
        industry == "Healthcare & Social Assistance" ~ "62",
        industry == "Arts, Entertainment & Recreation" ~ "71",
        industry == "Food & Accomodation" ~ "72",
        industry == "Other Services" ~ "81",
        industry == "Public Administration" ~ "92",
        industry == "Information Not Available" ~ "99"
      )
    ) %>% 
    transmute(
      stateabb = "MA",
      sector = sector,
      week0314 = `Week Ending 3/14`,
      week0321 = `Week Ending 3/21`,
      week0328 = `Week Ending 3/28`
    )
}

# Washington
clean_industry_wa <- function() {
  read_sheet(basesheet, sheet = "wa_industry", col_types = "ciiii") %>% 
    # use combined manufacturing
    filter(!industry %in% c("31", "32", "33")) %>% 
    # use combined retail
    filter(!industry %in% c("44", "45")) %>% 
    # use combined transportation & warehousing
    filter(!industry %in% c("48", "49")) %>% 
    mutate(
      industry = case_when(
        industry == "Manufacturing" ~ "31-33",
        industry == "Retail Trade" ~ "44-45",
        industry == "Trans/Wrhse" ~ "48-49",
        industry == "INA/99" ~ "99",
        TRUE ~ industry
      )
    ) %>% 
    transmute(
      stateabb = "WA",
      sector = industry,
      week0307 = `Week Ending 3/7`,
      week0314 = `Week Ending 3/14`,
      week0321 = `Week Ending 3/21`,
      week0328 = `Week Ending 3/28`
    )
}

map_dfr(c("ma", "wa"), clean_industry_state)


  
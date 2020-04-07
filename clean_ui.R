library(googlesheets4)
library(tidyverse)

basesheet <- "https://docs.google.com/spreadsheets/d/1FAgigJSpBcmpV70rRJfzzj4EzhkvXExwVyTTiqZb2lE"

# call to cleaning function for given state
clean_industry_state <- function(stateabb) {
  # run the clean industry function for state
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
    ) %>% 
    pivot_longer(matches("week"), names_to = "endweek", values_to = "ic") %>% 
    mutate(endweek = str_sub(endweek, start = 5))
}

# Michigan
clean_industry_mi <- function() {
  read_sheet(basesheet, sheet = "mi_industry") %>% 
    rename(industry = Industry) %>% 
    mutate(
      sector = case_when(
        industry == "Agriculture, Forestry, Fishing, and Hunting" ~ "11",
        industry == "Mining, Quarrying, and Oil and Gas Extraction" ~ "21",
        industry == "Utilities" ~ "22",
        industry == "Construction" ~ "23",
        industry == "Manufacturing" ~ "31-33",
        industry == "Wholesale Trade" ~ "42",
        industry == "Retail Trade" ~ "44-45",
        industry == "Transportation and Warehousing" ~ "48-49",
        industry == "Information" ~ "51",
        industry == "Finance and Insurance" ~ "52",
        industry == "Real Estate and Rental and Leasing" ~ "53",
        industry == "Professional, Scientific, and Technical Services" ~ "54",
        industry == "Management of Companies and Enterprises" ~ "55",
        industry == "Admin. and Support, Waste Mgmt. and Remediation Services" ~ "56",
        industry == "Educational Services" ~ "61",
        industry == "Health Care and Social Assistance" ~ "62",
        industry == "Arts, Entertainment, and Recreation" ~ "71",
        industry == "Accommodation and Food Services" ~ "72",
        industry == "Other Services (except Public Administration)" ~ "81",
        industry == "Public Administration" ~ "92",
        industry == "Unclassified" ~ "99"
      )
    ) %>% 
    filter(!is.na(sector)) %>% 
    transmute(
      stateabb = "MI",
      sector = sector,
      week0314 = `Week Ending March 14`,
      week0321 = `Week Ending March 21`,
      week0328 = `Week Ending March 28`
    ) %>% 
    pivot_longer(matches("week"), names_to = "endweek", values_to = "ic") %>% 
    mutate(endweek = str_sub(endweek, start = 5))
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
    ) %>% 
    pivot_longer(matches("week"), names_to = "endweek", values_to = "ic") %>% 
    mutate(endweek = str_sub(endweek, start = 5))
}

# Wyoming
clean_industry_wy <- function() {
  read_sheet(basesheet, sheet = "wy_industry") %>% 
    rename(industry = Industry) %>% 
    filter(industry != "Total") %>% 
    mutate(
      sector = case_when(
        industry == "11-Agriculture, forestry, fishing and hunting (11)" ~ "11",
        industry == "21-Mining, quarrying, and oil and gas extraction (21)" ~ "21",
        industry == "22, 48, 49 - Transportation, Warehousing, and Utilities" ~ "1021",
        industry == "23-Construction (23)" ~ "23",
        industry == "31, 32, 33 - Manufacturing" ~ "31-33",
        industry == "42-Wholesale trade (42)" ~ "42",
        industry == "44, 45 - Retail Trade" ~ "44-45",
        industry == "51-Information (51)" ~ "51",
        industry == "52, 53 - Financial Activities" ~ "1023",
        industry == "54, 55, 56 - Professional and Business Services" ~ "1024",
        industry == "61-Educational services (61)" ~ "61",
        industry == "62-Health care and social assistance (62)" ~ "62",
        industry == "71, 72 - Leisure and Hospitality" ~ "1026",
        industry == "81-Other services, except public administration (81)" ~ "81",
        industry == "92-Public administration (92)" ~ "92",
        industry == "99 - Unknown" ~ "99",
        industry == "99-Unclassified (99)" ~ "99"
      )
    ) %>% 
    transmute(
      stateabb = "WY",
      sector = sector,
      ic = `Initial Claims`,
      endweek = str_sub(`Week Ending`, start = 5)
    )
}

# combine data from all states
map_dfr(c("ma", "mi", "wa", "wy"), clean_industry_state)


  
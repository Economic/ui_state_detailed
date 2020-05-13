basesheet <- "https://docs.google.com/spreadsheets/d/1FAgigJSpBcmpV70rRJfzzj4EzhkvXExwVyTTiqZb2lE"
sheets_deauth()

# call to cleaning function for given state
clean_industry_state <- function(stateabb) {
  # run the clean industry function for state
  eval(parse(text=paste0("clean_industry_", stateabb, "()")))
}

# Alabama
clean_industry_al <- function() {
  read_sheet(basesheet, sheet = "al_industry", col_types = "cciiiiiiii") %>% 
    mutate(stateabb = "AL") %>%
    mutate(sector = ifelse(`NAICS Title` == "INA", 99, Naics)) %>% 
    filter(!is.na(sector)) %>% 
    select(stateabb, sector, matches("week")) %>% 
    pivot_longer(matches("week"), names_to = "endweek", values_to = "ic") %>% 
    mutate(endweek = str_sub(endweek, start = 5))
}

# California
# No Public Admin, includes Utilities in Trans & Warehousing
clean_industry_ca <- function() {
  read_sheet(basesheet, sheet = "ca_industry") %>% 
    rename(industry = Industry) %>% 
    mutate(
      stateabb = "CA",
      sector = case_when(
        industry == "Agriculture Forestry Fishing" ~ "11",
        industry == "Mining Oil Gas" ~ "21",
        industry == "Construction" ~ "23",
        industry == "Manufacturing" ~ "31-33",
        industry == "Wholesale Trade" ~ "42",
        industry == "Retail Trade" ~ "44-45",
        industry == "Transportation Warehousing Utilities" ~ "48-49, 22",
        industry == "Information" ~ "51",
        industry == "Finance and Insurance" ~ "52",
        industry == "Real Estate and Leasing" ~ "53",
        industry == "Prof. Scientific Techn. Services" ~ "54",
        industry == "Management" ~ "55",
        industry == "Admin. Support Waste Mgmt" ~ "56",
        industry == "Education Services" ~ "61",
        industry == "Health Care and Social Assistance" ~ "62",
        industry == "Arts Entertainment Recreation" ~ "71",
        industry == "Accommodation and Food Services" ~ "72",
        industry == "Other Services" ~ "81",
      )
    ) %>% 
    select(-weeks_0321_0411) %>% 
    select(stateabb, sector, matches("week")) %>% 
    pivot_longer(matches("week"), names_to = "endweek", values_to = "ic") %>% 
    mutate(endweek = str_sub(endweek, start = 5))
}

# Colorado
clean_industry_co <- function() {
  read_sheet(basesheet, sheet = "co_industry") %>% 
    rename(industry = Industry) %>% 
    mutate(
      stateabb = "CO",
      sector = case_when(
        industry == "Agriculture, Forestry, Fishing and Hunting" ~ "11",
        industry == "Mining" ~ "21",
        industry == "Utilities" ~ "22",
        industry == "Construction" ~ "23",
        industry == "Manufacturing" ~ "31-33",
        industry == "Wholesale Trade" ~ "42",
        industry == "Retail Trade" ~ "44-45",
        industry == "Transportation and Warehousing" ~ "48-49",
        industry == "Information" ~ "51",
        industry == "Finance and Insurance" ~ "52",
        industry == "Real Estate, Rental, and Leasing" ~ "53",
        industry == "Professional and Technical Services" ~ "54",
        industry == "Management of Companies and Enterprises" ~ "55",
        industry == "Administrative and Waste Services" ~ "56",
        industry == "Education Services" ~ "61",
        industry == "Health Care and Social Assistance" ~ "62",
        industry == "Arts, Entertainment, and Recreation" ~ "71",
        industry == "Accommodation and Food Services" ~ "72",
        industry == "Other Services" ~ "81",
        industry == "Public Administration" ~ "92"
      )
    ) %>% 
    select(stateabb, sector, matches("week")) %>% 
    pivot_longer(matches("week"), names_to = "endweek", values_to = "ic") %>% 
    mutate(endweek = str_sub(endweek, start = 5))
}

# Georgia
clean_industry_ga <- function() {
  read_sheet(basesheet, sheet = "ga_industry", col_types = "cciiiiiiiii") %>% 
    mutate(stateabb = "GA") %>%
    rename(sector = NAICS) %>% 
    filter(!is.na(sector)) %>% 
    select(stateabb, sector, matches("week")) %>% 
    pivot_longer(matches("week"), names_to = "endweek", values_to = "ic") %>% 
    mutate(endweek = str_sub(endweek, start = 5))
}

# Iowa
clean_industry_ia <- function() {
  read_sheet(basesheet, sheet = "ia_industry") %>% 
    rename(industry = Industry) %>% 
    mutate(
      stateabb = "IA",
      sector = case_when(
        industry == "Agriculture, Forestry, Fishing & Hunting" ~ "11",
        industry == "Mining" ~ "21",
        industry == "Utilities" ~ "22",
        industry == "Construction" ~ "23",
        industry == "Manufacturing" ~ "31-33",
        industry == "Wholesale Trade" ~ "42",
        industry == "Retail Trade" ~ "44-45",
        industry == "Transportation and Warehousing" ~ "48-49",
        industry == "Information" ~ "51",
        industry == "Finance and Insurance" ~ "52",
        industry == "Real Estate and Rental and Leasing" ~ "53",
        industry == "Professional Scientific & Technical Svc" ~ "54",
        industry == "Management of Companies and Enterprises" ~ "55",
        industry == "Admin., Support, Waste Mgmt, Remediation" ~ "56",
        industry == "Education Services" ~ "61",
        industry == "Health Care and Social Assistance" ~ "62",
        industry == "Arts, Entertainment, and Recreation" ~ "71",
        industry == "Accommodation and Food Services" ~ "72",
        industry == "Other Services (except Public Admin.)" ~ "81",
        industry == "Public Administration" ~ "92",
        industry == "Information Not Available (Includes persons not covered by UI such as self-employed and independent contractors.)" ~ "99"
      )
    ) %>% 
    filter(!is.na(sector)) %>% 
    select(stateabb, sector, matches("week")) %>% 
    pivot_longer(matches("week"), names_to = "endweek", values_to = "ic") %>% 
    mutate(endweek = str_sub(endweek, start = 5)) %>% 
    filter(as.numeric(endweek) >= 307)
}

# Kansas
# missing Utilities... not sure which industry they stick it in.
clean_industry_ks <- function() {
  read_sheet(basesheet, sheet = "ks_industry") %>% 
    rename(industry = Industry) %>% 
    mutate(
      stateabb = "KS",
      sector = case_when(
        industry == "Agriculture" ~ "11",
        industry == "Mining" ~ "21",
        industry == "Construction" ~ "23",
        industry == "Manufacturing" ~ "31-33",
        industry == "Wholesale trade" ~ "42",
        industry == "Retail trade" ~ "44-45",
        industry == "Transportation and warehousing" ~ "48-49",
        industry == "Information" ~ "51",
        industry == "Finance and insurance" ~ "52",
        industry == "Real estate and rental and leasing" ~ "53",
        industry == "Professional, scientific, and technical services" ~ "54",
        industry == "Management of companies and enterprises" ~ "55",
        industry == "Administrative and waste services" ~ "56",
        industry == "Educational services" ~ "61",
        industry == "Health care and social assistance" ~ "62",
        industry == "Arts, entertainment, and recreation" ~ "71",
        industry == "Accomodation and food services" ~ "72",
        industry == "Other services (except public administration)" ~ "81",
        industry == "Public administration" ~ "92",
      )
    ) %>% 
    filter(!is.na(sector)) %>% 
    select(stateabb, sector, matches("week")) %>% 
    pivot_longer(matches("week"), names_to = "endweek", values_to = "ic") %>% 
    mutate(endweek = str_sub(endweek, start = 5))
}



# Massachusetts
clean_industry_ma <- function() {
  read_sheet(basesheet, sheet = "ma_industry") %>% 
    rename(industry = Industry) %>% 
    mutate(
      stateabb = "MA",
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
    filter(!is.na(sector)) %>% 
    select(stateabb, sector, matches("week")) %>% 
    pivot_longer(matches("week"), names_to = "endweek", values_to = "ic") %>% 
    mutate(endweek = str_sub(endweek, start = 5))
}

# Maine
# missing utilities & mining
clean_industry_me <- function() {
  read_sheet(basesheet, sheet = "me_industry") %>% 
    rename(industry = Industry) %>% 
    mutate(
      stateabb = "ME",
      sector = case_when(
        industry == "Agriculture, Forestry, Fishing & Hunting" ~ "11",
        industry == "Construction" ~ "23",
        industry == "Manufacturing" ~ "31-33",
        industry == "Wholesale Trade" ~ "42",
        industry == "Retail" ~ "44-45",
        industry == "Transportation & Warehousing" ~ "48-49",
        industry == "Information" ~ "51",
        industry == "Finance & Insurance" ~ "52",
        industry == "Real Estate & Rental & Leasing" ~ "53",
        industry == "Professional, Scientific, & Technical Services" ~ "54",
        industry == "Management of Companies and Enterprises; Finance and Insurance" ~ "55",
        industry == "Administrative & Support & Waste Management & Remediation Services" ~ "56",
        industry == "Educational Services" ~ "61",
        industry == "Health Care & Social Assistance" ~ "62",
        industry == "Entertainment & Recreation" ~ "71",
        industry == "Food Services & Lodging" ~ "72",
        industry == "Other Services (except Public Administration)" ~ "81",
        industry == "Public Administration" ~ "92",
        industry == "Industry Not Identified (<5 Claims)" ~ "99"
      )
    ) %>% 
    filter(!is.na(sector)) %>% 
    select(stateabb, sector, matches("week")) %>% 
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
        industry == "Agriculture Forestry Fishing and Hunting" ~ "11",
        industry == "Mining, Quarrying, and Oil and Gas Extraction" ~ "21",
        industry == "Mining Quarrying and Oil and Gas Extraction" ~ "21",
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
        industry == "Professional, Scientific and Technical Services" ~ "54",
        industry == "Professional Scientific and Technical Services" ~ "54",
        industry == "Management of Companies and Enterprises" ~ "55",
        industry == "Admin. and Support, Waste Mgmt. and Remediation Services" ~ "56",
        industry == "Admin. and Support Waste Mgmt. and Remediation Services" ~ "56",
        industry == "Educational Services" ~ "61",
        industry == "Health Care and Social Assistance" ~ "62",
        industry == "Arts, Entertainment, and Recreation" ~ "71",
        industry == "Arts Entertainment and Recreation" ~ "71",
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
      endweek = paste0(sprintf("%02d", month(endweek)), sprintf("%02d", day(endweek))),
      ic = ic
    )
}

# Nebraska
# only 3-21+, but otherwise complete
clean_industry_ne <- function() {
  read_sheet(basesheet, sheet = "ne_industry") %>% 
    rename(industry = Industry) %>% 
    mutate(
      stateabb = "NE",
      sector = case_when(
        industry == "Agriculture, Forestry, Fishing & Hunting" ~ "11",
        industry == "Mining" ~ "21",
        industry == "Utilities" ~ "22",
        industry == "Construction" ~ "23",
        industry == "Manufacturing" ~ "31-33",
        industry == "Wholesale Trade" ~ "42",
        industry == "Retail Trade" ~ "44-45",
        industry == "Transportation and Warehousing" ~ "48-49",
        industry == "Information" ~ "51",
        industry == "Finance and Insurance" ~ "52",
        industry == "Real Estate and Rental and Leasing" ~ "53",
        industry == "Professional and Technical Services" ~ "54",
        industry == "Management of Companies and Enterprises" ~ "55",
        industry == "Administrative and Waste Services" ~ "56",
        industry == "Educational Services" ~ "61",
        industry == "Health Care and Social Assistance" ~ "62",
        industry == "Arts, Entertainment, and Recreation" ~ "71",
        industry == "Accommodation and Food Services" ~ "72",
        industry == "Other Services, Ex. Public Admin" ~ "81",
        industry == "Public Administration" ~ "92",
        industry == "Unknown" ~ "99"
      )
    ) %>% 
    filter(!is.na(sector)) %>% 
    select(stateabb, sector, matches("week")) %>% 
    pivot_longer(matches("week"), names_to = "endweek", values_to = "ic") %>% 
    mutate(endweek = str_sub(endweek, start = 5))
}

# Nevada
# only 3-21+, but otherwise complete
clean_industry_nv <- function() {
  read_sheet(basesheet, sheet = "nv_industry") %>% 
    rename(industry = Industry) %>% 
    mutate(
      stateabb = "NV",
      sector = case_when(
        industry == "Agriculture Forestry Fishing and Hunting" ~ "11",
        industry == "Mining" ~ "21",
        industry == "Utilities" ~ "22",
        industry == "Construction" ~ "23",
        industry == "Manufacturing" ~ "31-33",
        industry == "Wholesale Trade" ~ "42",
        industry == "Retail Trade" ~ "44-45",
        industry == "Transportation and Warehousing" ~ "48-49",
        industry == "Information" ~ "51",
        industry == "Finance and Insurance" ~ "52",
        industry == "Real Estate and Rental and Leasing" ~ "53",
        industry == "Professional Scientific and Technical Services" ~ "54",
        industry == "Management of Companies and Enterprises" ~ "55",
        industry == "Administrative and Support Waste Management" ~ "56",
        industry == "Educational Services" ~ "61",
        industry == "Health Care and Social Assistance" ~ "62",
        industry == "Arts Entertainment and Recreation" ~ "71",
        industry == "Accomodations and Food Services" ~ "72",
        industry == "Other Services (except Public Administration)" ~ "81",
        industry == "Public Administration" ~ "92",
        # NV includes both "unknown" and "unclassified" industries
        # classifying unkown + unclassified as same sector
        industry == "Unknown Industry" ~ "99",
        industry == "Unclassified Establishments" ~ "99"
      )
    ) %>% 
    filter(!is.na(sector)) %>% 
    select(stateabb, sector, matches("week")) %>% 
    pivot_longer(matches("week"), names_to = "endweek", values_to = "ic") %>% 
    mutate(endweek = str_sub(endweek, start = 5))
}

# New Mexico
clean_industry_nm <- function() {
  read_sheet(basesheet, sheet = "nm_industry") %>% 
    rename(industry = Industry) %>% 
    mutate(
      stateabb = "NM",
      sector = case_when(
        industry == "Agriculture Forestry Fishing and Hunting" ~ "11",
        industry == "Mining" ~ "21",
        industry == "Utilities" ~ "22",
        industry == "Construction" ~ "23",
        industry == "Manufacturing" ~ "31-33",
        industry == "Wholesale Trade" ~ "42",
        industry == "Retail Trade" ~ "44-45",
        industry == "Transportation and Warehousing" ~ "48-49",
        industry == "Information" ~ "51",
        industry == "Finance and Insurance" ~ "52",
        industry == "Real Estate and Rental and Leasing" ~ "53",
        industry == "Professional Scientific and Technical Services" ~ "54",
        industry == "Management of Companies and Enterprises" ~ "55",
        industry == "Administrative and Support Waste Management" ~ "56",
        industry == "Educational Services" ~ "61",
        industry == "Health Care and Social Assistance" ~ "62",
        industry == "Arts Entertainment and Recreation" ~ "71",
        industry == "Accomodations and Food Services" ~ "72",
        industry == "Other Services (except Public Administration)" ~ "81",
        industry == "Public Administration" ~ "92",
        industry == "INA" ~ "99"
      )
    ) %>% 
    filter(!is.na(sector)) %>% 
    select(stateabb, sector, matches("week")) %>% 
    pivot_longer(matches("week"), names_to = "endweek", values_to = "ic") %>% 
    mutate(endweek = str_sub(endweek, start = 5))
}

# New York
# lumps utilities + construction
clean_industry_ny <- function() {
  read_sheet(basesheet, sheet = "ny_industry") %>% 
    rename(industry = Industry) %>% 
    mutate(
      stateabb = "NY",
      sector = case_when(
        industry == "Agriculture, forestry, fishing, and hunting" ~ "11",
        industry == "Mining" ~ "21",
        industry == "Construction/utilities" ~ "22-23",
        industry == "Manufacturing" ~ "31-33",
        industry == "Wholesale trade" ~ "42",
        industry == "Retail trade" ~ "44-45",
        industry == "Transportation and warehousing" ~ "48-49",
        industry == "Information" ~ "51",
        industry == "Finance and insurance" ~ "52",
        industry == "Real estate and rental and leasing" ~ "53",
        industry == "Professional, scientific, and technical services" ~ "54",
        industry == "Management of companies and enterprises" ~ "55",
        industry == "Administrative and support services" ~ "56",
        industry == "Educational services" ~ "61",
        industry == "Health care and social assistance" ~ "62",
        industry == "Arts, entertainment and recreation" ~ "71",
        industry == "Accomodation and food services" ~ "72",
        industry == "Other services, except public administration" ~ "81",
        industry == "Public administration (including government)" ~ "92",
        industry == "Unclassified" ~ "99"
      )
    ) %>% 
    filter(!is.na(sector)) %>% 
    select(stateabb, sector, matches("week")) %>% 
    pivot_longer(matches("week"), names_to = "endweek", values_to = "ic") %>% 
    mutate(endweek = str_sub(endweek, start = 5))
}

# North Dakota
# complete
# used tableau to pull everything, then created sheet in basesheet with
# filter(!is.na(Indname) & Periodtypename == "Weekly" & is.na(County) & is.na(Region) & is.na(Gender) & is.na(`Race Ethn`) & Claimtypename == "Initial Claims") %>% select(Indname, `End Date`, Claimants)
clean_industry_nd <- function() {
  read_sheet(basesheet, sheet = "nd_industry") %>% 
    rename(industry = Indname) %>% 
    mutate(
      sector = case_when(
        industry == "Agriculture, Forestry, Fishing and Hunting" ~ "11",
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
        industry == "Professional and Technical Services" ~ "54",
        industry == "Management of Companies and Enterprises" ~ "55",
        industry == "Administrative and Waste Services" ~ "56",
        industry == "Educational Services" ~ "61",
        industry == "Health Care and Social Assistance" ~ "62",
        industry == "Arts, Entertainment and Recreation" ~ "71",
        industry == "Accommodation and Food Services" ~ "72",
        industry == "Other Services" ~ "81",
        industry == "Public Administration" ~ "92",
        industry == "Unclassified" ~ "99"
      )
    ) %>% 
    transmute(
      stateabb = "ND",
      sector = sector,
      endweek = paste0(sprintf("%02d", month(`End Date`)), sprintf("%02d", day(`End Date`))),
      ic = Claimants
    ) %>% 
    filter(as.numeric(endweek) >= 307)
}

# Oregon
# complete
clean_industry_or <- function() {
  read_sheet(basesheet, sheet = "or_industry") %>% 
    rename(industry = Industry) %>% 
    mutate(
      sector = case_when(
        industry == "Agriculture, forestry, fishing, and hunting" ~ "11",
        industry == "Agriculture, Forestry, Fishing and Hunting" ~ "11",
        industry == "Mining" ~ "21",
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
        industry == "Professional and Technical Services" ~ "54",
        industry == "Professional, Scientific, and Technical Services" ~ "54",
        industry == "Management of Companies and Enterprises" ~ "55",
        industry == "Administrative and Waste Services" ~ "56",
        industry == "Administrative and Support and Waste Management and Remediation Services" ~ "56",
        industry == "Educational Services" ~ "61",
        industry == "Health Care and Social Assistance" ~ "62",
        industry == "Arts, Entertainment, and Recreation" ~ "71",
        industry == "Accommodation and Food Services" ~ "72",
        industry == "Other Services" ~ "81",
        industry == "Other Services (except Public Administration)" ~ "81",
        industry == "Other Services excluding Public Administration" ~ "81",
        industry == "Public Administration" ~ "92",
      )
    ) %>% 
    filter(!is.na(sector)) %>% 
    filter(year(Date) == 2020) %>% 
    transmute(
      stateabb = "OR",
      sector = sector,
      endweek = paste0(sprintf("%02d", month(Date)), sprintf("%02d", day(Date))),
      ic = Claims
    ) %>% 
    filter(as.numeric(endweek) >= 307)
}

# Rhode Island
clean_industry_ri <- function() {
  read_sheet(basesheet, sheet = "ri_industry", col_types = "cciiiiiiiii") %>% 
    mutate(stateabb = "RI") %>%
    rename(sector = NAICS) %>% 
    filter(!is.na(sector)) %>% 
    select(stateabb, sector, matches("week")) %>% 
    pivot_longer(matches("week"), names_to = "endweek", values_to = "ic") %>% 
    mutate(endweek = str_sub(endweek, start = 5))
}

# Texas
clean_industry_tx <- function() {
  read_sheet(basesheet, sheet = "tx_industry", col_types = "cciiiiiiiii") %>% 
    mutate(stateabb = "TX") %>%
    rename(sector = NAICS) %>% 
    filter(!is.na(sector)) %>% 
    select(stateabb, sector, matches("week")) %>% 
    pivot_longer(matches("week"), names_to = "endweek", values_to = "ic") %>% 
    mutate(endweek = str_sub(endweek, start = 5))
}

# Washington
# complete
clean_industry_wa <- function() {
  read_sheet(basesheet, sheet = "wa_industry_naics2", col_types = "ciiiiiiii") %>% 
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
    mutate(stateabb = "WA") %>% 
    rename(sector = industry) %>%
    pivot_longer(matches("week"), names_to = "endweek", values_to = "ic") %>% 
    mutate(endweek = str_sub(endweek, start = 5))
}

# Wyoming
# uses supersectors for trans/w/u , fin, pro/bus, lei/hosp, 
# missing data in several places, going to assume zero
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
        # WY includes both "unknown" and "unclassified" industries
        # classifying unkown + unclassified as same sector
        industry == "99 - Unknown" ~ "99",
        industry == "99-Unclassified (99)" ~ "99"
      )
    ) %>% 
    transmute(
      stateabb = "WY",
      sector = sector,
      ic = `Initial Claims`,
      endweek = str_sub(`Week Ending`, start = 5)
    ) %>% 
    mutate(ic = ifelse(is.na(ic), 0, ic)) %>% 
    # WY includes both "unknown" and "unclassified" industries.
    # going to sum them:
    group_by(stateabb, sector, endweek) %>% 
    summarize(ic = sum(ic))
}

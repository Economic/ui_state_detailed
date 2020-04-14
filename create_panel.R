# grab state codes
state_names <- tigris::fips_codes %>% 
  mutate(statefips = as.numeric(state_code)) %>% 
  group_by(statefips) %>% 
  summarize(statename = first(state_name), stateabb = first(state)) 

# grab total IC from state-level reports to DOL
state_total_ic <- read_csv("https://oui.doleta.gov/unemploy/csv/ar539.csv") %>%
  transmute(
    stateabb = st,
    endweek = mdy(rptdate),
    # add initial claims and WS initial claims
    # see https://oui.doleta.gov/dmstree/handbooks/402/402_4/4024c6/4024c6.pdf#ETA539-ar539
    ic_headline = c3 + c7
  ) %>% 
  filter(endweek >= mdy("03-14-20"))

# prepare QCEW data
qcew_total_emp <- read_csv("output/qcew_naics_2digit.csv") %>% 
  mutate(statefips = as.numeric(str_sub(area_fips, end = 2))) %>% 
  group_by(statefips) %>% 
  summarize(emp_state_total = sum(emp))

qcew_all <- read_csv("output/qcew_naics_2digit.csv") %>% 
  # first prepare construction/utilities aggregation
  filter(industry_code %in% c("22", "23")) %>%
  group_by(area_fips) %>% 
  summarize(emp = sum(emp)) %>% 
  ungroup() %>% 
  mutate(industry_code = "22-23") %>% 
  # bind remaining QCEW data
  rbind(.,
    read_csv("output/qcew_naics_2digit.csv"),
    read_csv("output/qcew_naics_highlevel.csv")
  ) %>% 
  transmute(
    statefips = as.numeric(str_sub(area_fips, end = 2)),
    sector = industry_code,
    emp = emp
  ) %>% 
  inner_join(qcew_total_emp, by = "statefips")

# combine UI industry data from several states
thestates <- c(
  "al", "ks", "ma", "me", "mi", "nd", "ne", "nv", "ny", "or", "wa", "wy"
)
ui_data <- map_dfr(thestates, clean_industry_state) %>% 
  mutate(
    month = str_sub(endweek, end = 2), 
    day = str_sub(endweek, start = 3),
    year = 2020
  ) %>% 
  mutate(endweek = mdy(paste(month, day, year, sep = "-"))) %>% 
  select(stateabb, sector, endweek, ic) %>% 
  mutate(sectorname = case_when(
    sector == "11" ~ "Agriculture, Forestry, Fishing, and Hunting",
    sector == "21" ~ "Mining, Quarrying, and Oil and Gas Extraction",
    sector == "22" ~ "Utilities",
    sector == "23" ~ "Construction",
    sector == "31-33" ~ "Manufacturing",
    sector == "42" ~ "Wholesale Trade",
    sector == "44-45" ~ "Retail Trade",
    sector == "48-49" ~ "Transportation and Warehousing",
    sector == "51" ~ "Information",
    sector == "52" ~ "Finance and Insurance",
    sector == "53" ~ "Real Estate and Rental and Leasing",
    sector == "54" ~ "Professional, Scientific, and Technical Services",
    sector == "55" ~ "Management of Companies and Enterprises",
    sector == "56" ~ "Admin. and Support, Waste Mgmt. and Remediation Services",
    sector == "61" ~ "Educational Services",
    sector == "62" ~ "Health Care and Social Assistance",
    sector == "71" ~ "Arts, Entertainment, and Recreation",
    sector == "72" ~ "Accommodation and Food Services",
    sector == "81" ~ "Other Services (except Public Administration)",
    sector == "92" ~ "Public Administration",
    sector == "99" ~ "Unclassified",
    # some states do not entirely use 2-digit NAICS:
    # Wyoming
    sector == "1021" ~ "**Trade, transportation, and utilities**",
    sector == "1023" ~ "**Financial activities**",
    sector == "1024" ~ "**Professional and business services**",
    sector == "1026" ~ "**Leisure and hospitality**",
    # New York
    sector == "22-23" ~ "**Construction & utilities"
  )) %>% 
  inner_join(state_names, by = c("stateabb")) %>% 
  select(stateabb, statefips, statename, endweek, sector, sectorname, ic)

write_csv(ui_data, "output/state_ui_industry_original.csv")

# recode the data by removing unclassified scaling it to match reported state ic totals
ui_data_recoded <- ui_data %>% 
  # filter out unclassified and calculate ic share of total state ic
  filter(!sector == "99") %>% 
  group_by(stateabb, endweek) %>% 
  mutate(ic_share_sumic = ic / sum(ic)) %>% 
  ungroup() %>% 
  # replace ic value with ic share * headline state claims
  inner_join(state_total_ic, by = c("stateabb", "endweek")) %>% 
  mutate(ic = ic_share_sumic * ic_headline) %>% 
  # calculate ic share of employment
  inner_join(qcew_all, by = c("statefips", "sector")) %>% 
  mutate(ic_share_emp = ic / emp) %>% 
  select(stateabb, statename, statefips, sector, sectorname, endweek, ic, ic_share_sumic, ic_headline, emp, ic_share_emp, emp_state_total)

write_csv(ui_data_recoded, "output/state_ui_industry_recoded.csv")

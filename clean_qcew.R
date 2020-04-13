# Read in QCEW and calculate mean quarterly employment
# requires zipped "singlefile" csv from https://www.bls.gov/cew/downloadable-data-files.htm
qcew <- read_csv("input/2019_qtrly_singlefile.zip") %>% 
  # calculate quarterly emp using mean emp across months
  mutate(emp = (month1_emplvl + month2_emplvl + month3_emplvl)/3) %>% 
  filter(qtr == 3) %>% 
  select(area_fips, agglvl_code, industry_code, own_code, emp)

# NAICS 2-digit totals from aggregating across ownership
qcew %>% 
  filter(agglvl_code == 54) %>%
  # sum across ownership
  group_by(area_fips, industry_code) %>% 
  summarize(emp = sum(emp)) %>% 
  # drop unclassified industries 
  filter(industry_code != 99) %>% 
  write_csv("output/qcew_naics_2digit.csv")

# NAICS high-level sector totals from aggregating across ownership
qcew %>% 
  filter(agglvl_code == 53) %>%
  # sum across ownership
  group_by(area_fips, industry_code) %>% 
  summarize(emp = sum(emp)) %>% 
  # drop unclassified industries 
  filter(industry_code != 1029) %>% 
  write_csv("output/qcew_naics_highlevel.csv")

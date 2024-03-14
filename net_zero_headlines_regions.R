install.packages("tidyverse")
install.packages("janitor")
install.packages("usethis")

library(tidyverse)
library(janitor)
library(usethis)
library(sf)

# Import company list and geography files

raw <- read_csv("Data/Net_Zero_Dedup.csv")
clean_names(raw)

geog_look_up <- read_csv("Data/PCD_OA21_LSOA21_MSOA21_LAD_FEB24_UK_LU.csv") %>%
  select(pcd7, pcd8, pcds, ladcd, ladnm)
#distinct(geog_look_up)
clean_names(geog_look_up)

# Need to find region boundaries
lad_boundaries <- read_sf("lad21buc.geojson")
clean_names(lad_boundaries)

# Calculate how many regions they are in
data <- raw %>%
  select(Companynumber, Companyname, BestEstimateCurrentEmployees, 
         BestEstimateCurrentGVA, BestEstimateCurrentTurnover, Localauthoritycodes, Postcodes, TotalInnovateUKFunding,TotalDealroomFundingMillionPounds) %>%
  mutate(No_LAs = floor(nchar(Localauthoritycodes)/9)) %>%
  mutate(No_sites = str_count(Postcodes, ',') + 1) %>%
  mutate(Difference = case_when(No_sites - No_LAs != 0 ~ No_sites - No_LAs, No_sites - No_LAs == 0 ~ 0)) %>%
  mutate(turn_per_site = BestEstimateCurrentTurnover/No_sites) %>%
  mutate(GVA_per_site = BestEstimateCurrentGVA/No_sites) %>%
  mutate(empl_per_site = BestEstimateCurrentEmployees/No_sites) %>%
  mutate(GVA_per_job = BestEstimateCurrentGVA/BestEstimateCurrentEmployees) %>%
  mutate(TotalInnovateUKFunding = replace(TotalInnovateUKFunding,is.na(TotalInnovateUKFunding),0)) %>%
  mutate(Innovate_per_site = TotalInnovateUKFunding/No_sites) %>%
  mutate(TotalDealroomFundingMillionPounds = replace(TotalDealroomFundingMillionPounds,is.na(TotalDealroomFundingMillionPounds),0)) %>%
  mutate(TotalDealroomFunding = TotalDealroomFundingMillionPounds * 10^6) %>%
  mutate(Dealroom_per_site = TotalDealroomFunding/No_sites)

# Checking difference between number of LAs and number of sites
# data2 <- data %>% arrange(desc(Difference))

# Create an observation for each site
sites <- data %>% 
  separate(Postcodes, into = paste0("Postcode", 1 : max(data$No_sites, na.rm = TRUE)), sep = ",") %>%
  
  # Create a row for each business in each LA
  pivot_longer(cols = starts_with("Postcode"), names_to = NULL, values_to = "Postcode", values_drop_na = TRUE)

sites <- left_join(sites, geog_look_up, by = c("Postcode" = "pcds"))
 # merge(x = sites, y = geog_look_up, by.x =  "Postcode", by.y = "pcds", all = FALSE)

geog2 <- read_csv("WD21_LAD21_CTY21_RGN21_CTRY21.csv") %>%
  select(LAD21CD, LAD21NM, RGN21CD, RGN21NM)

geog2 <- distinct(geog2)

sites <- left_join(sites, geog2, by = c("ladcd" = "LAD21CD"))

summary <- sites %>%  
# Produce summary of how many companies in each LA, total turnover, total employees
  group_by(ladnm) %>%
  summarise(No_sites = n(), GVA = sum(GVA_per_site), Turnover = sum(turn_per_site), Employees = sum(empl_per_site), GVA_job = GVA/Employees, 
            InnovateFunding = sum(Innovate_per_site), DealroomFunding = sum(Dealroom_per_site))

regions <- 
  inner_join(sites, lad_boundaries, by = c("ladcd" = "LAD21CD")) %>%
  distinct() %>%
  group_by(RGN21NM) %>%
  summarise(No_sites = n(), GVA = sum(GVA_per_site), Turnover = sum(turn_per_site), Employees = sum(empl_per_site), GVA_job = GVA/Employees, 
            InnovateFunding = sum(Innovate_per_site), DealroomFunding = sum(Dealroom_per_site))
#Output csv of results
# Produce summary table of how many companies in each region and LA, total turnover, total employees
write_excel_csv(regions, "outputs/regions.csv")
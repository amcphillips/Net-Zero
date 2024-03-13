install.packages("tidyverse")
install.packages("janitor")
install.packages("usethis")

library(tidyverse)
library(janitor)
library(usethis)
library(sf)

# Import company list and geography files

raw <- read_csv("Data/Full_Data_Cols.csv")
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
  mutate(Innovate_per_LA = TotalInnovateUKFunding/No_LAs) %>%
  mutate(TotalDealroomFundingMillionPounds = replace(TotalDealroomFundingMillionPounds,is.na(TotalDealroomFundingMillionPounds),0)) %>%
  mutate(TotalDealroomFunding = TotalDealroomFundingMillionPounds * 10^6) %>%
  mutate(Dealroom_per_LA = TotalDealroomFunding/No_LAs)

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









  
# Produce summary of how many companies in each LA, total turnover, total employees
  group_by(LAcode) %>%
  summarise(No_Companies = n(), GVA = sum(GVA_per_LA), Turnover = sum(turn_per_LA), Employees = sum(empl_per_LA), GVA_job = GVA/Employees, 
            InnovateFunding = sum(Innovate_per_LA), DealroomFunding = sum(Dealroom_per_LA))

# Need to add data for geo data for mapping
geog2 <- inner_join(data, geog_look_up, by = c("LAcode" = "LAD21CD"), relationsh)

geog2 <- 
  inner_join(geog2, lad_boundaries, by = c("LAcode" = "LAD21CD")) %>%
  distinct() %>%
  group_by(RGN21NM) %>%
  summarise(sum(No_Companies), sum(GVA), sum(Turnover), sum(Employees), GVA_job = (sum(GVA)/sum(Employees)),
           sum(InnovateFunding), sum(DealroomFunding))
#Output csv of results
# Produce summary table of how many companies in each region and LA, total turnover, total employees
write_excel_csv(geog2, "outputs/regions.csv")
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

geog_look_up <- read_csv("WD21_LAD21_CTY21_RGN21_CTRY21.csv") %>%
  select(LAD21CD, RGN21CD, RGN21NM)
  distinct(geog_look_up)
  clean_names(geog_look_up)
#  dplyr::select(LAD21CD, RGN21CD) 
#  dplyr::distinct()
# dplyr::filter(RGN21CD %in% paste0("E1200000", 1:3))

lad_boundaries <- read_sf("lad21buc.geojson")
clean_names(lad_boundaries)

# Calculate how many LAs they are in

data <- raw %>%
  select(Companynumber, Companyname, BestEstimateCurrentEmployees, 
         BestEstimateCurrentGVA, BestEstimateCurrentTurnover, Localauthoritycodes, Postcodes, TotalInnovateUKFunding,TotalDealroomFundingMillionPounds) %>%
  mutate(No_LAs = floor(nchar(Localauthoritycodes)/9)) %>%
  mutate(No_sites = str_count(Postcodes, ',') + 1) %>%
  mutate(Difference = case_when(No_sites - No_LAs != 0 ~ No_sites - No_LAs, No_sites - No_LAs == 0 ~ 0)) %>%
  mutate(turn_per_LA = BestEstimateCurrentTurnover/No_LAs) %>%
  mutate(GVA_per_LA = BestEstimateCurrentGVA/No_LAs) %>%
  mutate(empl_per_LA = BestEstimateCurrentEmployees/No_LAs) %>%
  mutate(GVA_per_job = BestEstimateCurrentGVA/BestEstimateCurrentEmployees) %>%
  mutate(TotalInnovateUKFunding = replace(TotalInnovateUKFunding,is.na(TotalInnovateUKFunding),0)) %>%
  mutate(Innovate_per_LA = TotalInnovateUKFunding/No_LAs) %>%
  mutate(TotalDealroomFundingMillionPounds = replace(TotalDealroomFundingMillionPounds,is.na(TotalDealroomFundingMillionPounds),0)) %>%
  mutate(TotalDealroomFunding = TotalDealroomFundingMillionPounds * 10^6) %>%
  mutate(Dealroom_per_LA = TotalDealroomFunding/No_LAs)

# Checking difference between number of LAs and number of sites
# data2 <- data %>% arrange(desc(Difference))

# Split each LA into a variable of its own
data <- data %>%  
  separate(Localauthoritycodes, into = paste0("LA", 1 : max(data$No_LAs, na.rm = TRUE)), sep = ",") %>%

# Create a row for each business in each LA
  pivot_longer(cols = starts_with("LA"), names_to = NULL, values_to = "LAcode", values_drop_na = TRUE) %>%

# Produce summary of how many companies in each LA, total turnover, total employees
  group_by(LAcode) %>%
  summarise(No_Companies = n(), GVA = sum(GVA_per_LA), Turnover = sum(turn_per_LA), Employees = sum(empl_per_LA), GVA_job = GVA/Employees,
            InnovateFunding = sum(Innovate_per_LA), DealroomFunding = sum(Dealroom_per_LA))

# Add geography lookups

geog <- inner_join(data, geog_look_up, by = c("LAcode" = "LAD21CD"))

geog <- 
  inner_join(geog, lad_boundaries, by = c("LAcode" = "LAD21CD")) %>%
  distinct()

northern_geog <- geog %>%
  filter(RGN21CD %in% paste0("E1200000", 1:3))

# Produce summary table of how many companies in each region and LA, total turnover, total employees
write_excel_csv(geog[,c("LAD21NM", "RGN21NM", "No_Companies", "GVA", "Turnover", "Employees", "GVA_job", "InnovateFunding", "DealroomFunding")], "outputs/national_LAs.csv")
# Produce sum of how many are in Northern regions
write_excel_csv(northern_geog[,c("LAD21NM", "RGN21NM", "No_Companies", "GVA", "Turnover", "Employees", "GVA_job", "InnovateFunding", "DealroomFunding")], "outputs/northern_LAs.csv")


#creating graphs

ggplot(geog, aes(fill = GVA_job)) + aes(geometry = geometry) + geom_sf() + scale_fill_continuous(type = "viridis") + 
  ggtitle("GVA per job in the Net Zero Economy") + theme_void() #+

ggplot(northern_geog, aes(fill = GVA_job)) + aes(geometry = geometry) + geom_sf() + scale_fill_continuous(type = "viridis") + 
  ggtitle("GVA per job in the Net Zero Economy") + theme_void()
# creates graphs/maps by RTIC
# ggplot2::facet_wrap("RTIC")



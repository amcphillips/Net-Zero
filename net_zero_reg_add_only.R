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

postcodes <- read_csv("Data/ONSPD_FEB_2024_UK.csv") %>%
  select(pcds, oslaua, pcon, lat, long, lep1)

geo_lookup <- read_csv("WD21_LAD21_CTY21_RGN21_CTRY21.csv") %>%
  select(LAD21CD, LAD21NM, RGN21CD, RGN21NM) %>%
  distinct()

national <- raw %>%
  select(Companynumber, Companyname, BestEstimateCurrentEmployees, 
         BestEstimateCurrentGVA, BestEstimateCurrentTurnover, TotalInnovateUKFunding,TotalDealroomFundingMillionPounds) %>%
  summarise(No_Companies = n(), GVA = sum(BestEstimateCurrentGVA), Turnover = sum(BestEstimateCurrentTurnover), Employees = sum(BestEstimateCurrentEmployees), GVA_job = GVA/Employees,
            InnovateFunding = sum(TotalInnovateUKFunding, na.rm = TRUE), DealroomFunding = sum(TotalDealroomFundingMillionPounds, na.rm = TRUE)*10^6)

write_excel_csv(national,"outputs/national.csv")

regions <- raw %>%
  select(Companynumber, Companyname, Registeredpostcode, BestEstimateCurrentEmployees, 
         BestEstimateCurrentGVA, BestEstimateCurrentTurnover, TotalInnovateUKFunding,TotalDealroomFundingMillionPounds)
  
regions <- left_join(regions, postcodes, by = c("Registeredpostcode" = "pcds"))

regions <- left_join(regions, geo_lookup, by = c("oslaua" = "LAD21CD"))
  

regions_summary <- regions %>%
  group_by(RGN21NM) %>%
  summarise(No_Companies = n(), GVA = sum(BestEstimateCurrentGVA), Turnover = sum(BestEstimateCurrentTurnover), Employees = sum(BestEstimateCurrentEmployees), GVA_job = GVA/Employees,
            InnovateFunding = sum(TotalInnovateUKFunding, na.rm = TRUE), DealroomFunding = sum(TotalDealroomFundingMillionPounds, na.rm = TRUE)*10^6)

write_excel_csv(regions_summary,"outputs/region_reg_address.csv")
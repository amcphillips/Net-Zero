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

lad_boundaries <- read_sf("lad21buc.geojson")
clean_names(lad_boundaries)

national <- raw %>%
  select(Companynumber, Companyname, BestEstimateCurrentEmployees, 
         BestEstimateCurrentGVA, BestEstimateCurrentTurnover, TotalInnovateUKFunding,TotalDealroomFundingMillionPounds) %>%
  summarise(No_Companies = n(), GVA = sum(BestEstimateCurrentGVA), Turnover = sum(BestEstimateCurrentTurnover), Employees = sum(BestEstimateCurrentEmployees),
            InnovateFunding = sum(TotalInnovateUKFunding, na.rm = TRUE), DealroomFunding = sum(TotalDealroomFundingMillionPounds, na.rm = TRUE)*10^6)

write_excel_csv(national,"outputs/national.csv")

national_GVA_per_job <- raw %>%
  select(Companynumber, Companyname, BestEstimateCurrentEmployees, BestEstimateCurrentGVA)  %>%
  filter(BestEstimateCurrentGVA>0, BestEstimateCurrentEmployees>0) %>%
  mutate(GVAPerJob = BestEstimateCurrentGVA/BestEstimateCurrentEmployees) %>%
  summarise(No_Companies = n(), GVAPerJob = sum(BestEstimateCurrentGVA)/sum(BestEstimateCurrentEmployees))

write_excel_csv(national_GVA_per_job,"outputs/national_GVA_per_job.csv")

regions <- raw %>%
  select(Companynumber, Companyname, Registeredpostcode, BestEstimateCurrentEmployees, 
         BestEstimateCurrentGVA, BestEstimateCurrentTurnover, TotalInnovateUKFunding,TotalDealroomFundingMillionPounds)
  
regions <- left_join(regions, postcodes, by = c("Registeredpostcode" = "pcds"))

regions <- left_join(regions, geo_lookup, by = c("oslaua" = "LAD21CD"))

regions_summary <- regions %>%
  group_by(RGN21NM) %>%
  summarise(No_Companies = n(), GVA = sum(BestEstimateCurrentGVA), Turnover = sum(BestEstimateCurrentTurnover), Employees = sum(BestEstimateCurrentEmployees),
            InnovateFunding = sum(TotalInnovateUKFunding, na.rm = TRUE), DealroomFunding = sum(TotalDealroomFundingMillionPounds, na.rm = TRUE)*10^6)

write_excel_csv(regions_summary,"outputs/region_reg_address.csv")

regions_GVA_per_job <-regions %>%
  filter(BestEstimateCurrentGVA>0, BestEstimateCurrentEmployees>0) %>%
  mutate(GVAPerJob = BestEstimateCurrentGVA/BestEstimateCurrentEmployees) %>%
  group_by(RGN21NM) %>%
  summarise(No_Companies = n(), GVAPerJob = sum(BestEstimateCurrentGVA)/sum(BestEstimateCurrentEmployees))

write_excel_csv(regions_GVA_per_job,"outputs/region_reg_address_GVA_per_job.csv")

LA_summary <- regions %>%
  group_by(LAD21NM) %>%
  summarise(No_Companies = n(), GVA = sum(BestEstimateCurrentGVA), Turnover = sum(BestEstimateCurrentTurnover), Employees = sum(BestEstimateCurrentEmployees),
            InnovateFunding = sum(TotalInnovateUKFunding, na.rm = TRUE), DealroomFunding = sum(TotalDealroomFundingMillionPounds, na.rm = TRUE)*10^6)

write_excel_csv(LA_summary,"outputs/LAs_reg_address.csv")

LA_GVA_per_job <-regions %>%
  filter(BestEstimateCurrentGVA>0, BestEstimateCurrentEmployees>0) %>%
  mutate(GVAPerJob = BestEstimateCurrentGVA/BestEstimateCurrentEmployees) %>%
  group_by(LAD21NM) %>%
  summarise(No_Companies = n(), GVAPerJob = sum(BestEstimateCurrentGVA)/sum(BestEstimateCurrentEmployees))

write_excel_csv(LA_GVA_per_job,"outputs/LAs_reg_address_GVA_per_job.csv")

map_data <- 
  right_join(LA_summary, lad_boundaries, by = c("LAD21NM" = "LAD21NM")) %>%
  replace(is.na(.), 0) %>%
  distinct()


ggplot(map_data, aes(fill = log(GVA))) + aes(geometry = geometry) + geom_sf() + scale_fill_continuous() + 
  ggtitle("GVA by registered address\n of Net Zero companies") + theme_void()

ggsave("outputs/GVA.png")

ggplot(map_data, aes(fill = log(No_Companies))) + aes(geometry = geometry) + geom_sf() + scale_fill_continuous() + 
  ggtitle("Registered address of\n Net Zero companies") + theme_void()

ggsave("outputs/companies.png")

ggplot(map_data, aes(fill = log(Employees))) + aes(geometry = geometry) + geom_sf() + scale_fill_continuous() + 
  ggtitle("Number of employees by registered\n address of Net Zero companies") + theme_void()

ggsave("outputs/employees.png")

map_data2 <- 
  right_join(LA_GVA_per_job, lad_boundaries, by = c("LAD21NM" = "LAD21NM")) %>%
  replace(is.na(.), 0) %>%
  distinct()

ggplot(map_data2, aes(fill = log(GVAPerJob))) + aes(geometry = geometry) + geom_sf() + scale_fill_continuous() + 
  ggtitle("GVA per job by registered address\n of Net Zero companies") + theme_void()

ggsave("outputs/GVA_per_job.png")
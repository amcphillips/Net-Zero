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

postcodes <- read_csv("Data/PCD_OA21_LSOA21_MSOA21_LAD_FEB24_UK_LU.csv") %>%
  select(pcds, ladcd, ladnm)

geo_lookup <- read_csv("WD_to_LAD_to_CTY_to_RGN_to_CTRY_2023.csv") %>%
  select(LAD23CD, LAD23NM, RGN23CD, RGN23NM) %>%
  distinct()

lad_boundaries <- read_sf("lad23buc.geojson")
clean_names(lad_boundaries)

ITL_lookup <- read_csv("Data/LAD23_LAU1_ITL3_to_ITL2_ITL1_amended.csv")
clean_names(ITL_lookup)

ITL2_boundaries <- read_sf("Data/ITL2_Jan_2021_UK_BGC_V2_2022.geojson")
clean_names(ITL2_boundaries)

national <- raw %>%
  select(Companynumber, Companyname, BestEstimateCurrentEmployees, 
         BestEstimateCurrentGVA, BestEstimateCurrentTurnover, TotalInnovateUKFunding,TotalDealroomFundingMillionPounds) %>%
  summarise(No_Companies = n(), GVA = sum(BestEstimateCurrentGVA), Turnover = sum(BestEstimateCurrentTurnover), Employees = sum(BestEstimateCurrentEmployees),
            InnovateFunding = sum(TotalInnovateUKFunding, na.rm = TRUE), DealroomFunding = sum(TotalDealroomFundingMillionPounds, na.rm = TRUE)*10^6)

#write_excel_csv(national,"outputs/national.csv")

national_GVA_per_job <- raw %>%
  select(Companynumber, Companyname, BestEstimateCurrentEmployees, BestEstimateCurrentGVA)  %>%
  filter(BestEstimateCurrentGVA>0, BestEstimateCurrentEmployees>0) %>%
  mutate(GVAPerJob = BestEstimateCurrentGVA/BestEstimateCurrentEmployees) %>%
  summarise(No_Companies = n(), GVAPerJob = sum(BestEstimateCurrentGVA)/sum(BestEstimateCurrentEmployees))

#write_excel_csv(national_GVA_per_job,"outputs/national_GVA_per_job.csv")

regions <- raw %>%
  select(Companynumber, Companyname, Registeredpostcode, BestEstimateCurrentEmployees, 
         BestEstimateCurrentGVA, BestEstimateCurrentTurnover, TotalInnovateUKFunding,TotalDealroomFundingMillionPounds)
  
regions <- left_join(regions, postcodes, by = c("Registeredpostcode" = "pcds"))

regions <- left_join(regions, geo_lookup, by = c("ladcd" = "LAD23CD"))

regions <- left_join(regions, ITL_lookup, by = c("ladcd" = "LAD23CD"))

regions_summary <- regions %>%
  group_by(RGN23NM) %>%
  summarise(No_Companies = n(), GVA = sum(BestEstimateCurrentGVA), Turnover = sum(BestEstimateCurrentTurnover), Employees = sum(BestEstimateCurrentEmployees),
            InnovateFunding = sum(TotalInnovateUKFunding, na.rm = TRUE), DealroomFunding = sum(TotalDealroomFundingMillionPounds, na.rm = TRUE)*10^6)

#write_excel_csv(regions_summary,"outputs/region_reg_address.csv")

regions_GVA_per_job <-regions %>%
  filter(BestEstimateCurrentGVA>0, BestEstimateCurrentEmployees>0) %>%
  mutate(GVAPerJob = BestEstimateCurrentGVA/BestEstimateCurrentEmployees) %>%
  group_by(RGN23NM) %>%
  summarise(No_Companies = n(), GVAPerJob = sum(BestEstimateCurrentGVA)/sum(BestEstimateCurrentEmployees))

#write_excel_csv(regions_GVA_per_job,"outputs/region_reg_address_GVA_per_job.csv")

  ITL2_summary <- regions %>%
  group_by(ITL221NM) %>%
  summarise(No_Companies = n(), GVA = sum(BestEstimateCurrentGVA), Turnover = sum(BestEstimateCurrentTurnover), Employees = sum(BestEstimateCurrentEmployees),
            InnovateFunding = sum(TotalInnovateUKFunding, na.rm = TRUE), DealroomFunding = sum(TotalDealroomFundingMillionPounds, na.rm = TRUE)*10^6)

write_excel_csv(ITL2_summary,"outputs/ITL2_reg_address.csv")

ITL2_GVA_per_job <-regions %>%
  filter(BestEstimateCurrentGVA>0, BestEstimateCurrentEmployees>0) %>%
  mutate(GVAPerJob = BestEstimateCurrentGVA/BestEstimateCurrentEmployees) %>%
  group_by(ITL221NM) %>%
  summarise(No_Companies = n(), GVAPerJob = sum(BestEstimateCurrentGVA)/sum(BestEstimateCurrentEmployees))

write_excel_csv(ITL2_GVA_per_job,"outputs/ITL2_reg_address_GVA_per_job.csv")

map_data <- 
  right_join(ITL2_summary, ITL2_boundaries, by = c("ITL221NM" = "ITL221NM")) %>%
  replace(is.na(.), 0) %>%
  distinct()


ggplot(map_data, aes(fill = GVA)) + aes(geometry = geometry) + geom_sf() + scale_fill_continuous() + 
  ggtitle("GVA by registered address\n of Net Zero companies") + theme_void()

ggsave("outputs/reg_add_ITL2_GVA.png")

ggplot(map_data, aes(fill = No_Companies)) + aes(geometry = geometry) + geom_sf() + scale_fill_continuous() + 
  ggtitle("Registered address of\n Net Zero companies") + theme_void()

ggsave("outputs/reg_add_ITL2_companies.png")

ggplot(map_data, aes(fill = Employees)) + aes(geometry = geometry) + geom_sf() + scale_fill_continuous() + 
  ggtitle("Number of employees by registered\n address of Net Zero companies") + theme_void()

ggsave("outputs/reg_add_ITL2_employees.png")

map_data2 <- 
  right_join(ITL2_GVA_per_job, ITL2_boundaries, by = c("ITL221NM" = "ITL221NM")) %>%
  replace(is.na(.), 0) %>%
  distinct()

ggplot(map_data2, aes(fill = GVAPerJob)) + aes(geometry = geometry) + geom_sf() + scale_fill_continuous() + 
  ggtitle("GVA per job by registered address\n of Net Zero companies") + theme_void()

ggsave("outputs/reg_add_ITL2_GVA_per_job.png")

north <- regions %>%
  filter(RGN23CD == "E12000001" | RGN23CD == "E12000002" | RGN23CD == "E12000003") %>%
  group_by(ITL221NM) %>%
  summarise(No_Companies = n(), GVA = sum(BestEstimateCurrentGVA), Turnover = sum(BestEstimateCurrentTurnover), Employees = sum(BestEstimateCurrentEmployees),
            InnovateFunding = sum(TotalInnovateUKFunding, na.rm = TRUE), DealroomFunding = sum(TotalDealroomFundingMillionPounds, na.rm = TRUE)*10^6)

write_excel_csv(north,"outputs/North_ITL2_reg_address.csv")

north_map_data <- 
  left_join(north, ITL2_boundaries, by = c("ITL221NM" = "ITL221NM")) %>%
  replace(is.na(.), 0) %>%
  distinct()

ggplot(north_map_data, aes(fill = GVA)) + aes(geometry = geometry) + geom_sf() + scale_fill_continuous() + 
  ggtitle("GVA by registered address\n of Net Zero companies") + theme_void()

ggsave("outputs/reg_add_ITL2_north_GVA.png")

ggplot(north_map_data, aes(fill = No_Companies)) + aes(geometry = geometry) + geom_sf() + scale_fill_continuous() + 
  ggtitle("Registered address of\n Net Zero companies") + theme_void()

ggsave("outputs/reg_add_ITL2_north_companies.png")

ggplot(north_map_data, aes(fill = Employees)) + aes(geometry = geometry) + geom_sf() + scale_fill_continuous() + 
  ggtitle("Number of employees by registered\n address of Net Zero companies") + theme_void()

ggsave("outputs/reg_add_ITL2_north_employees.png")

North_GVA_per_job <-regions %>%
  filter(BestEstimateCurrentGVA>0, BestEstimateCurrentEmployees>0, RGN23CD == "E12000001" | RGN23CD == "E12000002" | RGN23CD == "E12000003") %>%
  mutate(GVAPerJob = BestEstimateCurrentGVA/BestEstimateCurrentEmployees) %>%
  group_by(ITL221NM) %>%
  summarise(No_Companies = n(), GVAPerJob = sum(BestEstimateCurrentGVA)/sum(BestEstimateCurrentEmployees))

write_excel_csv(North_GVA_per_job,"outputs/North_reg_address_GVA_per_job.csv")

north_map_data2 <- 
  left_join(North_GVA_per_job, ITL2_boundaries, by = c("ITL221NM" = "ITL221NM")) %>%
  replace(is.na(.), 0) %>%
  distinct()

ggplot(north_map_data2, aes(fill = GVAPerJob)) + aes(geometry = geometry) + geom_sf() + scale_fill_continuous() + 
  ggtitle("GVA per job by registered address\n of Net Zero companies") + theme_void()

ggsave("outputs/north_GVA_per_job.png")

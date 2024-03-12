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

national <- raw %>%
  select(Companynumber, Companyname, BestEstimateCurrentEmployees, 
         BestEstimateCurrentGVA, BestEstimateCurrentTurnover, TotalInnovateUKFunding,TotalDealroomFundingMillionPounds) %>%
  summarise(No_Companies = n(), GVA = sum(BestEstimateCurrentGVA), Turnover = sum(BestEstimateCurrentTurnover), Employees = sum(BestEstimateCurrentEmployees), GVA_job = GVA/Employees,
            InnovateFunding = sum(TotalInnovateUKFunding, na.rm = TRUE), DealroomFunding = sum(TotalDealroomFundingMillionPounds, na.rm = TRUE)*10^6)

write_excel_csv(national,"outputs/national.csv")

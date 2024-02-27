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

# Need to find region boundaries
lad_boundaries <- read_sf("lad21buc.geojson")
clean_names(lad_boundaries)

# Calculate how many regions they are in
data <- raw %>%
  select(Companynumber, Companyname, BestEstimateCurrentEmployees, 
         BestEstimateCurrentGVA, BestEstimateCurrentTurnover, Localauthoritycodes, Postcodes) %>%
  mutate(No_LAs = floor(nchar(Localauthoritycodes)/9)) %>%
  mutate(No_sites = str_count(Postcodes, ',') + 1) %>%
  mutate(Difference = case_when(No_sites - No_LAs != 0 ~ No_sites - No_LAs, No_sites - No_LAs == 0 ~ 0)) %>%
  mutate(turn_per_LA = BestEstimateCurrentTurnover/No_LAs) %>%
  mutate(GVA_per_LA = BestEstimateCurrentGVA/No_LAs) %>%
  mutate(empl_per_LA = BestEstimateCurrentEmployees/No_LAs) %>%
  mutate(GVA_per_job = BestEstimateCurrentGVA/BestEstimateCurrentEmployees)

# Checking difference between number of LAs and number of sites
# data2 <- data %>% arrange(desc(Difference))

# Split each LA into a variable of its own
data <- data %>%  
  separate(Localauthoritycodes, into = paste0("LA", 1 : max(data$No_LAs, na.rm = TRUE)), sep = ",") %>%
  
  # Create a row for each business in each LA
  pivot_longer(cols = starts_with("LA"), names_to = NULL, values_to = "LAcode", values_drop_na = TRUE) %>%
  
  # Produce summary of how many companies in each LA, total turnover, total employees
  group_by(LAcode) %>%
  summarise(No_Companies = n(), GVA = sum(GVA_per_LA), Turnover = sum(turn_per_LA), Employees = sum(empl_per_LA), GVA_job = GVA/Employees)

# Need to add data for geo data for mapping
geog2 <- inner_join(data, geog_look_up, by = c("LAcode" = "LAD21CD"))

geog2 <- 
  inner_join(geog2, lad_boundaries, by = c("LAcode" = "LAD21CD")) %>%
  distinct() %>%
  group_by(RGN21NM) %>%
  summarise(sum(No_Companies), sum(GVA), sum(Turnover), sum(Employees), GVA_job = (sum(GVA)/sum(Employees)))
#Output csv of results
# Produce summary table of how many companies in each region and LA, total turnover, total employees
write_excel_csv(geog2, "outputs/regions.csv")
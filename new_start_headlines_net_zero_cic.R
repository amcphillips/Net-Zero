install.packages("tidyverse")
install.packages("janitor")
install.packages("usethis")

library(tidyverse)
library(janitor)
library(usethis)
library(sf)

# Import company list and geography files

raw <- read_csv("Data/basiccompanieslist_financialsColumnLayout_.csv")
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
  select(-LinkedIn, -Facebook, -Twitter, -Instagram, -Youtube, -LastKnownDealroomFunding) %>%
  mutate(No_LAs = floor(nchar(Localauthoritycodes)/9)) %>%
  mutate(No_sites = str_count(Postcodes, ',') + 1) %>%
  mutate(Difference = case_when(No_sites - No_LAs != 0 ~ No_sites - No_LAs, No_sites - No_LAs == 0 ~ 0)) %>%
  mutate(turn_per_LA = BestEstimateCurrentTurnover/No_LAs) %>%
  mutate(empl_per_LA = BestEstimateCurrentEmployees/No_LAs)

# Checking difference between number of LAs and number of sites
# data2 <- data %>% arrange(desc(Difference))

# Split each LA into a variable of its own
data <- data %>%  
  separate(Localauthoritycodes, into = paste0("LA", 1 : max(data$No_LAs, na.rm = TRUE)), sep = ",") %>%

# Create a row for each business in each LA
  pivot_longer(cols = starts_with("LA"), names_to = NULL, values_to = "LAcode", values_drop_na = TRUE) %>%

# Produce summary of how many companies in each LA, total turnover, total employees
  group_by(LAcode) %>%
  summarise(No_Companies = n(), Turnover = sum(turn_per_LA), Employees = sum(empl_per_LA))

# Add geography lookups

geog <- inner_join(data, geog_look_up, by = c("LAcode" = "LAD21CD"))

geog <- 
  inner_join(geog, lad_boundaries, by = c("LAcode" = "LAD21CD")) %>%
  distinct()

northern_geog <- geog %>%
  filter(RGN21CD %in% paste0("E1200000", 1:3))

# Produce summary table of how many companies in each region and LA, total turnover, total employees
write_excel_csv(geog[,c("LAD21NM", "RGN21NM", "No_Companies", "Employees", "Turnover")], "outputs/national.csv")
# Produce sum of how many are in Northern regions
write_excel_csv(northern_geog[,c("LAD21NM", "RGN21NM", "No_Companies", "Employees", "Turnover")], "outputs/northern.csv")


#creating graphs

ggplot(geog, aes(fill = log(No_Companies))) + aes(geometry = geometry) + geom_sf() + scale_fill_continuous(type = "viridis") +
 theme_void() #+

ggplot(northern_geog, aes(fill = log(No_Companies))) + aes(geometry = geometry) + geom_sf() + scale_fill_continuous(type = "viridis") +
  theme_void()
# creates graphs/maps by RTIC
# ggplot2::facet_wrap("RTIC")



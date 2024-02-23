data <- readr::read_csv("basiccompanieslist_financialsColumnLayout_.csv")

lad21_rgn21 <- readr::read_csv("WD21_LAD21_CTY21_RGN21_CTRY21.csv") |>
  dplyr::select(LAD21CD, RGN21CD) 
#  dplyr::distinct()
 # dplyr::filter(RGN21CD %in% paste0("E1200000", 1:3))

lad21 <- sf::read_sf("lad21buc.geojson")
# businesses by rtic
# what are :: and |> used for?

businesses_rtic <- data |>
    #do we actually need to select or just use whole dataset?
    dplyr::select(Companynumber, Companyname, Localauthoritycodes, RTICs) |>
  # needs refining
  # calculates how many LAs the company is located in. Divide by 9?
  dplyr::mutate(noOfLAs = floor(nchar(Localauthoritycodes) / 8)) |>
  # check RTICs don't include ',' in name, split put individual RTICs
  tidyr::separate(RTICs, into = paste0("RTIC", 1:10), sep = ",") |>
  # turns each RTIC into new row?
  tidyr::pivot_longer(dplyr::starts_with("RTIC"), names_to = NULL, values_to = "RTIC", values_drop_na = TRUE) |>
  # remove any spaces
  dplyr::mutate(RTIC = stringr::str_trim(RTIC)) |>
  # only get unique rows, but why is it needed?
  dplyr::distinct() |>
  # filters to only inlude companies that have stated RTICs
  # dplyr::filter(grepl("Net Zero|Energy Generation", RTIC)) |>
  # create new column for each LA company appears in
  tidyr::separate(Localauthoritycodes, into = paste0("la", 1:345), sep = ",") |>
  # turn new LA columns into new row
  tidyr::pivot_longer(cols = dplyr::starts_with("la"), names_to = NULL, values_to = "LAcode", values_drop_na = TRUE) |>
  # double check
  # sets data groupings up for summarising
  dplyr::group_by(RTIC, LAcode) |>
  # produces summary table of number of businesses in each LA by RTIC
  dplyr::summarise(n = dplyr::n())

# joins data above to LA to region lookup data
businesses_rtic_north <- dplyr::inner_join(businesses_rtic, lad21_rgn21, by = c("LAcode" = "LAD21CD"))
# appends data to geogrpahy file for boundaries?
businesses_rtic_north_final <- dplyr::inner_join(lad21, businesses_rtic_north, by = c("LAD21CD" = "LAcode"))

#creating graphs
ggplot2::ggplot(businesses_rtic_north_final,
                # created fill colour based on log(n) results
                ggplot2::aes(fill = log(n))) +
  # pacakge to create maps?
  ggplot2::geom_sf() +
  # colour palette?
  ggplot2::scale_fill_continuous(type = "viridis") +
  # removes unnecessary background elements
  ggplot2::theme_minimal() +
  # creates graphs/maps by RTIC
  ggplot2::facet_wrap("RTIC")
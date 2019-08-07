earthquakes <- system.file("extdata", "NOAA_data.txt", package="Rthquake") %>%
  readr::read_delim(delim = "\t")

test_that("Result is a leaflet", {
  DF <- earthquakes %>%
    eq_clean_data() %>%
    filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
    eq_map(annot_col = "popup_text")

  expect_s3_class(DF, "leaflet")
}
)

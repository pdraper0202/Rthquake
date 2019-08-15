context("Test eq_location_clean")

earthquakes <- system.file("extdata", "NOAA_data.txt", package="Rthquake") %>%
  readr::read_delim(delim = "\t")

test_that("Result is a dataframe", {
  DF <- earthquakes %>%
    eq_clean_data() %>%
    eq_location_clean()

  expect_s3_class(DF, "data.frame")
}
)

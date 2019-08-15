context("Test eq_clean_data")

earthquakes <- system.file("extdata", "NOAA_data.txt", package="Rthquake") %>%
  readr::read_delim(delim = "\t")

test_that("Result is a dataframe", {
          DF <- earthquakes %>%
            eq_clean_data()

          expect_s3_class(DF, "data.frame")
          }
          )

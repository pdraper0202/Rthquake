earthquakes <- system.file("extdata", "NOAA_data.txt", package="Rthquake") %>%
  readr::read_delim(delim = "\t")

test_that("Result is a ggplot", {
  DF <- earthquakes %>%
    eq_clean_data() %>%
    eq_location_clean() %>%
    filter(COUNTRY %in% c("USA", "CHINA", "AFGHANISTAN")) %>%
    filter(DATE > "2000-01-01") %>%
    ggplot() +
    geom_timeline(aes(x = DATE,
                      y = COUNTRY,
                      color = TOTAL_DEATHS,
                      size = EQ_PRIMARY,
                      scaling_factor = 1
    )
    ) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_size_continuous(name = 'Richter scale value') +
    scale_color_continuous(name = '# of Deaths')

  expect_s3_class(DF, "ggplot")
}
)

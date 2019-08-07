#' Function to clean earthquake data
#'
#' \code{eq_clean_data} takes an NOAA raw earthquake data file and processes it for further analysis.
#'
#' @param rawdata An NOAA tab-delimited earthquake file.
#'
#' @return A dataframe.
#'   The dataframe returned is processed to be used with \code{\link{geom_timeline}} and \code{\link{geom_timeline_label}}.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom lubridate ymd
#'
#' @examples
#'
#' \dontrun{
#' data(earthquakes)
#' earthquakes_cleaned <- eq_clean_data(earthquakes)
#' }
#'
#' @export
eq_clean_data <- function(rawdata) {

  rawdata %>%
    # Remove dates B.C.E
    dplyr::filter(YEAR > 0) %>%
    # Convert latitude and longitude to numeric
    dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE),
                  LATITUDE = as.numeric(LATITUDE)
                  ) %>%
    # Replace missing dates
    dplyr::mutate(MONTH = ifelse(is.na(MONTH), 1L, MONTH),
                  DAY = ifelse(is.na(DAY), 1L, DAY)
                  ) %>%
    # Reformat date columns
    dplyr::mutate(YEAR = sprintf("%+05d", as.integer(YEAR)),
                  MONTH =  sprintf("%02d", as.integer(MONTH)),
                  DAY =  sprintf("%02d", as.integer(DAY))
                  ) %>%
    # Create a date column
    dplyr::mutate(DATE = lubridate::ymd(paste(YEAR, MONTH, DAY, sep = "_"))) %>%
    # Adjust data types of columns for geom_timeline
    dplyr::mutate(EQ_PRIMARY = as.numeric(EQ_PRIMARY),
                  TOTAL_DEATHS = as.numeric(TOTAL_DEATHS)
                  )
}



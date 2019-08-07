#' Clean LOCATION Variable
#'
#' \code{eq_location_clean} cleans the \code{LOCATION_NAME} variable in an NOAA earthquakes data file.
#' Country name and white-space is removed, and the name is converted to title-case.
#'
#' @param rawdata An NOAA tab-delimited earthquake file.
#'
#' @return A dataframe.
#'   The dataframe returned is processed to be used with \code{\link{geom_timeline}} and \code{\link{geom_timeline_label}}.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_to_title
#' @importFrom stringr str_replace
#'
#' @examples
#'
#' \dontrun{
#' data(earthquakes)
#' earthquakes_cleaned <- earthquakes %>% eq_clean_data() %>% eq_location_clean()
#' }
#'
#' @export
eq_location_clean <- function (rawdata) {
  rawdata %>%
    # Remove country name and colon, convert to title case
    dplyr::mutate(LOCATION_NAME = stringr::str_to_title(stringr::str_replace(LOCATION_NAME, ".*:\\s+(.*)", "\\1"))) %>%
    # Remove any whitespace
    dplyr::mutate(LOCATION_NAME = stringr::str_trim(LOCATION_NAME))
}

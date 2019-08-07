#' Earthquake Leaflet Map
#'
#' \code{eq_map} generates a \code{\link[leaflet]{leaflet}} map displaying the geographical position of earthquakes
#' in a NOAA earthquakes data file.
#'
#' The size of the circles displayed are proportional to the earthquake magnitude.
#' Clicking on a given circle will display the associated value of the variable specified by \code{annot_col}.
#'
#' @param data An NOAA earthquakes data file
#' @param annot_col Character. The name of the variable to display in the annotation.
#'
#' @return An interactive leaflet map displaying earthquake locations.
#'
#' @importFrom magrittr extract2
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#'
#' @examples
#'
#' \dontrun{
#' data(earthquakes)
#' earthquakes %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#' eq_map(annot_col = "DATE")
#' }
#'
#' @export
eq_map <- function (data, annot_col = "DATE") {

  labels <-  data %>%
    magrittr::extract2(annot_col) %>%
    as.character

  leaflet::leaflet(data = data) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng = ~ LONGITUDE,
                     lat = ~ LATITUDE,
                     radius = ~ EQ_PRIMARY,
                     popup = labels,
                     weight = 3
                     )
}

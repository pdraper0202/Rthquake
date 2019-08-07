#' Leaflet Map Annotation Label
#'
#' \code{eq_create_label} generates a more informative annotation label for the leaflet map generated
#' with \code{\link{eq_map}}.
#'
#' The fields displayed are LOCATION, SIZE, and DEATHS
#'
#' @param data An NOAA earthquake data file.
#'
#' @importFrom purrrlyr by_row
#'
#' @examples
#'
#' \dontrun{
#' data(earthquakes)
#' earthquakes %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#' dplyr::mutate(popup_text = eq_create_label(.)) %>%
#' eq_map(annot_col = "popup_text")
#' }
#'
#' @export
eq_create_label <- function (data) {

  data %>%

    purrrlyr::by_row(function (row) {
      LOCATION = ifelse(
        !is.na(row$LOCATION_NAME),
        sprintf("<b>Location: </b>%s<br>", row$LOCATION_NAME),
        NA
      )
      SIZE = ifelse(
        !is.na(row$EQ_PRIMARY),
        sprintf("<b>Magnitude: </b>%s<br>", row$EQ_PRIMARY),
        NA
      )
      DEATHS = ifelse(
        !is.na(row$TOTAL_DEATHS),
        sprintf("<b>Total deaths: </b>%s<br>", row$TOTAL_DEATHS),
        NA
      )
      c(LOCATION, SIZE, DEATHS) %>%
        na.omit() %>%
        paste(collapse = "") %>%
        return
    },
    .to = "out") %>%

    dplyr::select("out") %>%

    unlist
}

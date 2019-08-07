#' GeomTimeline
#'
#' see \code{geom_timeline}
#'
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline",

                                 ggplot2::Geom,

                                 required_aes = "x",

                                 default_aes = ggplot2::aes(y = 0,
                                                            size = 1.5,
                                                            color = "grey",
                                                            alpha = 0.5,
                                                            shape = 19,
                                                            stroke = 0.5,
                                                            fill = NA,
                                                            scaling_factor = 1.
                                                            ),

                                 draw_key = ggplot2::draw_key_point,

                                 draw_panel = function(data,
                                                       panel_scales,
                                                       coord) {

                                   coords <- coord$transform(data, panel_scales)

                                   coords$size <- coords$scaling_factor * coords$size / max(coords$size)

                                   # Horizontal line coordinates
                                   xline_coords <- data.frame(x = rep(0:1, times = length(unique(coords$y))),
                                                              y = c(unique(coords$y), unique(coords$y))
                                                              )
                                   xline_coords$id = rep(1:(nrow(xline_coords) / 2), 2)

                                   # Add the horizontal line grob
                                   line_grob <- grid::polylineGrob(x = xline_coords$x,
                                                                   y = xline_coords$y,
                                                                   id = xline_coords$id,
                                                                   gp = grid::gpar(col = "grey",
                                                                                   alpha = 0.25,
                                                                                   lwd = 3
                                                                                   )
                                                                   )

                                   # Add the points grob
                                   points_grob <- grid::pointsGrob(x = coords$x,
                                                                   y = coords$y,
                                                                   pch = coords$shape,
                                                                   gp = grid::gpar(col = coords$colour,
                                                                                   alpha = coords$alpha,
                                                                                   cex = coords$size
                                                                                   )
                                                                   )

                                   grid::grobTree(line_grob, points_grob)
                                 }
)

#' Earthquake Timeline
#'
#' \code{geom_timeline} displays a timeline of earthquakes. Date is displayed on the x-axis and countries are displayed
#' on the y-axis as individual timelines. The size of the point is proportional to earthquake magnitude, and the color
#' is related to the number of deaths resulting from the earthquake.
#'
#' @param mapping A set of aesthetics created by \code{aes()}.
#' @param data The NOAA earthquake data to be displayed.
#' @param stat The statistical transformation for that layer.
#' @param position Position adjustment.
#' @param ... Other arguments passed to \code{layer()}.
#' @param na.rm Option to remove missing values.
#' @param show.legend Option to display legends.
#' @param inherit.aes Option to override the default aesthetics.
#'
#' @section Aesthetics:
#' \code{geom_timeline} can be used with the following aesthetics:
#' \itemize{
#'  \item x
#'  \item y
#'  \item size
#'  \item color
#'  \item alpha
#'  \item shape
#'  \item stroke
#'  \item fill
#' }
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_point
#' @importFrom grid polylineGrob
#' @importFrom grid pointsGrob
#' @importFrom grid gpar
#' @importFrom grid grobTree
#'
#'
#' @examples
#'
#' \dontrun{
#' data(earthquakes)
#' earthquakes %>%
#' eq_clean_data() %>%
#' eq_location_clean() %>%
#' dplyr::filter(COUNTRY %in% c("USA", "CHINA", "AFGHANISTAN")) %>%
#' dplyr::filter(DATE > "2000-01-01") %>%
#' ggplot() +
#' geom_timeline(aes(x = DATE,
#'                   y = COUNTRY,
#'                   color = TOTAL_DEATHS,
#'                   size = EQ_PRIMARY,
#'                   scaling_factor = 1
#' )
#' )
#' }
#'
#' @export
geom_timeline <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...
                          ) {

  ggplot2::layer(geom = GeomTimeline,
                 mapping = mapping,
                 data = data,
                 stat = stat,
                 position = position,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...)
                 )
}

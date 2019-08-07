#' GeomTimelineLabel
#'
#' see \code{geom_timeline_label}
#'
#' @export
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel",

                                      ggplot2::Geom,

                                      required_aes = c("x", "label", "magnitude"),

                                      default_aes = ggplot2::aes(y = 0,
                                                                 n_max = 7,
                                                                 angle = 45,
                                                                 line_height = 0.1
                                                                 ),

                                      draw_panel = function(data,
                                                            panel_scales,
                                                            coord) {

                                        coords <- coord$transform(data, panel_scales)

                                        # Filter down to n_max results (per country)
                                        coords <- coords %>%
                                          dplyr::arrange(desc(magnitude)) %>%
                                          dplyr::group_by(y) %>%
                                          dplyr::top_n(.$n_max[1], magnitude)

                                        # Vertical lines data
                                        coords$yend <- coords$y + coords$line_height
                                        coords$id <- seq(1:nrow(coords))

                                        ylines <- grid::polylineGrob(x = c(coords$x, coords$x),
                                                                     y = c(coords$y, coords$yend),
                                                                     id = c(coords$id, coords$id),
                                                                     gp = grid::gpar(col = "grey",
                                                                                     alpha = 0.75,
                                                                                     lwd = 0.5
                                                                                     )
                                                                   )

                                        # Labels
                                        labeltext <- grid::textGrob(x = coords$x,
                                                                    y = coords$y + coords$line_height,
                                                                    label = coords$label,
                                                                    rot = coords$angle,
                                                                    just = c("left", "center"),
                                                                    gp = grid::gpar(col = "black",
                                                                                    fontsize = 3. * .pt
                                                                                    )
                                                                    )

                                        # Add the grobs
                                        grid::grobTree(ylines, labeltext)
                                      }
)

#' Earthquake Timeline
#'
#' \code{geom_timeline_label} should be used with \code{geom_timeline}. This geom adds a label (country, by default)
#'  to the top \code{n_max} earthquakes (sorted by magnitude, by default) per country in an earthquakes timeline plot.
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
#' \code{geom_timeline_label} can be used with the following aesthetics:
#' \itemize{
#'  \item x
#'  \item y
#'  \item label
#'  \item magnitude
#'  \item n_max
#' }
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_point
#' @importFrom grid polylineGrob
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @importFrom grid grobTree
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr top_n
#'
#'
#' @examples
#'
#' \dontrun{
#' data(earthquakes)
#' earthquakes %>%
#' eq_clean_data() %>%
#'   eq_location_clean() %>%
#'   dplyr::filter(COUNTRY %in% c("USA", "CHINA", "AFGHANISTAN")) %>%
#'   dplyr::filter(DATE > "2000-01-01") %>%
#'   ggplot() +
#'   geom_timeline(aes(x = DATE,
#'                     y = COUNTRY,
#'                     color = TOTAL_DEATHS,
#'                     size = EQ_PRIMARY,
#'                     scaling_factor = 1
#'   )
#'   ) +
#'   geom_timeline_label(aes(x = DATE,
#'                           y = COUNTRY,
#'                           label = LOCATION_NAME,
#'                           magnitude = EQ_PRIMARY,
#'                           n_max = 5
#'   )
#'   ) +
#'   theme_bw() +
#'   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#'   scale_size_continuous(name = 'Richter scale value') +
#'   scale_color_continuous(name = '#' of Deaths')
#' }
#'
#' @export
geom_timeline_label <- function(mapping = NULL,
                                data = NULL,
                                stat = "identity",
                                position = "identity",
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE,
                                ...
) {

  ggplot2::layer(geom = GeomTimelineLabel,
                 mapping = mapping,
                 data = data,
                 stat = stat,
                 position = position,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...)
  )
}

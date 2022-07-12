# geom_timeline
#
# Contains functions for creating the timeline geom

#' Base class for geom_timeline
#'
#' Provides the baseclass for the [geom_timeline()] geom. This function is not
#' meant to be called directly by the user and is not exported. Please see [geom_timeline()]
#'
#' @family Timeline Geom
#' @import ggplot2
#' @seealso [ggplot2::Geom()]
GeomTimeline <- ggplot2::ggproto("GeomTimeline", GeomPoint,
                        required_aes = c("x","y", "size", "fill"),
                        default_aes = aes(color="grey", alpha=0.75),
                        draw_key = draw_key_point,
                        draw_panel = function(data, panel_scales, coord) {

                          data_trans <- coord$transform(data, panel_scales)

                          circles <- grid::circleGrob(
                            x=data_trans$x,
                            y=data_trans$y,
                            r=map_to_interval(data_trans$size, x_min=0, x_max=10, u=0.05, l=0),
                            gp=grid::gpar(
                              col=data_trans$color,
                              fill=data_trans$fill,
                              alpha=data_trans$alpha
                            )
                          )

                          return(grid::gTree(children=grid::gList(circles)))
                        }
                  )

#' Create an earthquake timeline
#'
#' Creates a timeline of earthquakes for each specified country passed to the geom.
#'
#' This geom is for the visualization of the [NOAA earthquake data](https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/search). The
#' geom displays a timeline where circles mark the year of an earthquake and whose radius is proportional
#' to the magnitude and whose color fill intensity is proportional to the number of deaths. A factor can be supplied
#' to the geom whose levels are countries and a separate timeline will be draw for each country. It is recommended
#' that no more than three countries be visualized in this way to avoid crowding the figure.
#'
#' This geom is not to be used with the raw data from NOAA but must be first cleaned with the [eq_clean_data()] function.
#'
#' @param mapping A set of aesthetic mappings created by \code{aes()}
#' @param data The data to be displayed in this layer
#'
#' @section Aesthetics:
#'
#' The \code{geom_timeline()} understands the following aesthetics
#' \describe{
#'  \item{x}{numeric representing the year of the earthquake}
#'  \item{y}{factor representing the region. A new timeline will be plotted for each level of the factor}
#'  \item{size}{a numeric controlling the size of the circle marks along the timeline}
#'  \item{fill}{a numeric controlling the fill of the circle marks}
#'  \item{alpha}{optional numeric between 0 and 1 controlling the transparency of the circles}
#'  \item{color}{optional character specifying the color of the circle borders}
#' }
#'
#' @inheritParams ggplot2::geom_point
#' @family Geom Timeline
#' @seealso [eq_clean_data()], [geom_label_timeline()]
#'
#' @examples
#' \dontrun{
#'  data <- eq_clean(raw_data)
#'  data %>%
#'   ggplot(aes(x=Year, y=Country, size=Magnitude, fill=Deaths)) +
#'   geom_timeline(alpha=0.5)
#' }
#' @import grid
#' @import ggplot2
#' @export
geom_timeline <- function(
  mapping=NULL,
  data=NULL,
  stat="identity",
  position="identity",
  na.rm=TRUE,
  show.legend=NA,
  inherit.aes=TRUE, ...) {
  layer(
    geom=GeomTimeline,
    mapping=mapping,
    data=data,
    stat=stat,
    position=position,
    show.legend=show.legend,
    inherit.aes = inherit.aes,
    params=list(na.rm=na.rm, ...)
  )
}

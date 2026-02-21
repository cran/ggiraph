#' @title Create interactive contours of a 2d density estimate
#'
#' @description
#' The geometries are based on [ggplot2::geom_density_2d()] and [ggplot2::geom_density_2d_filled()].
#' See the documentation for those functions for more details.
#'
#' @param ... arguments passed to base function,
#' plus any of the [interactive_parameters].
#' @inheritSection interactive_parameters Details for interactive geom functions
#' @examples
#' # add interactive contours to a ggplot -------
#' @examples
#' library(ggplot2)
#' library(ggiraph)
#'
#' m <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
#'   geom_point_interactive(aes(
#'     tooltip = paste("Waiting:", waiting, "\neruptions:", eruptions)
#'   )) +
#'   xlim(0.5, 6) +
#'   ylim(40, 110)
#' p <- m +
#'   geom_density_2d_interactive(aes(tooltip = paste("Level:", after_stat(level))))
#' x <- girafe(ggobj = p)
#' x
#'
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#' set.seed(4393)
#' dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
#' d <- ggplot(dsmall, aes(x, y))
#'
#'
#' p <- d +
#'   geom_density_2d_interactive(aes(colour = cut, tooltip = cut, data_id = cut))
#' x <- girafe(ggobj = p)
#' x <- girafe_options(x = x, opts_hover(css = "stroke:red;stroke-width:3px;"))
#' x
#'
#' p <- d +
#'   geom_density_2d_filled_interactive(
#'     aes(colour = cut, tooltip = cut, data_id = cut),
#'     contour_var = "count"
#'   ) +
#'   facet_wrap(vars(cut))
#' x <- girafe(ggobj = p)
#' x <- girafe_options(x = x, opts_hover(css = "stroke:red;stroke-width:3px;"))
#' x
#'
#'
#' p <- d +
#'   stat_density_2d(
#'     aes(
#'       fill = after_stat(nlevel),
#'       tooltip = paste("nlevel:", after_stat(nlevel))
#'     ),
#'     geom = "interactive_polygon"
#'   ) +
#'   facet_grid(. ~ cut) +
#'   scale_fill_viridis_c_interactive(tooltip = "nlevel")
#' x <- girafe(ggobj = p)
#' x
#' @seealso [girafe()]
#' @export
geom_density_2d_interactive <- function(...) {
  layer_interactive(geom_density_2d, ...)
}

#' @export
#' @rdname geom_density_2d_interactive
#' @usage NULL
geom_density2d_interactive <- geom_density_2d_interactive

#' @rdname ggiraph-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomInteractiveDensity2d <- ggproto(
  "GeomInteractiveDensity2d",
  GeomInteractivePath,
  default_aes = add_default_interactive_aes(GeomDensity2d),
  parameters = interactive_geom_parameters
)

#' @rdname geom_density_2d_interactive
#' @export
geom_density_2d_filled_interactive <- function(...) {
  layer_interactive(geom_density_2d_filled, ...)
}

#' @export
#' @rdname geom_density_2d_interactive
#' @usage NULL
geom_density2d_filled_interactive <- geom_density_2d_filled_interactive

#' @rdname ggiraph-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomInteractiveDensity2dFilled <- ggproto(
  "GeomInteractiveDensity2dFilled",
  GeomInteractivePolygon,
  default_aes = add_default_interactive_aes(GeomDensity2dFilled)
)

#' @title Create a ggiraph object (deprecated)
#'
#' @description Create an interactive graphic to be used in a web browser.
#'
#' This function is now deprecated, users should
#' now use function [girafe()].
#'
#' @inheritParams girafe
#' @param width widget width ratio (0 < width <= 1).
#' @param tooltip_extra_css extra css (added to \code{position: absolute;pointer-events: none;})
#' used to customize tooltip area.
#' @param hover_css css to apply when mouse is hover and element with a data-id attribute.
#' @param tooltip_opacity tooltip opacity
#' @param tooltip_offx tooltip x offset
#' @param tooltip_offy tooltip y offset
#' @param tooltip_zindex tooltip css z-index, default to 999.
#' @param zoom_max maximum zoom factor
#' @param selection_type row selection mode ("single", "multiple", "none")
#'  when widget is in a Shiny application.
#' @param selected_css css to apply when element is selected (shiny only).
#' @export
#' @keywords internal
ggiraph <- function(code, ggobj = NULL,
                    pointsize = 12,
                    width = .75,
                    width_svg = 6, height_svg = 5,
                    tooltip_extra_css = NULL,
                    hover_css = NULL,
                    tooltip_opacity = .9,
                    tooltip_offx = 10,
                    tooltip_offy = 0,
                    tooltip_zindex = 999,
                    zoom_max = 1,
                    selection_type = "multiple",
                    selected_css = NULL,
                    ...) {
  .Deprecated(new = "girafe", old = "ggiraph")

  x <- girafe(code = code, ggobj = ggobj, pointsize = pointsize,
              width_svg = width_svg, height_svg = height_svg, ...)
  x <- girafe_options(
    x = x,
    opts_tooltip(css = tooltip_extra_css,
                 opacity = tooltip_opacity,
                 use_cursor_pos = TRUE,
                 offx = tooltip_offx, offy = tooltip_offy,
                 delay_mouseover = 200, delay_mouseout = 500,
                 zindex = tooltip_zindex),
    opts_zoom(min = 1, max = zoom_max),
    opts_selection(type = selection_type, css = selected_css),
    opts_toolbar(position = "top", saveaspng = FALSE),
    opts_hover(css = hover_css),
    opts_sizing(rescale = TRUE, width = width)
  )

  # fix for package ceterisParibus unit tests
  class(x) <- unique( c(class(x), "ggiraph"))

  x
}



#' @title Create a ggiraph output element
#' @description Render a ggiraph within an application page.
#'
#' This function is now deprecated, users should
#' now use function [girafeOutput()].
#' @param outputId output variable to read the ggiraph from.
#' @param width widget width
#' @param height widget height
#' @examples
#' \dontrun{
#' if( require(shiny) && interactive() ){
#'   app_dir <- file.path( system.file(package = "ggiraph"), "examples/shiny/cars" )
#'   shinyAppDir(appDir = app_dir )
#'  }
#' if( require(shiny) && interactive() ){
#'   app_dir <- file.path( system.file(package = "ggiraph"), "examples/shiny/crimes" )
#'   shinyAppDir(appDir = app_dir )
#' }
#' }
#' @export
#' @keywords internal
ggiraphOutput <- function(outputId, width = "100%", height = "500px"){
  .Deprecated(new = "girafeOutput")
  shinyWidgetOutput(outputId, 'girafe', package = 'ggiraph', width = width, height = height)
}

#' @title Reactive version of ggiraph object
#'
#' @description Makes a reactive version of a ggiraph object for use in Shiny.
#'
#' This function is now deprecated, users should
#' now use function [renderGirafe()].
#' @param expr An expression that returns a [ggiraph()] object.
#' @param env The environment in which to evaluate expr.
#' @param quoted Is `expr` a quoted expression
#' @examples
#' \dontrun{
#' if( require(shiny) && interactive() ){
#'   app_dir <- file.path( system.file(package = "ggiraph"), "examples/shiny" )
#'   shinyAppDir(appDir = app_dir )
#' }
#' }
#' @export
#' @keywords internal
renderggiraph <- function(expr, env = parent.frame(), quoted = FALSE) {
  .Deprecated(new = "renderGirafe")
  if (!quoted) { expr <- substitute(expr) } # force quoted
	shinyRenderWidget(expr, girafeOutput, env, quoted = TRUE)
}










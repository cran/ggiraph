#' @title Create interactive observations connections
#'
#' @description
#' These geometries are based on [geom_path()],
#' [geom_line()] and [geom_step()].
#' See the documentation for those functions for more details.
#'
#' @param ... arguments passed to base function,
#' plus any of the [interactive_parameters].
#' @inheritSection interactive_parameters Details for interactive geom functions
#' @examples
#' # add interactive paths to a ggplot -------
#' @example examples/geom_path_interactive.R
#' @seealso [girafe()]
#' @export
geom_path_interactive <- function(...)
  layer_interactive(geom_path, ...)

#' @importFrom stats complete.cases
#' @importFrom stats ave
#' @rdname ggiraph-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomInteractivePath <- ggproto(
  "GeomInteractivePath",
  GeomPath,
  default_aes = add_default_interactive_aes(GeomPath),
  parameters = interactive_geom_parameters,
  draw_key = interactive_geom_draw_key,
  draw_panel = function(data,
                        panel_params,
                        coord,
                        arrow = NULL,
                        lineend = "butt",
                        linejoin = "round",
                        linemitre = 10,
                        na.rm = FALSE,
                        .ipar = IPAR_NAMES) {
    gr <- GeomPath$draw_panel(
      data = data,
      panel_params = panel_params,
      coord = coord,
      arrow = arrow,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      na.rm = na.rm)

    if (inherits(gr, "zeroGrob")) {
      return(gr)
    }

    # must be sorted on group
    data <- data[order(data$group), , drop = FALSE]
    munched <- coord_munch(coord, data, panel_params)
    rows <-
      stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]

    # Work out grouping variables for grobs
    n <- nrow(munched)
    constant <- length(gr$x0) == n

    if (!constant) {
      group_diff <- munched$group[-1] != munched$group[-n]
      end <- c(group_diff, TRUE)
      add_interactive_attrs(gr, munched, rows = !end, ipar = .ipar)
    } else {
      add_interactive_attrs(gr, munched, ipar = .ipar)
    }

  }
)


#' @export
#' @rdname geom_path_interactive
geom_line_interactive <- function(...)
  layer_interactive(geom_line, ...)

#' @rdname ggiraph-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomInteractiveLine <- ggproto(
  "GeomInteractiveLine",
  GeomLine,
  default_aes = add_default_interactive_aes(GeomLine),
  parameters = interactive_geom_parameters,
  draw_key = interactive_geom_draw_key,
  draw_panel = function(data, panel_params, coord, ..., .ipar = IPAR_NAMES) {
    GeomInteractivePath$draw_panel(data, panel_params, coord, ..., .ipar = .ipar)
  }
)

#' @export
#' @rdname geom_path_interactive
geom_step_interactive <- function(...)
  layer_interactive(geom_step, ...)

#' @rdname ggiraph-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomInteractiveStep <-
  ggproto(
    "GeomInteractiveStep",
    GeomStep,
    default_aes = add_default_interactive_aes(GeomStep),
    parameters = interactive_geom_parameters,
    draw_key = interactive_geom_draw_key,
    draw_panel = function(data, panel_params, coord, direction = "hv", .ipar = IPAR_NAMES) {
      ldata <- split(data, data$group)
      ldata <- lapply(ldata, stairstep, direction = direction)
      data <- do.call(rbind, ldata)
      row.names(data) <- NULL
      GeomInteractivePath$draw_panel(data, panel_params, coord, .ipar = .ipar)
    }
  )

# Calculate stairsteps
stairstep <- function(data, direction = "hv") {
  direction <- match.arg(direction, c("hv", "vh", "mid"))
  data <- as.data.frame(data)[order(data$x), ]
  n <- nrow(data)

  if (n <= 1) {
    # Need at least one observation
    return(data[0, , drop = FALSE])
  }

  if (direction == "vh") {
    xs <- rep(1:n, each = 2)[-2 * n]
    ys <- c(1, rep(2:n, each = 2))
  } else if (direction == "hv") {
    ys <- rep(1:n, each = 2)[-2 * n]
    xs <- c(1, rep(2:n, each = 2))
  } else {
    xs <- rep(1:(n-1), each = 2)
    ys <- rep(1:n, each = 2)
  }

  if (direction == "mid") {
    gaps <- data$x[-1] - data$x[-n]
    mid_x <- data$x[-n] + gaps/2 # map the mid-point between adjacent x-values
    x <- c(data$x[1], mid_x[xs], data$x[n])
    y <- c(data$y[ys])
    data_attr <- data[c(1,xs,n), setdiff(names(data), c("x", "y"))]
  } else {
    x <- data$x[xs]
    y <- data$y[ys]
    data_attr <- data[xs, setdiff(names(data), c("x", "y"))]
  }

  data_frame0(x = x, y = y, data_attr)
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_crossbar_interactive.R,
%   R/geom_errorbar_interactive.R, R/geom_linerange_interactive.R,
%   R/geom_pointrange_interactive.R
\name{geom_crossbar_interactive}
\alias{geom_crossbar_interactive}
\alias{geom_errorbar_interactive}
\alias{geom_linerange_interactive}
\alias{geom_pointrange_interactive}
\title{Create interactive vertical intervals: lines, crossbars & errorbars}
\usage{
geom_crossbar_interactive(...)

geom_errorbar_interactive(...)

geom_linerange_interactive(...)

geom_pointrange_interactive(...)
}
\arguments{
\item{...}{arguments passed to base function,
plus any of the \link{interactive_parameters}.}
}
\description{
These geometries are based on \code{\link[=geom_crossbar]{geom_crossbar()}}, \code{\link[=geom_errorbar]{geom_errorbar()}},
\code{\link[=geom_linerange]{geom_linerange()}} and \code{\link[=geom_pointrange]{geom_pointrange()}}.
See the documentation for those functions for more details.
}
\section{Details for interactive geom functions}{

The interactive parameters can be supplied with two ways:
\itemize{
\item As aesthetics with the mapping argument (via \code{\link[=aes]{aes()}}).
In this way they can be mapped to data columns and apply to a set of geometries.

\item As plain arguments into the geom_*_interactive function.
In this way they can be set to a scalar value.
}
}

\examples{
# add interactive intervals -------
library(ggplot2)
library(ggiraph)

# Create a simple example dataset
df <- data.frame(
  trt = factor(c(1, 1, 2, 2)),
  resp = c(1, 5, 3, 4),
  group = factor(c(1, 2, 1, 2)),
  upper = c(1.1, 5.3, 3.3, 4.2),
  lower = c(0.8, 4.6, 2.4, 3.6)
)

p <- ggplot(df, aes(trt, resp, colour = group))
g <- p + geom_linerange_interactive(aes(ymin = lower, ymax = upper, tooltip = group))
x <- girafe(ggobj = g)
if( interactive() ) print(x)

g <- p + geom_pointrange_interactive(aes(ymin = lower, ymax = upper, tooltip = group))
x <- girafe(ggobj = g)
if( interactive() ) print(x)

g <- p + geom_crossbar_interactive(aes(ymin = lower, ymax = upper, tooltip = group), width = 0.2)
x <- girafe(ggobj = g)
if( interactive() ) print(x)

g <- p + geom_errorbar_interactive(aes(ymin = lower, ymax = upper, tooltip = group), width = 0.2)
x <- girafe(ggobj = g)
if( interactive() ) print(x)
}
\seealso{
\code{\link[=girafe]{girafe()}}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interactive_polyline_grob.R
\name{interactive_polyline_grob}
\alias{interactive_polyline_grob}
\alias{interactive_lines_grob}
\title{Create interactive polyline grob}
\usage{
interactive_polyline_grob(...)

interactive_lines_grob(...)
}
\arguments{
\item{...}{arguments passed to base function,
plus any of the \link{interactive_parameters}.}
}
\value{
An interactive grob object.
}
\description{
These grobs are based on \code{\link[=polylineGrob]{polylineGrob()}} and \code{\link[=linesGrob]{linesGrob()}}.
See the documentation for those functions for more details.
}
\section{Details for interactive_*_grob functions}{

The interactive parameters can be supplied as arguments in the relevant function
and they can be scalar values or vectors depending on params on base function.
}

\seealso{
\code{\link[=girafe]{girafe()}}
}

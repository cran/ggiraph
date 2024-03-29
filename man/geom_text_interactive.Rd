% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_label_interactive.R,
%   R/geom_text_interactive.R
\name{geom_label_interactive}
\alias{geom_label_interactive}
\alias{geom_text_interactive}
\title{Create interactive textual annotations}
\usage{
geom_label_interactive(...)

geom_text_interactive(...)
}
\arguments{
\item{...}{arguments passed to base function,
plus any of the \link{interactive_parameters}.}
}
\description{
The geometries are based on \code{\link[=geom_text]{geom_text()}} and \code{\link[=geom_label]{geom_label()}}.
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
# add interactive labels to a ggplot -------
library(ggplot2)
library(ggiraph)


p <- ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
  geom_label_interactive(aes(tooltip = paste(rownames(mtcars), mpg, sep = "\n")))
x <- girafe(ggobj = p)
if( interactive() ) print(x)


p <- ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
  geom_label_interactive(aes(fill = factor(cyl),
                             tooltip = paste(rownames(mtcars), mpg, sep = "\n")),
                         colour = "white",
                         fontface = "bold")
x <- girafe(ggobj = p)
if( interactive() ) print(x)

# add interactive texts to a ggplot -------
library(ggplot2)
library(ggiraph)

## the data
dataset = mtcars
dataset$label = row.names(mtcars)

dataset$tooltip = paste0( "cyl: ", dataset$cyl, "<br/>",
       "gear: ", dataset$gear, "<br/>",
       "carb: ", dataset$carb)

## the plot
gg_text = ggplot(dataset,
                 aes(x = mpg, y = wt, label = label,
                     color = qsec,
                     tooltip = tooltip, data_id = label)) +
  geom_text_interactive(check_overlap = TRUE) +
  coord_cartesian(xlim = c(0,50))

## display the plot
x <- girafe(ggobj = gg_text)
x <- girafe_options(x = x,
                    opts_hover(css = "fill:#FF4C3B;font-style:italic;") )
if( interactive() ) print(x)
}
\seealso{
\code{\link[=girafe]{girafe()}}
}

ggiraph R package
================

<p align="center">

<img width="15%" src="https://www.ardata.fr/img/hexbin/ggiraph.svg">

</p>

> Make ‘ggplot’ Graphics Interactive

## Overview

`ggiraph` is an htmlwidget and a ggplot2 extension. It allows ggplot
graphics to be animated.

Animation is made with ggplot geometries that can understand three
arguments:

  - `tooltip`: column of dataset that contains tooltips to be displayed
    when mouse is over elements.
  - `onclick`: column of dataset that contains javascript function to be
    executed when elements are clicked.
  - `data_id`: column of dataset that contains id to be associated with
    elements.

If used within a shiny application, elements associated with an id
(`data_id`) can be selected and manipulated on client and server
sides.

[![](https://img.youtube.com/vi/cJt5hlCi_do/0.jpg)](https://youtu.be/cJt5hlCi_do)

## Installation

##### Get development version on github

``` r
devtools::install_github('davidgohel/ggiraph')
```

##### Get CRAN version

``` r
install.packages("ggiraph")
```

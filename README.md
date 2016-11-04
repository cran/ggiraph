ggiraph R package
================

> Make 'ggplot' Graphics Interactive

[![Travis-CI Build Status](https://travis-ci.org/davidgohel/ggiraph.svg?branch=master)](https://travis-ci.org/davidgohel/ggiraph) [![Build status](https://ci.appveyor.com/api/projects/status/github/davidgohel/ggiraph?branch=master)](https://ci.appveyor.com/project/davidgohel/ggiraph/branch/master) [![version](http://www.r-pkg.org/badges/version/ggiraph)](http://www.r-pkg.org/pkg/ggiraph) ![cranlogs](http://cranlogs.r-pkg.org./badges/ggiraph) ![Active](http://www.repostatus.org/badges/latest/active.svg)

Overview
--------

`ggiraph` is an htmlwidget and a ggplot2 extension. It lets ggplot graphics being animated.

Animation is made with ggplot geometries that can understand three arguments:

-   `tooltip`: column of dataset that contains tooltips to be displayed when mouse is over elements.
-   `onclick`: column of dataset that contains javascript function to be executed when elements are clicked.
-   `data_id`: column of dataset that contains id to be associated with elements.

If used within a shiny application, elements associated with an id (`data_id`) can be selected and manipulated on client and server sides.


Installation
------------

##### github

    # get development version
    devtools::install_github('davidgohel/ggiraph')

##### cran

    # get cran version
    install.packages("ggiraph")

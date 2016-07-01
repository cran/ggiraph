## ----eval=FALSE----------------------------------------------------------
#  shiny::runApp(appDir = system.file("shiny/crimes", package = "ggiraph"), display.mode = "showcase")
#  shiny::runApp(appDir = system.file("shiny/cars", package = "ggiraph"), display.mode = "showcase")

## ----eval=FALSE----------------------------------------------------------
#  ggiraphOutput("plot")

## ----eval=FALSE----------------------------------------------------------
#  output$plot <- renderggiraph({
#      ggiraph(code = print(gg_blahblah) )
#    })

## ----eval=FALSE----------------------------------------------------------
#  output$myplot <- renderggiraph({
#      ggiraph(code = print(gg_blahblah), selection_type = "multiple" )
#    })

## ----eval=FALSE----------------------------------------------------------
#  input$myplot_selected

## ----eval=FALSE----------------------------------------------------------
#  # delete selection
#  session$sendCustomMessage(type = 'myplot_set', message = character(0))


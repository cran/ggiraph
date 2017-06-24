## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
library(ggiraph)
theme_set(theme_minimal())

# geom_point_interactive example
gg_point_1 <- ggplot(mtcars, aes(x = disp, y = qsec, 
		color = wt, tooltip = row.names(mtcars), data_id = row.names(mtcars) ) ) + 
	geom_point_interactive(size=3) + 
  scale_color_gradient(low = "#F3C899", high = "#8C120A") 

# htmlwidget call
ggiraph(code = {print(gg_point_1)}, 
        tooltip_extra_css = "padding:2px;background:rgba(70,70,70,0.1);color:black;border-radius:2px 2px 2px 2px;",
        hover_css = "fill:#1279BF;stroke:#1279BF;cursor:pointer;", pointsize = 14)


## ----echo=FALSE, results='asis'------------------------------------------
geoms_ <- objects(envir = as.environment("package:ggiraph"), pattern = "^geom_([a-zA-Z0-9]*)_interactive$")
htmltools::tags$ul( lapply( geoms_, htmltools::tags$li ) )

## ------------------------------------------------------------------------
library(ggiraph)

head(mpg)
g <- ggplot(mpg, aes( x = displ, y = cty, color = hwy) )

## ----message=FALSE-------------------------------------------------------
my_gg <- g + geom_point_interactive(aes(tooltip = model), size = 2) 
ggiraph(code = print(my_gg) )

## ----message=FALSE-------------------------------------------------------
my_gg <- g + geom_point_interactive(
    aes(tooltip = model, data_id = model), size = 2) 
ggiraph(code = print(my_gg), hover_css = "cursor:pointer;fill:red;stroke:red;")

## ----message=FALSE, warning=FALSE----------------------------------------
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
head(crimes)

# create an 'onclick' column
crimes$onclick <- sprintf("window.open(\"%s%s\")",
  "http://en.wikipedia.org/wiki/", as.character(crimes$state) )

gg_crime <- ggplot(crimes, aes(x = Murder, y = Assault, color = UrbanPop )) + 
  geom_point_interactive(
    aes( data_id = state, tooltip = state, onclick = onclick ), size = 3 ) + 
  scale_colour_gradient(low = "#999999", high = "#FF3333")

ggiraph(code = print(gg_crime), hover_css = "fill-opacity:.3;cursor:pointer;")


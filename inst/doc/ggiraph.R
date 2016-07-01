## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
library(ggiraph)

# geom_point_interactive example
gg_point_1 <- ggplot(mtcars, aes(x = disp, y = qsec, 
		color = wt, tooltip = row.names(mtcars), data_id = row.names(mtcars) ) ) + 
	geom_point_interactive(size=3) + 
  scale_color_gradient(low = "#F3C899", high = "#8C120A") 

gg_point_1 <- gg_point_1 + theme_minimal()
# htmlwidget call
ggiraph(code = {print(gg_point_1)}, width = .6,
        tooltip_extra_css = "padding:2px;background:rgba(70,70,70,0.1);color:black;border-radius:2px 2px 2px 2px;",
        hover_css = "fill:#1279BF;stroke:#1279BF;cursor:pointer;")


## ------------------------------------------------------------------------
library(ggiraph)

head(mpg)
g <- ggplot(mpg, aes( x = displ, y = cty, color = drv) ) + theme_minimal()

## ----message=FALSE-------------------------------------------------------
my_gg <- g + geom_point_interactive(aes(tooltip = model), size = 2) 
ggiraph(code = print(my_gg), width = .7)

## ----message=FALSE-------------------------------------------------------
my_gg <- g + geom_point_interactive(
    aes(tooltip = model, data_id = model), size = 2) 
ggiraph(code = print(my_gg), width = .7)

## ----message=FALSE, warning=FALSE----------------------------------------
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
head(crimes)

# create an 'onclick' column
crimes$onclick <- sprintf("window.open(\"%s%s\")",
  "http://en.wikipedia.org/wiki/", as.character(crimes$state) )

gg_crime <- ggplot(crimes, aes(x = Murder, y = Assault, color = UrbanPop )) + 
  geom_point_interactive(
    aes( data_id = state, tooltip = state, onclick = onclick ), size = 3 ) + 
  scale_colour_gradient(low = "#999999", high = "#FF3333") + 
  theme_minimal()

ggiraph(code = print(gg_crime),
        hover_css = "fill-opacity:.3;cursor:pointer;")

## ------------------------------------------------------------------------
ggiraph(code = print(my_gg + theme_linedraw()), zoom_max = 5)


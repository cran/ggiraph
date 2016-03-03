## ----message=FALSE-------------------------------------------------------
library(ggiraph)

dataset <- mtcars
head(dataset)
dataset$tooltip <- row.names(dataset)

# geom_point_interactive example
gg_point_1 <- ggplot(dataset, aes(x = disp, y = qsec, 
		color = wt, tooltip = tooltip ) ) + 
	geom_point_interactive(size=3)

# htmlwidget call
ggiraph(code = {print(gg_point_1)}, width = 7, height = 6)

## ------------------------------------------------------------------------
dataset$data_id <- tolower(row.names(dataset))

# geom_point_interactive example
gg_point_2 <- ggplot(dataset, aes(x = disp, y = qsec, 
		color = wt, tooltip = tooltip, data_id = data_id ) ) + 
	geom_point_interactive(size=4)

# htmlwidget call
ggiraph(code = {print(gg_point_2)}, 
        width = 7, height = 6, 
        hover_css = "fill:orange;r:6px;cursor:pointer;")

## ----message=FALSE, warning=FALSE----------------------------------------
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)

crimes$onclick <- sprintf(
  "window.open(\"%s%s\")",
  "http://en.wikipedia.org/wiki/",
  as.character(crimes$state)
)

if (require("maps") ) {
  states_map <- map_data("state")
  gg_map <- ggplot(crimes, aes(map_id = state))
  gg_map <- gg_map + 
    geom_map_interactive(
      aes( fill = Murder, data_id = state, tooltip = state, onclick = onclick ), 
      map = states_map) + 
    expand_limits(x = states_map$long, y = states_map$lat)
}

ggiraph(code = print(gg_map), 
        width = 7, height = 5, 
        hover_css = "fill:orange;stroke-width:1px;stroke:wheat;cursor:pointer;")


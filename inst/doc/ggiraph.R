## ----message=FALSE-------------------------------------------------------
library(ggiraph)
mytheme_main <- theme( panel.background = element_blank(), 
  panel.grid.major = element_line(colour = "#dddddd"), 
  axis.ticks = element_line(colour = "#dddddd") )

mytheme_map <- theme(
  panel.background = element_blank(), axis.title.x = element_blank(),
  axis.text = element_blank(), axis.line.x = element_blank(),
  axis.line.y = element_blank(), axis.title.y = element_blank(),
  axis.ticks.x = element_blank(), axis.ticks.y = element_blank() )

## ----message=FALSE-------------------------------------------------------
dataset <- mtcars
head(dataset)
dataset$tooltip <- row.names(dataset)

# geom_point_interactive example
gg_point_1 <- ggplot(dataset, aes(x = disp, y = qsec, 
		color = wt, tooltip = tooltip ) ) + 
	geom_point_interactive(size=3)

# htmlwidget call
ggiraph(code = {print(gg_point_1 + mytheme_main)}, width = 7, height = 6)

## ------------------------------------------------------------------------
dataset$data_id <- tolower(row.names(dataset))

# geom_point_interactive example
gg_point_2 <- ggplot(dataset, aes(x = disp, y = qsec, 
		color = wt, tooltip = tooltip, data_id = data_id ) ) + 
	geom_point_interactive(size=4)

# htmlwidget call
ggiraph(code = {print(gg_point_2 + mytheme_main)}, 
        width = 7, height = 6, 
        hover_css = "{fill:orange;r:6px;}")

## ----message=FALSE, warning=FALSE----------------------------------------
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)

crimes$onclick <- sprintf(
  "function() {window.open('%s%s')}",
  "http://en.wikipedia.org/wiki/",
  as.character(crimes$state)
)

if (require("maps") ) {
  states_map <- map_data("state")
  gg_map <- ggplot(crimes, aes(map_id = state))
  gg_map <- gg_map + 
    geom_map_interactive(
      aes( fill = Murder, tooltip = state, 
           onclick = onclick, data_id = state), 
      map = states_map) + 
    expand_limits(x = states_map$long, y = states_map$lat)
}

ggiraph(code = {print(gg_map + mytheme_map)}, width = 7, height = 5)


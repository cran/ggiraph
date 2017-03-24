## ---- warning=FALSE, message=FALSE---------------------------------------
library(ggiraph)

gg_point = ggplot(mtcars, aes(x = wt, y = drat, color = disp) ) +
  geom_point() + theme_minimal()

ggiraph(code = {print(gg_point)}, zoom_max = 1, width = 0.25)
ggiraph(code = {print(gg_point)}, zoom_max = 1, width = 0.50)
ggiraph(code = {print(gg_point)}, zoom_max = 1, width = 0.75)


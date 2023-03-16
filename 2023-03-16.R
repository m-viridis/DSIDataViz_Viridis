#DSI DATAVIZ CODEALONG
#March 16, 2023

#import libraries
library (tidyverse)
library (socviz)
library (ggplot2)
library (gapminder)

#COMBINE LOGSCALE, TWO TYPES OF PLOTS AND COLOR
#p <- ggplot (data = gapminder,
#             mapping = aes(x = gdpPercap, y = lifeExp, color = continent))
#p + geom_point() + geom_smooth(method= "lm") + scale_x_log10(labels = scales::dollar)

#p <- ggplot (data = gapminder,
#             mapping = aes(x = gdpPercap, y = lifeExp,
#                           color = continent))

#p + geom_point() + geom_smooth(method= "lm") + 
#  scale_x_log10(labels = scales::dollar)


p <- ggplot (data = gapminder,
             mapping = aes(x = gdpPercap, y = lifeExp))

#CHANGE TRANSPARENCY
p + geom_point(alpha = 0.3) + geom_smooth(method = "lm") + 
  scale_x_log10(labels = scales::dollar)

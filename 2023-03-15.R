library(tidyverse)
library(socviz)
library(ggplot2)
library(gapminder)

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point()



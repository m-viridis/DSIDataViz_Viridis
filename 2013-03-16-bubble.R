library(ggplot2)
library(dplyr)
library(gapminder)

#data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

data <- gapminder |> filter(year=="2007") |> dplyr::select(-year) #n.b. |> is the symbol for piping

ggplot(data, aes(x=gdpPercap, y=lifeExp, size = pop, color=continent)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(.1, 24), name="Population")


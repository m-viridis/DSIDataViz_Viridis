#DSI DATAVIZ CODEALONG
#March 30, 2023

#import libraries
library (ggplot2)
library (ggrepel)
library (ggthemes)
library (tidyverse)
library (socviz)
library(maps)
library (RColorBrewer)

us_states <- map_data("state")
head(us_states)

party_colors <- c("#2E74C0", "#CB454A")

#create map
p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat, group =group))
p + geom_polygon(fill = "white", color = "black")

#change shape DOESN"T WORK :(
p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat, group =group, fill = region))
p + geom_polygon(color ="gray90", linewidth = 0.1) +
  coord_map(projection = "albers", lat0=39,lat1=45) +
  guides(fill = FALSE)

#merge dfs
election$region <- tolower(election$state)
us_states_elec <- left_join(us_states, election, by = 'region')
head(us_states_elec)

#plot elec data
p0 <- ggplot(data = us_states_elec,
             mapping = aes(x = long,
                           y = lat,
                           group = group,
                           fill = party))
p1 <- p0 + geom_polygon(color = "gray90", linewidth = 0.1) +
  coord_map(projection = "albers", lat0=39, lat1=45)
p2 <- p1 + scale_fill_manual(values = party_colors) +
  labs(title = "2016 elec", fill=NULL)
p2 + theme_map()

#explore data
county_map |>
  sample_n(5)
county_data |>
  select(id,name,state,pop_dens) |>
  sample_n(5)

#merge dfs at county level
county_full <- left_join(county_map, county_data, by="id")
head(county_full)

#plot pop density
p3 <- ggplot(data = county_full,
             mapping = aes(x = long, y = lat, fill = pop_dens, group = group))
p4 <-p3 + geom_polygon(color = "gray90", size = 0.05) +
  coord_equal()
p5 <- p4 + scale_fill_brewer(palette = "Blues",
                             labels = c("0-10", "10-50", "50-100", "100-500", "500-1000", "1000-5000", ">5000"))
p5 + labs(fill = "Pop per \n square mile") +
  theme_map() +
  guides(fill = guide_legend(nrow=1)) +
  theme(legend.position = "bottom")
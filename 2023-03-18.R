#DSI DATAVIZ CODEALONG
#March 18, 2023

#import libraries
library (tidyverse)
library (socviz)
library (ggplot2)
library (gapminder)

#######################################################################
#REFERENCE: facet_wrap vs facet_grid https://ggplot2-book.org/facet.html
#######################################################################

#show GDP change per country using linegraph
p <- ggplot (data = gapminder,
             mapping = aes(x = year, y = gdpPercap))
p + geom_line(aes(group = country))

#break out data into different continents by continent using facet_wrap
p <- ggplot (data = gapminder,
             mapping = aes(x = year, y = gdpPercap))
p + geom_line(color="gray70", aes(group = country)) + 
  geom_smooth(size=1.1, method = "loess", se = FALSE) + #loess=local polynomial regression fitting; se=standard error https://stackoverflow.com/questions/68048196/what-does-se-stand-for-in-geom-smooth-se-false
  scale_y_log10(labels = scales::dollar) +
  facet_wrap(~continent, ncol=5) +
  labs(x = "Year",
       y = "GDP per capita",
       Title = "GDP per capita on 5 Continents")

#break out data by multiple variables using facet_grid
p <- ggplot(data = gss_sm, mapping = aes(x = age, y = childs))
p + geom_point(alpha = 0.2) + geom_smooth() + facet_grid(sex~race)

#map data by us region and proportion
p <- ggplot(data = gss_sm, mapping = aes(x = bigregion))
p + geom_bar(mapping = aes(y = ..prop.., group = 1)) #prop changes counts into proportions

#map data by us region and religion, then proportion
p <- ggplot(data = gss_sm, mapping = aes(x = bigregion, fill = religion))
p + geom_bar(position = "fill")

#breakout data into different graphs by bigregion with facet_wrap
p <- ggplot(data = gss_sm, mapping = aes(x = religion))
p + geom_bar(position = "dodge", mapping = aes(y = ..prop.., group = bigregion)) +
  facet_wrap(~bigregion, ncol = 1)

#create new R variable for two states

oh_wi <- c("OH","WI") #c function means concatenate

#counts vs percollege 
p <- ggplot(data = subset(midwest, subset = state |> oh_wi), #changed piping symbol from %in%
            mapping = aes(x = percollege, fill = state))
p + geom_histogram(alpha = 0.4, bins = 20)

#density plot instead of histogram
p <- ggplot(data = subset(midwest, subset = state |> oh_wi), 
            mapping = aes(x = area, fill = state, color = state))
p + geom_density(alpha = 0.3)

#review organdata by selecting col 1-6 and a random sample of 10 records
organdata |> select(1:6) |> sample_n(size = 10)

#break out data in dif graphs by country using facet_wrap
p <- ggplot(data = organdata,
            mapping = aes(x = year, y = donors))
p + geom_line(aes(group = country)) + facet_wrap(~country)

#create boxplots with flipped data to read country labels better
p <- ggplot(data = organdata,
            mapping = aes(x = country, y = donors))
p + geom_boxplot() + coord_flip() #change x and y axis

#sort labels by amount of donations
p <- ggplot(data = organdata,
            mapping = aes(x = reorder(country, donors, na.rm = TRUE), y = donors))
p + geom_boxplot() + coord_flip()

#sort labels by country name
p <- ggplot(data = organdata,
            mapping = aes(x = reorder(country, country, na.rm = TRUE), y = donors))
p + geom_boxplot() + coord_flip()

#colour boxplots by "world" (appears to be political leaning)
p <- ggplot(data = organdata,
            mapping = aes(x = reorder(country, donors, na.rm = TRUE), y = donors, color = world))
p + geom_point() + coord_flip()

#create jitterplot
p <- ggplot(data = organdata,
            mapping = aes(x = reorder(country, donors, na.rm = TRUE), y = donors, color = world))
p + geom_jitter() + coord_flip()

#create R variable that pipes data
by_country <- organdata |> 
  group_by(consent_law, country) |> 
  summarize_if(is.numeric, funs(mean, sd), na.rm = TRUE) |> #if the data is numeric, apply functions for mean & standard deviation, remove null values
  ungroup() #https://bookdown.org/yih_huynh/Guide-to-R-Book/groupby.html

#create cleveland plot
p <- ggplot(data = by_country,
            mapping = aes(x = donors_mean, y = reorder(country, donors_mean), color = consent_law))
p + geom_point(size = 3) +
  labs(x = "Donor Procurement Rate", y = "", color = "Consent Law") +
  theme(legend.position="top")

#add labels using hjust
p <- ggplot(data = by_country,
            mapping = aes(x = roads_mean, y = donors_mean))
p + geom_point() + geom_text(mapping = aes(label = country), hjust=0) #0 left-justifies, 1 right justifies

#add labels using ggrepel
p <- ggplot(elections_historic, aes(x = popular_pct,
                                    y = ec_pct,
                                    label = winner_label))
p + geom_hline(yintercept = 0.5, size=1.4, color = "gray80") + 
  geom_vline(xintercept = 0.5, size=1.4, color = "gray80") +
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Winner's Share of Pop Vote",
       y = "Winner's Share of Elec College Vote",
       title = "Elections")
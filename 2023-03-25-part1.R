#DSI DATAVIZ CODEALONG
#March 25, 2023

#import libraries
library (tidyverse)
library (socviz)
library (ggplot2)
library (gapminder)
library (ggrepel)
library (ggthemes)
library (RColorBrewer)

#create model colors obj

model_colors <- RColorBrewer::brewer.pal(3, "Set1")

#create a legend for geoms

p0 <- ggplot(data = gapminder, mapping = aes(x = log(gdpPercap), y = lifeExp))

p1 <- p0 + geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", aes(color = "OLS", fill = "OLS")) + #Linear Model, straight line
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, df = 3), #See notes below
              aes(color = "Cubic Spline", fill = "Cubic Spline")) +
  geom_smooth(method = "loess", aes(color = "LOESS", fill = "LOESS")) #Local Polynomial Regression Fitting

####################################################################

#formula = description of the model to be fitted
#splines = allows for better calculation of fit; line can wiggle
#bs = B-Spline calculation
#df = degrees of freedom (use instead of "knots")
#https://rdrr.io/r/stats/lm.html
#https://www.rdocumentation.org/packages/splines/versions/3.6.2/topics/bs
#compare different ways of calculating: https://medium.com/analytics-vidhya/spline-regression-in-r-960ca82aa62c

####################################################################

#create legend
p1 + scale_color_manual(name = "Models", values = model_colors) +
  scale_fill_manual(name = "Models", values = model_colors) +
  theme(legend.position = "top")

#explore gapminder
str(gapminder) #see classes, size, variables

#create obj of model and explore
out <- lm(formula = lifeExp ~ gdpPercap + pop + continent, data = gapminder)
str(out)
summary(out)

#create new df for predictive modelling with gapminder
min_gdp <- min(gapminder$gdpPercap)
max_gdp <- max(gapminder$gdpPercap)
med_pop <- median(gapminder$pop)

pred_df <- expand.grid(gdpPercap = (seq(from = min_gdp, to = max_gdp, length.out = 100)),
                       pop = med_pop,
                       continent = c("Africa", "Americas", "Asia", "Europe", "Oceania"))
dim(pred_df) #shows 3 cols, 500 rows

#predicting values
pred_out <- predict(object = out, newdata = pred_df, interval = "predict") #interval control prediction intervals

#head(pred_out)

#combine dfs

pred_df <-cbind(pred_df, pred_out)
head(pred_df)

#use ggplot to visualize subset of data

p2 <- ggplot(data = subset(pred_df, continent %in% c("Europe", "Africa")),
             aes(x = gdpPercap, y = fit, ymin = lwr, ymax = upr,
                 color = continent, fill = continent, group = continent))
p2 + geom_point(data = subset(gapminder, continent %in% c("Europe", "Africa")),
                aes(x = gdpPercap, y = lifeExp, color = continent),
                alpha = 0.5, inherit.aes = FALSE) +
  geom_line() +
  geom_ribbon(alpha = 0.1, color = FALSE) +
  scale_x_log10(labels = scales::dollar)

############################################
#geom_ribbon https://typethepipe.com/vizs-and-tips/ggplot-geom_ribbon-shadow-confidence-interval/
############################################

#make new plot with different continents
p3 <- ggplot(data = subset(pred_df, continent %in% c("Asia", "Americas")),
             aes(x = gdpPercap, y = fit, ymin = lwr, ymax = upr,
                 color = continent,
                 fill = continent,
                 group = continent))
p3 + geom_point(data = subset(gapminder, continent %in% c("Asia", "Americas")),
                aes(x = gdpPercap, y = lifeExp, color = continent),
                alpha = 0.5, inherit.aes = FALSE) +
  geom_line() +
  geom_ribbon(alpha = 0.1, color = FALSE) +
  scale_x_log10(labels = scales::dollar)

#create new model obj and dataset
country_obj <- lm(formula = lifeExp ~ gdpPercap + pop + country, data = gapminder)
country_df <- expand.grid(gdpPercap = (seq(from = min_gdp, to = max_gdp, length.out = 100)),
                             pop = med_pop,
                             country = c("Afghanistan", "Australia"))

country_out <- predict(object = country_obj, newdata = country_df, interval = "predict")
country_df <-cbind(country_df, country_out)
head(country_df)

p4 <- ggplot(data = subset(country_df, country %in% c("Afghanistan", "Australia")),
             aes(x = gdpPercap, y = fit, ymin = lwr, ymax = upr,
                 color = country,
                 fill = country,
                 group = country))
p4 + geom_point(data = subset(gapminder, country %in% c("Afghanistan", "Australia")),
                aes(x = gdpPercap, y = lifeExp, color = country),
                alpha = 0.5, inherit.aes = FALSE) +
  geom_line() +
  geom_ribbon(alpha = 0.1, color = FALSE) +
  scale_x_log10(labels = scales::dollar)

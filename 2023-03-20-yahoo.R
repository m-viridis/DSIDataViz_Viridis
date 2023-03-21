#import libraries
library (tidyverse)
library (socviz)
library (ggplot2)
library (gapminder)
library (RColorBrewer)

########################################################
#I prefer using ratio because most people won't be
#used to the idea of time moving in different directions
#which would increase cognitive load
########################################################

p <- ggplot(data = yahoo,
            mapping = aes(x = Year, y = Revenue/Employees))

p + geom_vline(xintercept = 2012) +
  geom_line(color = "gray60", size = 2) +
  annotate("text", x = 2013, y = 0.44,
           label = " Mayer becomes CEO", size = 2.5) +
  labs(x = "Year\n",
       y = "Revenue/Employees",
       title = "Yahoo Revenue to Employee Ratio, 2004-2014")


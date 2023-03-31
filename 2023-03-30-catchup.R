#DSI DATAVIZ CODEALONG
#March 30, 2023

#import libraries
library (ggplot2)
library (ggrepel)
library (ggthemes)
library (tidyverse)
library (socviz)
library(maps)

#explore data
election |>
  select(state, total_vote, r_points, pct_trump, party, census) |>
  sample_n(5)

#create thematic map with dotplots, facet by region
party_colors <- c("#2E74C0", "#CB454A")

p0 <- ggplot(data = subset(election, st %nin% "DC"),
             mapping = aes(x=r_points, y=reorder(state, r_points), color = party))

p1 <- p0 + geom_vline(xintercept = 0, color = "gray30") + geom_point(size=2)
p2 <- p1 + scale_color_manual(values = party_colors)
p3 <- p2 + scale_x_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30, 40),
                              labels = c("30\n (Clinton)", "20", "10", "0", "10", "20", "30", "40 \n(Trump)"))
p3 + facet_wrap(~ census, ncol=1, scales = "free_y") +
               guides(color=FALSE) + labs(x = "Point Margin", y = "") +
               theme(axis.text=element_text(size=8))


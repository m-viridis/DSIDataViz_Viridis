#Visualize Elections ON political donations data from 2014-2022
#Source: https://finances.elections.on.ca/en/contributions?fromYear=2014&toYear=2023

#######
#README
#######
#Please feel free to use the elec.csv file in the repo and skip the "read and prepare data" section below. If you wish to download/prep the data yourself, please keep reading.
#Methodology note: Because file sizes were so large, I downloaded this data in 9 batches, selecting the following restrictions:
#Party (only chose the three major parties) x Event (General Elections for 2014, 2018, 2022), 3x3 = 9
#If your internet is better, you may find it easier to just download datasets from the three years here: https://finances.elections.on.ca/en/downloads
#If you choose to do the three bulk downloads, you'll have to write your own code to merge the csv files.

##################
#import libraries
##################
library (ggplot2)
library (ggrepel)
library (ggthemes)
library (tidyverse)
library (readxl)

#######################
#read and prepare data
#######################

#open csv files and merge
#****make sure to change the filepaths to work with your machine!****
ndp2022 <- read.csv(file = 'Work/DSI/DataViz/2022NDP.csv')
lib2022 <- read.csv(file = 'Work/DSI/DataViz/2022LIB.csv')
pc2022 <- read.csv(file = 'Work/DSI/DataViz/2022PC.csv')
ndp2018 <- read.csv(file = 'Work/DSI/DataViz/2018NDP.csv')
lib2018 <- read.csv(file = 'Work/DSI/DataViz/2018LIB.csv')
pc2018 <- read.csv(file = 'Work/DSI/DataViz/2018PC.csv')
ndp2014 <- read.csv(file = 'Work/DSI/DataViz/2014NDP.csv')
lib2014 <- read.csv(file = 'Work/DSI/DataViz/2014LIB.csv')
pc2014 <- read.csv(file = 'Work/DSI/DataViz/2014PC.csv')

elec <- rbind(ndp2022, lib2022, pc2022, ndp2018, lib2018, pc2018, ndp2014, lib2014, pc2014)

#explore data
head(elec)
names(elec)
dim(elec) #63,852 rows, 15 col
str(elec) #n.b. the Amount column datatype will be chr/character so we'll need to convert

#convert data-type 
#source: https://stackoverflow.com/questions/31944103/convert-currency-with-commas-into-numeric

elec$Amount <- as.numeric(gsub('\\$', '', elec$Amount))
str(elec) #make sure it worked and there are no more $

#################################################################################
#create objects for plot
#n.b. colours correspond to political parties/are legible to full colourblindness
#see party_col_grayscale.jpg
#################################################################################

party_colours <- c("#BB4949", "#F89D28", "#143675")
party_names <- c("Liberal", "New Democrat", "Progressive Conservative")

###############################################
#summarize total giving per year/party and plot
###############################################

#plot basic bargraph
p <- ggplot (data = elec, 
             mapping = aes(x = Year, fill = Party.Abbreviation)) #use abbreviation to avoid inconsistent data
p1 <- p + geom_bar(position = "stack")

#correct years on xaxis and change Amount to $ on yaxis
p2 <- p1 + scale_x_continuous(breaks = c(2014, 2018, 2022)) +
  scale_y_continuous(labels=scales::dollar_format())

#change labels and positioning
p3 <- p2 + labs(title = "Ontario General Elections:", 
                subtitle = "Political Donations to Major Parties",
                x = "Year",
                y = "Donation Totals",
                fill = "Party",
                caption = "Source: Elections Ontario") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

#add custom colours and names for legend
p4 <- p3 + scale_fill_manual(values = party_colours, labels = party_names)
p4

#add theme for a more professional aesthetic
p5 <- p4 + theme_stata(base_family = "Verdana")
p5

#add alt text
#source: https://ggplot2.tidyverse.org/reference/get_alt_text.html
p6 <- p5 + labs(alt = paste("Barplot graph showing that political donations to the Progressive Conservative Party in the 2014, 2018 and 2022 general elections were about the same amount as total donations to the Liberal and New Democrat Parties combined. We also see NDP donations outpace LIB donations in 2018 and 2022."
))
get_alt_text(p6) #check the alt text works

p6


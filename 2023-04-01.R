#DSI DATAVIZ CODEALONG
#April 1, 2023

#import libraries
library (rAmCharts)
library (ggplot2)
library (gganimate)
library (gifski)
library (gapminder)

#interactive line graph
amPlot(iris, col=c("Sepal.Length", "Sepal.Width"),
       type = c("line", "step"),
       zoom = TRUE,
       legend = TRUE)

#animated ggplot setup
p <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, size = pop, colour = country)) +
  geom_point(show.legend = FALSE, alpha=0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2,12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p

#animate plot
p2 <- p + transition_time(year) +
  labs(title = "Year: {frame_time}")
animate(p2, duration = 10, renderer = gifski_renderer())

##interactive plot code##
#install libraries

library(shiny)
library(ggplot2)
library(dplyr)
library(gapminder)

# Define UI
ui <- fluidPage(
  #add title
  titlePanel("GDP & Life Expectancy"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", 
                  "Choose year",
                  choices = unique(gapminder$year))
    ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
  
)

#server
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    #modify data based on UI input
    plotdata <- gapminder |>
      filter(year == input$year)
    
    #create plot
    p <- ggplot(plotdata, aes(x=gdpPercap, y=lifeExp, size=pop, colour=country)) +
      geom_point(show.legend = FALSE, alpha = 0.7) +
      scale_color_viridis_d() +
      scale_size(range = c(2,12)) +
      scale_x_log10() +
      labs(x = "GDP per capita",
           y = "Life expectancy")
    p
  })
  
}

#run app 
shinyApp(ui = ui, server = server)

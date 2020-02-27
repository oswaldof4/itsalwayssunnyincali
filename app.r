# Second example!!!

library(shiny)
library(tidyverse)
library(shinythemes)

ui <- navbarPage("Waldo's naviagation bar!",
                 theme = shinytheme("cyborg"),
                 tabPanel("First tab!",
                          
                          h1("Some giant text - HI! It's Julia."),
                          p("Here's some regular text...what's up it's david"), 

                          plotOutput(outputId = "diamond_plot")),
                 tabPanel("Second tab!",
                          sidebarLayout(
                            sidebarPanel("Some text!",
                                         checkboxGroupInput(inputId = "diamondclarity",
                                                            "Choose some!",
                                                            choices = c(levels(diamonds$clarity)))
                            ),
                            mainPanel("Main panel text!",
                                      plotOutput(outputId = "diamond_plot2"))
                          ))
)

server <- function(input, output){
  
  output$diamond_plot <- renderPlot({
    ggplot(data = diamonds, aes(x = carat, y = price)) +
      geom_point(aes(color = clarity)) 
  })
  
  diamond_clarity <- reactive({
    diamonds %>% 
      filter(clarity %in% input$diamondclarity)
  })
  
  output$diamond_plot2 <- renderPlot({
    ggplot(data = diamond_clarity(), aes(x = clarity, y = price)) +
      geom_violin(aes(fill = clarity))
  })
}

shinyApp(ui = ui, server = server)
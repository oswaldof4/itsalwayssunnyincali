# Second example!!!

library(shiny)
library(tidyverse)
library(shinythemes)

ui <- navbarPage("It's always sunny in California",
                 theme = shinytheme("cyborg"),
                 tabPanel("Instructions!",
                          h1("Some giant text - something new from Oswaldo"),
                          p("Here's some regular text... something new from david"),
                          plotOutput(outputId = "diamond_plot")),
                 tabPanel("First tab!",
                          sidebarLayout(
                            sidebarPanel("Some text!",
                                         checkboxGroupInput(inputId = "diamondclarity",
                                                            "Choose some!",
                                                            choices = c(levels(diamonds$clarity)))
                            ),
                            mainPanel("Main panel text!",
                                      plotOutput(outputId = "diamond_plot2"))
                          )),
                 tabPanel("Second tab!",
                          sidebarLayout(
                            sidebarPanel("Some text!",
                                         checkboxGroupInput(inputId = "diamondclarity",
                                                            "Choose some!",
                                                            choices = c(levels(diamonds$clarity)))
                            ),
                            mainPanel("Main panel text!",
                                      plotOutput(outputId = "diamond_plot2"))
                          )),
                 tabPanel("Third tab!",
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
      geom_point(aes(color = clarity)) +
      theme_minimal()
  })
  
  diamond_clarity <- reactive({
    diamonds %>% 
      filter(clarity %in% input$diamondclarity)
  })
  
  output$diamond_plot2 <- renderPlot({
    ggplot(data = diamond_clarity(), aes(x = clarity, y = price)) +
      geom_violin(aes(fill = clarity), alpha = 0.5)
  })
}

shinyApp(ui = ui, server = server)
# Second example!!!

library(shiny)
library(tidyverse)
library(shinythemes)

ui <- navbarPage("California solar electricity exploration",
                 shinythemes::themeSelector(),
                 tabPanel("Instructions!",
                          h1("It's always sunny in California"),
                          p("Here's where we would write about what the app is, what it does, and how to use it."),
                          plotOutput(outputId = "diamond_plot")
                          ),
                 tabPanel("Timelapse map of solar capacity by county",
                          h2("Map of solar capacity by county"),
                          p("Solar capacity by county indicated by shading for the
                            selected time span. Darker shaded counties indicate more
                            solar capacity. Hover cursor over county to view exact capacity."),
                          sidebarLayout(
                            sidebarPanel("Select year",
                                          sliderInput(inputId = "placeholder1",
                                                      "(range of years)",
                                                      min = min(diamonds$carat),
                                                      max = max(diamonds$carat),
                                                      value = 1
                                                      )
                                         ),
                            mainPanel("Main panel text!",
                                      plotOutput(outputId = "temp_plot")
                                      ) 
                            )
                          ),
                 tabPanel("Electricity use by county",
                          h2("Selected county name electricity and solar statistics"),
                          p("Total county electricity usage and solar generation"),
                          sidebarLayout(
                            sidebarPanel("Some text!",
                                         checkboxGroupInput(inputId = "diamondclarity",
                                                            "Choose some!",
                                                            choices = c(levels(diamonds$clarity)))
                                         ),
                            mainPanel("Main panel text!",
                                      plotOutput(outputId = "diamond_plot2")
                                      ) # Can't seem to figure out why this plot isn't showing up
                            )
                          ),
                 tabPanel("Low income solar",
                          h2("Low income solar participation by county"),
                          sidebarLayout(
                            sidebarPanel("Some text!",
                                         checkboxGroupInput(inputId = "placeholder2",
                                                            "Choose some!",
                                                            choices = c(levels(diamonds$clarity))
                                                            )
                                         ),
                            mainPanel("Main panel text!",
                                      plotOutput(outputId = "temp_plot2")
                                      )
                            )
                          )
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
  
  output$temp_plot <- renderPlot({
    ggplot(data = diamonds, aes(x = carat, y = price)) +
      geom_point(aes(color = clarity)) +
      theme_minimal()
  })
  
  # diamond_clarity <- reactive({
  #   diamonds %>% 
  #     filter(clarity %in% input$placeholder1)
  # })
}

shinyApp(ui = ui, server = server)
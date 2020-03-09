# Second example!!!

library(shiny)
library(tidyverse)
library(shinythemes)
library(lubridate)
library(janitor)
library(here)
library(plotly)

# -------------------------------------
# Joining plant datasets
# -------------------------------------

# Read in data 

gen_by_plant <- read_csv(here::here("data", "annual_generation_by_power_plant.csv"))

plant_info <- read_csv(here::here("data", "plant_info.csv"), 
                       skip = 12, col_names = TRUE)

# Clean up data

tidy_gen_by_plant <- gen_by_plant %>% 
  clean_names() %>% 
  select(year, state, cec_plant_id, status, start_date, 
         retire_date, prime_mover_id, prime_mover_desc, 
         capacity, gross_m_wh, net_m_wh) %>% 
  rename("gross_mwh" = "gross_m_wh") %>% 
  rename("net_mwh" = "net_m_wh") %>% 
  # filter for just photovoltaics (this excludes solar thermal, wind, etc.)
  filter(prime_mover_id == "PV")
# for visuals, remember to filter out non-operational systems once they have a "retire date" so they still show up if a year is selected before retirement

tidy_plant_info <- plant_info %>% 
  clean_names() %>% 
  # we'll already have state in the other df
  select(-state)

# Join dataframes

plants_location_join <- inner_join(tidy_plant_info, tidy_gen_by_plant, "cec_plant_id")
# note that there are a 19 entries wiht energy_source_category not equal to "SUN" but prime_mover_id equal to "PV"

# David's tab
solar_capacity_df <- plants_location_join %>% 
  select(-resource_id, -resource_id_name) %>%
  filter(status == "OP") %>% 
  mutate(plant_name = fct_reorder(plant_name, desc(capacity))) %>% 
  group_by(year, plant_name, county) %>% 
  summarize(total_capacity = (sum(capacity))) 


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
                                         selectizeInput(inputId = "county_selection",
                                                        "Choose some!",
                                                        choices = c(unique(solar_capacity_df$county)),
                                                        multiple = T)
                            ),
                            mainPanel("Main panel text!",
                                      plotOutput(outputId = "solar_capacity_plot")
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
  
  solar_capacity <- reactive({
    solar_capacity_df %>% 
      filter(county %in% input$county_selection)
  })
  
  output$solar_capacity_plot <- renderPlot({
    ggplot(data = solar_capacity(),
           aes(x = year,
               y = total_capacity,
               group = plant_name)) +
      geom_col(color = alpha("black",.1), fill = "orange", alpha = 1, show.legend = FALSE) +
      labs(title = "Solar capacity (2008-2018)",
           # add to title **reactive text** "in selected county name" 
           x = "Year",
           y = "Cumulative capacity installed (MW)") +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_continuous(lim = c(2008,2019), expand = c(0,0), breaks = seq(2008, 2018, 2)) +
      theme_classic()
  })
  
  # diamond_clarity <- reactive({
  #   diamonds %>% 
  #     filter(clarity %in% input$placeholder1)
  # })
  
  
  
  
  
  # --------------------
  
}

shinyApp(ui = ui, server = server)
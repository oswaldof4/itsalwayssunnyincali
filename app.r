# Make sure we have all the necessary libraries

library(shinythemes)
library(tidyverse)
library(lubridate)
library(janitor)
library(shiny)
library(here)
library(sf)

# ---- Load in raw data ----

# Read in plant generation data 

gen_by_plant <- read_csv(here::here("data", "annual_generation_by_power_plant.csv"))

plant_info <- read_csv(here::here("data", "plant_info.csv"), 
                       skip = 12, col_names = TRUE)

# Read in CA shape file
ca <- read_sf(dsn = here::here("data", "states"),
              layer = "cb_2017_us_state_20m") %>% 
  dplyr::select(NAME) %>% 
  filter(NAME == "California") %>% 
  st_transform(crs = 4326)

# Read in CA counties
ca_counties <- read_sf(dsn = here::here("data", "ca_counties"), 
                       layer = "california_county_shape_file") %>% 
  rename(county = "NAME") %>% 
  select(county) %>% 
  group_by(county) %>% 
  summarize()  
st_crs(ca_counties) = 4326

# ---- Julia's data wrangling ----

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

# Get plant capacity by county and year
plant_capacity <- plants_location_join %>% 
  filter(status == "OP") %>% 
  group_by(county, year) %>% 
  summarize(tot_capacity = sum(capacity))

# Join capacity information to CA counties and make sf object with sticky geometries
cap_county_join <- full_join(plant_capacity, ca_counties, "county")
cap_county_sf <- st_as_sf(cap_county_join) 

# ---- David's data wrangling ----

solar_capacity_df <- plants_location_join %>% 
  select(-resource_id, -resource_id_name) %>%
  filter(status == "OP") %>% 
  mutate(plant_name = fct_reorder(plant_name, desc(capacity))) %>% 
  group_by(year, plant_name, county) %>% 
  summarize(total_capacity = (sum(capacity))) 

# ---- Waldo's data wrangling ----


# ---- User interface ----

ui <- navbarPage("California solar electricity exploration",
                 theme = shinytheme("sandstone"),
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
                                         sliderInput(inputId = "yearselection",
                                                     "(range of years)",
                                                     min = 2001,
                                                     max = 2018,
                                                     value = 2005
                                         ) # Years 2001-2005 are all the same
                                         # Could we add individual points for plants?
                            ),
                            mainPanel("Main panel text!",
                                      plotOutput(outputId = "capacity_map_plot")
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
                            )
                          )
                 ),
                 tabPanel("Low income solar",
                          h2("Low income solar participation by county"),
                          sidebarLayout(
                            sidebarPanel("Some text!",
                                         checkboxGroupInput(inputId = "diamondclarity",
                                                            "Choose some!",
                                                            choices = c(levels(diamonds$clarity))
                                         )
                            ),
                            mainPanel("Main panel text!",
                                      plotOutput(outputId = "diamond_plot2")
                            )
                          )
                 )
)

# ---- Server code (reactive only) ----

server <- function(input, output){
  
  # Julia's tab
  capacity_map_sf <- reactive({
    cap_county_sf %>% 
      filter(year %in% input$yearselection)
  })
  
  output$capacity_map_plot <- renderPlot({
    ggplot() +
      geom_sf(data = ca) +
      geom_sf(data = capacity_map_sf(), 
              aes(fill = tot_capacity), 
              size = 0.2, 
              color = "white") +
      scale_fill_continuous(low = "yellow", high = "red") +
      theme_minimal() +
      labs(fill = "Total Capacity")
  })
  
  # David's tab
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
  
  # Placeholders
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
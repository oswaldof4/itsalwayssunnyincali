# Make sure we have all the necessary libraries

library(shinythemes)
library(tidyverse)
library(lubridate)
library(janitor)
library(shiny)
library(here)
library(sf)
library(gt)

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

# read in data from US Energy Information Administration, annual generation by state for each resource type from 1990 - 2018 and remove first row

# Could not figure out code using tidyverse, so run these three lines of code before using annual_generation_state to make tidy_gen_state

annual_generation_state <- read_csv("annual_generation_state.csv")
colnames(annual_generation_state) <- as.character(unlist(annual_generation_state[1,]))
annual_generation_state = annual_generation_state[-1, ]


# ---- Tab 2 data wrangling ----

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

# ---- Tab 3 data wrangling ----

# total operational capacity for each year
solar_capacity_df <- plants_location_join %>% 
  select(-resource_id, -resource_id_name) %>%
  filter(status == "OP") %>% 
  mutate(plant_name = fct_reorder(plant_name, desc(capacity))) %>% 
  group_by(year, county, plant_name, ) %>% 
  summarize(total_capacity = (sum(capacity))) %>% 
  mutate(annual_count = n())


# % of total California solar capacity in 2018
solar_capacity_pct <- plants_location_join %>% 
  filter(status == "OP") %>% 
  filter(year == 2018) %>%
  mutate(ca_capacity = sum(capacity)) %>%
  group_by(county, ca_capacity) %>% 
  summarize(county_capacity = sum(capacity)) %>% 
  mutate(pct_total = county_capacity/ca_capacity)


# median system size
solar_cap_median <- plants_location_join %>% 
  filter(status == "OP") %>% 
  filter(year == 2018) %>%
  group_by(county) %>%
  summarize(median_cap = median(capacity))

# numberof homes powered
# Resource for 258.18 homes / MW stat: https://www.seia.org/initiatives/whats-megawatt
homes_powered_df <- plants_location_join %>% 
  filter(status == "OP") %>% 
  filter(year == 2018) %>%
  mutate(homes_per_MW = 258.1818) %>% 
  mutate(total_homes_powered = sum(capacity*homes_per_MW)) %>% 
  group_by(county, total_homes_powered) %>%
  summarize(homes_powered = sum(capacity*homes_per_MW))

# ---- Tab 3 continued data wrangling ----

part_table_df <- full_join(solar_capacity_pct, solar_cap_median) %>% 
  select(-ca_capacity, -county_capacity)

table_df <- full_join(part_table_df, homes_powered_df) %>% 
  select(-total_homes_powered)

# ----- Tab 4 data wrangling ---------

# Make tidy dataframe with generation by state, resource and year

tidy_gen_state <- annual_generation_state %>% 
  clean_names()

# Get total mwh generation of solar and total for each state in 2018
solar_by_state <- tidy_gen_state %>% 
  mutate(mwh_numeric = as.numeric(gsub(",", "", generation_megawatthours))) %>% 
  filter(energy_source %in% c("Total", "Solar Thermal and Photovoltaic")) %>% 
  select(year, state, energy_source, generation_megawatthours, mwh_numeric) %>% 
  group_by(state, energy_source, year) %>% 
  summarize(energy_source_sum = sum(mwh_numeric))

# Get solar as fraction of total for each state by putting into wide format
solar_frac_state <- solar_by_state %>% 
  pivot_wider(names_from = energy_source, 
              values_from = energy_source_sum) %>% 
  clean_names() %>% 
  mutate(solar = replace_na(solar_thermal_and_photovoltaic, 0)) %>% 
  mutate(solar_frac = (solar / total)) %>%
  mutate(year = as.numeric(year)) 

# put back into longer format for ggplot
# total_out_of_solar_longer <- total_out_of_solar_state %>% 
#   pivot_longer(cols = c(total, solar_thermal_and_photovoltaic, total_out_of_solar)) %>% 
#   filter(name == "total_out_of_solar") %>% 
#   filter(state %in% c("CA", "AZ"))

# Make graph of solar compared to all other energy for CA in 2018

ggplot(data = solar_frac_state, 
       aes(x = year, 
           y = solar_frac)) +
  geom_line(aes(color = state)) +
  theme_minimal() +
  labs(x = "Year", 
       y = "Solar as fraction of total generation")

ggplot(data = solar_frac_state, 
       aes(x = year, 
           y = total)) +
  geom_line(aes(color = state)) +
  theme_minimal() +
  labs(x = "Year", 
       y = "Total Annual Solar Generation (Mwh)")


# ---- User interface ----

ui <- navbarPage("It's Always Sunny in California",
                 theme = shinytheme("sandstone"),
                 tabPanel("Home",
                          sidebarLayout(
                            sidebarPanel(h1("California solar exploration"),
                                         p("The United States currently has more than 27 gigawatts of utility scale solar projects in operation and California is home to approximately 40% of them. The California Energy Commission collects data from power plants with a total nameplate capacity of 1MW or more that are located within California or within a control area with end users inside California. This web application allows the user to explore where these solar plants are located county by county.")
                            ),
                            mainPanel(img(src="featured_image_topaz.jpg", height = "75%", width = "100%", style = 'display: block;'),
                                      p("Topaz Solar Farm in San Luis Obispo county. One of the largest solar plants in the world, it was completed in 2014, cost $2.5 billion to build, and has a capacity of 550 megawatts. Photo credit: First Solar via GigaOm")
                            ) 
                          ),
                          h2(HTML("<strong>Citations</strong>")),
                          p("Electric Power Annual 2018. U.S. Energy Information Administration. October 2019"),
                          p("Fehrenbacher, K., 2015. Special report: How the rise of a mega solar panel farm shows us the future of energy. GigaOm. January 2015. https://gigaom.com/2015/01/20/a-special-report-the-rise-of-a-mega-solar-panel-farm-why-its-important/."
                          ),
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
                                                     min = 2008,
                                                     max = 2018,
                                                     value = 2008
                                         ) # Years 2001-2005 are all the same
                                         # Could we add individual points for plants?
                            ),
                            mainPanel("",
                                      plotOutput(outputId = "capacity_map_plot")
                            ) 
                          )
                 ),
                 tabPanel("Solar electricity generation by county",
                          h2("Cumulative megawatts of solar capacity per county from 2008-2018."),
                          p(""),
                          sidebarLayout(
                            sidebarPanel("",
                                         selectizeInput(inputId = "county_selection",
                                                        "Choose a county:",
                                                        choices = c(unique(solar_capacity_df$county)),
                                                        multiple = T,
                                                        selected = "Santa Barbara")
                            ),
                            mainPanel(gt_output(outputId = "gt")
                            ) # Maybe we should add largest capacity plant + name?
                          ),
                          plotOutput(outputId = "solar_capacity_plot"),
                 ),
                 tabPanel("CA vs. other states",
                          h2("Solar statistics by state"),
                          sidebarLayout(
                            sidebarPanel("Some text!",
                                         checkboxGroupInput(inputId = "diamondclarity",
                                                            "Choose some!",
                                                            choices = c(levels(diamonds$clarity))
                                         ),
                                         selectizeInput(inputId = "state_selection",
                                                        "Choose a state:",
                                                        choices = c(unique(solar_frac_state$state)),
                                                        multiple = T,
                                                        selected = "CA")
                            ),
                            mainPanel("Main panel text!",
                                      plotOutput(outputId = "solar_pct_plot")
                            )
                          )
                 )
)

# ---- Server code (reactive only) ----

server <- function(input, output){
  

  # Tab 2
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
      labs(fill = "Total Capacity (MW)")
  })
  
  # Tab 3
  solar_capacity <- reactive({
    solar_capacity_df %>% 
      filter(county %in% input$county_selection)
  })
  
  output$solar_capacity_plot <- renderPlot({
    ggplot(data = solar_capacity(),
           aes(x = year,
               y = total_capacity,
               group = plant_name)) +
      geom_col(color = alpha("black",.1), aes(fill = annual_count), alpha = 1, show.legend = TRUE) +
      scale_fill_continuous(low = "yellow", high = "orange") +
      labs(title = "Solar capacity (2008-2018)",
           x = "Year",
           y = "Cumulative capacity installed (MW)",
           fill = "Count of installations") +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_continuous(lim = c(2008,2019), expand = c(0,0), breaks = seq(2008, 2018, 2)) +
      theme_classic() +
      theme(legend.justification = c(0,1),
            legend.position = c(0.01,0.95),
            legend.background = element_blank(),
            legend.key = element_blank()) +
      facet_wrap(~county)
  })
  
  county_stat <- reactive({
    table_df %>% 
      filter(county %in% input$county_selection)
  })
  
  output$gt <- render_gt({
    county_stat() %>% 
      gt(groupname_col = NULL) %>%
      tab_header(
        title = "County statistics" #,
        # subtitle = "Subtitle"
      ) %>% 
      fmt_percent(
        columns = vars(pct_total),
        decimals = 1) %>%
      fmt_number(
        columns = vars(homes_powered),
        decimals = 0,
        use_seps = T
      ) %>% 
      cols_label(
        county = "",
        pct_total = "Percentage of CA solar",
        median_cap = "Median plant capacity",
        homes_powered = "Equivalent homes powered"
      ) %>% 
      cols_align(align = "center")
  })
  
  # Tab 4
  diamond_clarity <- reactive({
    diamonds %>% 
      filter(clarity %in% input$diamondclarity)
  })
  
  solar_frac <- reactive({
    solar_frac_state %>% 
      filter(state %in% input$state_selection)
  })
  
  output$solar_pct_plot <- renderPlot({
    ggplot(data = solar_frac(), 
           aes(x = year, 
               y = solar_frac)) +
      geom_line(aes(color = state)) +
      theme_minimal() +
      labs(x = "Year", 
           y = "Solar as fraction of total generation")
  })
  
}

shinyApp(ui = ui, server = server)
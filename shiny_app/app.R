#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(geojsonio)
require(leaflet)
require(dplyr)
require(tidyr)
require(rjson)
require(tidyverse)
require(reshape2)
require(readr)
library(shinyWidgets)
library(viridis)

pubs = read_csv("../clean/pubs.csv")
college_locs = read_csv("../clean/college_locs.csv")
venues = read_csv("../clean/venues.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("CS Graduate School Exploration"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          # Switch to toggle scaling by faculty
          materialSwitch(inputId = "scale_by_faculty", value = TRUE, label = "Scale by faculty"),

          # Slider to select year range
          sliderInput(inputId = "year_range", label = h3("Publication range"), min = min(pubs$year), 
                      max = max(pubs$year), value = c(min(pubs$year), max(pubs$year)), sep = "", step = 1, animate = TRUE),
          
          checkboxGroupInput(inputId = "selected_areas", label = h3("Areas"), 
                             choices = list("AI" = "AI", "Interdisciplinary" = "Interdisciplinary", "Systems" = "Systems", "Theory" = "Theory", "Bad" = "Bad"),
                             selected = c("AI", "Interdisciplinary", "Systems", "Theory"))),
          
          # checkboxGroupInput(inputId = "selected_subs", label = h3("Subareas"), 
          #                    choices = list("AI", 
          #                                   "Computer vision", 
          #                                   "Machine learning & data mining",
          #                                   "Natural language processing",
          #                                   "The Web & information retrieval",
          #                                   "Computer architecture",
          #                                   "Computer networks",
          #                                   "Computer security",
          #                                   "Databases",
          #                                   "Design automation",
          #                                   "Embedded & real-time systems",
          #                                   "High-performance computing",
          #                                   "Mobile computing",
          #                                   "Measurement & perf. analysis",
          #                                   "Operating systems",
          #                                   "Programming languages",
          #                                   "Software engineering",
          #                                   "Algorithms & complexity",
          #                                   "Cryptography",
          #                                   "Logic & verification",
          #                                   "Comp. bio & bioinformatics",
          #                                   "Computer graphics",
          #                                   "Economics & computation",
          #                                   "Human-computer interaction",
          #                                   "Robotics",
          #                                   "Visualization"), selected = c("AI", "Computer vision", "Machine learning & data mining",
          #                                                                  "Natural language processing",
          #                                                                  "The Web & information retrieval",
          #                                                                  "Computer architecture",
          #                                                                  "Computer networks",
          #                                                                  "Computer security",
          #                                                                  "Databases",
          #                                                                  "Design automation",
          #                                                                  "Embedded & real-time systems",
          #                                                                  "High-performance computing",
          #                                                                  "Mobile computing",
          #                                                                  "Measurement & perf. analysis",
          #                                                                  "Operating systems",
          #                                                                  "Programming languages",
          #                                                                  "Software engineering",
          #                                                                  "Algorithms & complexity",
          #                                                                  "Cryptography",
          #                                                                  "Logic & verification",
          #                                                                  "Comp. bio & bioinformatics",
          #                                                                  "Computer graphics",
          #                                                                  "Economics & computation",
          #                                                                  "Human-computer interaction",
          #                                                                  "Robotics",
          #                                                                  "Visualization"))
          # 
        
        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("myplot")#,
           #verbatimTextOutput("debug_text")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #output$debug_text = renderPrint({"hello"})
  map_data_reactive = reactive({
    pubs = filter(pubs, 
                  between(year, input$year_range[1], input$year_range[2]) 
                  & (umbrella %in% input$selected_areas) | (is.na(umbrella) & "Bad" %in% input$selected_areas))
    
  faculty_counts = pubs %>%
    filter(!is.na(institution)) %>%
    group_by(institution, name) %>%
    summarise() %>%
    summarise(fac_counts = n())
  
  # Normalize by size -- is it by department -- have this toggleable might make map look better
  # Do it by year -- might need to do it in plotly  -- might need to do map in plotly
  # Maybe filter pubs by venue/area before doing this
  # Maybe have pie chart pop up at each dot to show breakdown of pubs
  counts = pubs %>%
    filter(!is.na(institution)) %>%
    group_by(institution) %>%
    summarise(dis = n()) %>%
    mutate(freq = dis / sum(dis))
  
  # Get area per college with most pubs
  counts_by_area = pubs %>%
    filter(!is.na(institution)) %>%
    group_by(institution, umbrella) %>%
    summarise(dis = n()) %>%
    mutate(strong_area = umbrella[which.max(dis)])
  
  counts = left_join(counts, faculty_counts, by = c("institution"))
  
  counts = counts %>%
    mutate(count_per_faculty = dis/fac_counts)
  
  # Min-max scaling to get good circle sizes
  BIG_MARKER_SIZE = 30
  MIN_MARKER_SIZE = 2
  
  counts = counts %>%
    mutate(radius = (freq- min(freq)) / (max(freq)-min(freq)) * BIG_MARKER_SIZE + MIN_MARKER_SIZE) %>%
    mutate(radius_by_faculty = ((count_per_faculty - min(count_per_faculty)) / (max(count_per_faculty)-min(count_per_faculty))) * BIG_MARKER_SIZE + MIN_MARKER_SIZE)
  
  # Only get colleges that have a corresponding location
  map_data = left_join(counts, college_locs, by = c("institution" = "college_name"))
  
  # Add in counts_by_area data
  map_data = left_join(map_data, counts_by_area, by = c("institution"))
  
  # Color palette
  group_pal = colorFactor(viridis(7), map_data$strong_area)
  map_data
  })
    output$myplot <- renderLeaflet({
       # Make map
      if(!input$scale_by_faculty) {
      map = leaflet(data = map_data_reactive()) %>% 
        addTiles() %>%
        addCircleMarkers(lng = ~long, lat = ~lat, 
                         label = ~institution, radius = ~radius, fillColor = ~group_pal(strong_area), color = ~group_pal(strong_area), opacity=0.0, fillOpacity=0.4) %>%
        addLegend("bottomright", pal = group_pal, values = ~strong_area)
      } else {
        map = leaflet(data = map_data_reactive()) %>% 
          addTiles() %>%
          addCircleMarkers(lng = ~long, lat = ~lat, 
                           label = ~institution, radius = ~radius_by_faculty, fillColor = ~group_pal(strong_area), color = ~group_pal(strong_area), opacity=0.0, fillOpacity=0.4) %>%
          addLegend("bottomright", pal = group_pal, values = ~strong_area)
      }
      map
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

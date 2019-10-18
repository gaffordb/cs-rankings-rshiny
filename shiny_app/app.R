library(shiny)
library(shinyjs)
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

# Get data
pubs = read_csv(file.path("clean", "pubs.csv"))
college_locs = read_csv(file.path("clean", "college_locs.csv"))
venues = read_csv(file.path("clean", "venues.csv"))

# Define subsections and code-names
ai_subs = list("AI" = "ai",
               "Computer vision" = "vision",
               "Machine learning & data mining" = "mlmining",
               "Natural language processing" = "nlp",
               "The Web & information retrieval" = "ir")

systems_subs = list("Computer architecture" = "arch",
                    "Computer networks" = "comm",
                    "Computer security" = "sec",
                    "Databases" = "mod",
                    "Design automation" = "da",
                    "Embedded & real-time systems" = "bed",
                    "High-performance computing" = "hpc",
                    "Mobile computing" = "mobile",
                    "Measurement & perf. analysis" = "metrics",
                    "Operating systems" = "ops",
                    "Programming languages" = "plan",
                    "Software engineering" = "soft")

theory_subs = list("Algorithms & complexity" = "act",
                   "Cryptography" = "crypt",
                   "Logic & verification" = "log")

interdisciplinary_subs = list("Comp. bio & bioinformatics" = "bio",
                              "Computer graphics" = "graph",
                              "Economics & computation" = "ecom",
                              "Human-computer interaction" = "chi",
                              "Robotics" = "robotics",
                              "Visualization" = "vis")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("CS Graduate School Exploration"),
    shinyjs::useShinyjs(),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          # Switch to toggle scaling by faculty
          materialSwitch(inputId = "scale_by_faculty", value = FALSE, label = "Scale by faculty"),

          # Slider to select year range
          sliderInput(inputId = "year_range", label = h3("Publication range"), 
                      min = min(pubs$year), max = max(pubs$year), 
                      value = c(min(pubs$year), max(pubs$year)), sep = "", 
                      step = 1, animate = TRUE),
          
          # Pickers for subsections
          pickerInput(
            inputId = "ai_subs",
            label = "AI",
            multiple = TRUE,
            options = list(`actions-box` = TRUE, `show-content` = FALSE),
            choices = ai_subs,
            selected = ai_subs
          ),
          
          pickerInput(
            inputId = "systems_subs",
            label = "Systems",
            multiple = TRUE,
            options = list(`actions-box` = TRUE),
            choices = systems_subs,
            selected = systems_subs
          ),
          
          pickerInput(
            inputId = "theory_subs",
            label = "Theory",
            multiple = TRUE,
            options = list(`actions-box` = TRUE),
            choices = theory_subs,
            selected = theory_subs
          ),
          
          pickerInput(
            inputId = "interdisciplinary_subs",
            label = "Interdisciplinary",
            multiple = TRUE,
            options = list(`actions-box` = TRUE),
            choices = interdisciplinary_subs,
            selected = interdisciplinary_subs
          ),
          
          # Input to choose additional conferences of interest
          selectizeInput(
            inputId = "additional_conferences",
            label = h3("Add conferences"),
            choices = (unique(pubs$conf)),
            multiple = TRUE
          )),
        
        
        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("myplot"),
           verbatimTextOutput("debug_text")
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  map_data_reactive = reactive({
    # All selected areas
    selected_areas = c(
      input$ai_subs,
      input$systems_subs,
      input$theory_subs,
      input$interdisciplinary_subs
    )
    
    # Needs to be a selected conf or in a selected area
    pubs = filter(pubs,
                  (
                    between(year, input$year_range[1], input$year_range[2])
                    & (area %in% selected_areas)
                  )
                  | conf %in% input$additional_conferences)
    
    # Only execute below if pubs is not empty
    if (nrow(pubs) > 0) {
      # Get num active faculty per univ
      faculty_counts = pubs %>%
        filter(!is.na(institution)) %>%
        group_by(institution, name) %>%
        summarise() %>%
        summarise(fac_counts = n())
      
      # Get pubs per univ
      counts = pubs %>%
        filter(!is.na(institution)) %>%
        group_by(institution) %>%
        summarise(dis = n()) %>%
        mutate(freq = dis / sum(dis))
      
      # Get area per univ with most pubs
      counts_by_area = pubs %>%
        filter(!is.na(institution)) %>%
        group_by(institution, umbrella) %>%
        summarise(dis = n()) %>%
        mutate(strong_area = umbrella[which.max(dis)])
      
      # Add faculty_counts to counts
      counts = left_join(counts, faculty_counts, by = c("institution"))
      
      counts = counts %>%
        mutate(count_per_faculty = dis / fac_counts)
      
      # Min-max scaling to get good circle sizes
      BIG_MARKER_SIZE = 30
      MIN_MARKER_SIZE = 2
      
      # Add radius information
      counts = counts %>%
        mutate(radius = (freq - min(freq)) / (max(freq) - min(freq)) * BIG_MARKER_SIZE 
               + MIN_MARKER_SIZE) %>%
        mutate(radius_by_faculty = ((
          count_per_faculty - min(count_per_faculty)) / (max(count_per_faculty) 
                                                         - min(count_per_faculty)
        )) * BIG_MARKER_SIZE + MIN_MARKER_SIZE)
      
      # Only get colleges that have a corresponding location
      map_data = left_join(counts, college_locs, by = c("institution" = "college_name"))
      
      # Add in counts_by_area data
      map_data = left_join(map_data, counts_by_area, by = c("institution"))
      
      map_data
    } else {
      NULL
    }
  })
  
    output$myplot <- renderLeaflet({
      if(!is.null(map_data_reactive())) {
      group_pal = colorFactor(viridis(7), map_data_reactive()$strong_area)
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
      } else {
        map = leaflet() %>% addTiles()
      }
      map
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

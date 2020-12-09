
# Code for producing a shiny app

# Aim of CarniDIET Shiny app:
# - Interactive map of all study (unique lat/long) locations
#   - Users can subset by family, genus or individual species
#   - families are coloured differently 
#   - When a species is selected, a silhouette of the species is downloaded from Phylopic and placed in the top
#   - Markers will be stock silhouettes of the species in question or the genus (use the Phylopic extraction function)
#
# - Studies can be selected and information extracted from each of the studies
#   - Diet composition
#   - Prey species list
#   - Sample size
#   - Duration of study
#   
# - It will be have an interactive query from that allows the user to download what they want from the dataset

# Packages ----
library(pacman)
pacman::p_load(shiny, leaflet, sf, sp, tidyverse, shinythemes)

# Loading data -----
carnidiet <- read.csv("../Data/data.csv") # CarniDIET
# current.ranges <- st_read("./Data", "current.ranges") # Current geographic ranges
# pn.ranges <- st_read("./Data", "pn.ranges") # Potentual geographic ranges

#  Get individual studies to show distribution on the map
studies <- carnidiet %>% 
  dplyr::group_by(familyCarni, scientificNameCarni, lifeStageCarni, sexCarni,
                  sourcePrimaryReference, geographicRegion,country, stateProvince,
                  county, municipality, verbatimLocality, protectedAreaHigher, protectedAreaLower, islandGroup, island,
                  decimalLatitude, decimalLongitude) %>% 
  dplyr::summarise(x = length(scientificNameCarni))

# Remove those with no coordinates:
studies <- studies %>% filter(!is.na(decimalLatitude) |
                              !is.na(decimalLongitude) |
                              !decimalLatitude == 0 |
                              !decimalLongitude == 0)

# Change names of coordinatres
tib <- as_tibble(studies)

map_world <- borders("world", colour="grey", fill = "lightgray")


# CarniDIET Shiny App ----

# Requires:
# UI (User Interface): What the user will see
# Server: Code for how to build/rebuild R objects to display in the UI
# shinyApp: Combines the above into an App.


# ui.R ----
ui <- fluidPage(title = "CarniDIET",
                verticalLayout(titlePanel("The spatial distribtion of records in CarniDIET 1.0"),
                               plotOutput("global_map", height = "500px", width = "900px"),
                               wellPanel(selectInput(inputId = "family_map", 
                                                     label = "Choose family", 
                                                     choices = unique(tib$familyCarni),
                                                     selected = unique(tib$familyCarni)[1],
                                                     multiple = TRUE))
                               )
)

# server.R ----
server <- function(input, output) {
  
  # Plot map
  output$global_map <- renderPlot(
  ggplot() +
    map_world +
    geom_point(pch = 21, aes(x = decimalLongitude,
                   y = decimalLatitude,
                   fill = familyCarni),
               colour = "black", size = 3, alpha = 0.5,
               data = tib[tib$familyCarni %in% input$family_map,]) +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          panel.border = element_rect(colour="grey", fill = NA, size=1),
          panel.background = element_rect(fill="#FCFCFC"),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 15)) +
    facet_wrap(.~familyCarni) +
    guides(colour = guide_legend(override.aes = list(size = 10)))
  )
  
}

# Run app ----
shinyApp(ui = ui, server = server)

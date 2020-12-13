
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
pacman::p_load(shiny, leaflet, sf, sp, tidyverse, shinythemes,
               shinyWidgets, golem,RColorBrewer, scales,
               lattice, dplyr)

# Loading data -----
carnidiet <- read.csv("./Data/CarniDIET 1.0.csv") # CarniDIET

#  Get individual studies to show distribution on the map
studies <- carnidiet %>% 
  dplyr::group_by(familyCarni, scientificNameCarni, lifeStageCarni, sexCarni,
                  sourceTitle, geographicRegion,country, stateProvince,
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
tib$id <- 1:nrow(tib)

# CarniDIET Shiny App ----

# Requires:
# UI (User Interface): What the user will see
# Server: Code for how to build/rebuild R objects to display in the UI
# shinyApp: Combines the above into an App.


shinyApp(
  
  
  
ui = fluidPage(
  
  # UI Theme
  #theme = shinytheme("united"),
  
  tags$head(HTML("<title> CarniDIET 1.0 </title>")),
  
  navbarPage(title = span(img(src = "logosmall.png", style = "margin-top: -14px", height = 65))),
  
  # Name of the app
  #column(3, offset = 5, titlePanel(img(src = "logosmall.png"))),
  
  #titlePanel(img(src = "logosmall.png")),
  
  # Panel layout names and UI output  
  
    sidebarPanel(
        #img(src = "logosmall.png"),
        selectizeGroupUI(
          id = "my-filters",
          inline = FALSE,
          params = list(
            family = list(inputId = "familyCarni", title = "Select family", placeholder = "select"),
            species = list(inputId = "scientificNameCarni", title = "Select species", placeholder = "select"),
            country = list(inputId = "country", title = "Select country", placeholder = "select"))),
        # uiOutput("family"),
        # uiOutput("species"),
        # uiOutput("country"),
        downloadButton('download', "Download the data")
    ),
  
    mainPanel(
        leafletOutput("mymap"),
        p(), 
        tableOutput("table")
  )
  
),


server = function(input, output, session) {

  
  # Create new table as the drop-downs are updated
  tab.family <- reactive({ 
    
    tib %>% 
      filter(familyCarni %in% input$family) 
    })
  
  tab.species <- reactive({ 
    
    tib %>% 
      filter(familyCarni %in% input$family) %>% 
      filter(scientificNameCarni %in% input$species) 
  })

  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = tib,
    vars = c("familyCarni", "scientificNameCarni", "country")
  )
  
    
  # # Drop-down for families
  # output$family <- renderUI({
  #           selectizeInput('family', 'Select family', choices = c("select" = "", levels(tib$familyCarni)))
  # })
  #   
  # # Drop-down for species
  # output$species <- renderUI({
  #   # Subset by the selected family
  #   choice_species <- reactive({
  #     tib %>% filter(familyCarni == input$family) %>% pull(scientificNameCarni) %>% as.character()
  #   })
  #           # Drop-down for carnivore in that family
  #           selectizeInput('species', 'Select species', choices = c("select" = "", choice_species()))
  # })
   
  
  # Create map to show all data
  output$mymap <- renderLeaflet ({
    leaflet() %>%
      addTiles() %>%
      setView(0,20, zoom = 1.5) %>% 
      addCircles(data = tib,
                 lng = ~decimalLongitude,
                 lat = ~decimalLatitude,
                 #popup = ~paste(familyCarni, scientificNameCarni, country, sep = "; "),
                 color = "lightgrey") %>%

      # addCircles(data = tab.family(),
      #            lng = ~decimalLongitude,
      #            lat = ~decimalLatitude,
      #            popup = ~paste(familyCarni, scientificNameCarni, country, sep = "_"),
      #            color = "Red") %>%
      # 
      # Update to show just  the species selected
      addCircles(data = res_mod(),
                 lng = ~decimalLongitude,
                 lat = ~decimalLatitude,
                 popup = ~paste(sep = "<br/>",
                                paste0("<B>", country ,"</B>"),
                                paste0("<B>", familyCarni,": </B>", "<I>", gsub("_", " ", scientificNameCarni), "</I>"),
                                paste0("Life stage: ", lifeStageCarni),
                                paste0("Sex: ", sexCarni)
                 ),
                 #popup = ~ paste(familyCarni, scientificNameCarni, country, sep = "; "),
                 color = "black",
                 weight = 5)
       # addPolygons(data = range(),
       #          color = "red",
       #          smoothFactor = 1,
       #          opacity = 0.5)
       # 
  })

  # Generate a table of the studies for the selected species
  output$table <- renderTable({
    
    res_mod()
    
  })
  
  # Allow users to download the data.
  output$download <- downloadHandler(
    
    filename = function() {"diet.csv"},
    content = function(fname) {
      write.csv(res_mod(), fname)
    }
    
  )
  
  
# This apparently can update the map

# observe({
#   leafletProxy("map", data = tab()) %>%
#     clearShapes() %>%
#     addCircleMarkers(lat = ~lat,
#                      lng = ~long,
#                      popup = ~CarniBinom)
# })

}

)

#shinyApp(ui = ui, server = server)

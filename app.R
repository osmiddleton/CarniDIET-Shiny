
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
               lattice, dplyr, shinyjs, DT)

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

# Change names of coordinates
tib <- as_tibble(studies)
tib$id <- 1:nrow(tib)


# CarniDIET summarised
cd_summary <- carnidiet %>% 
  dplyr::group_by(familyCarni, scientificNameCarni, lifeStageCarni, sexCarni,
                  foodType,scientificNamePrey,
                  country, samplingProtocol,methodQuantification, sourceTitle) %>% 
  dplyr::summarise(x = length(scientificNameCarni))

#cd_summary <- cd_summary[1:10,]

# Get colour scheme for families:
fams <- levels(factor(tib$familyCarni))
colour_scheme <- colorFactor(palette = "Paired", fams)

# CarniDIET Shiny App ----

# Requires:
# UI (User Interface): What the user will see
# Server: Code for how to build/rebuild R objects to display in the UI
# shinyApp: Combines the above into an App.





shinyApp(

  # User interface ----
  
  ui = fluidPage(
    
    # UI Theme
    theme = shinytheme("lumen"),
    
    # Title of app
    tags$head(HTML("<title> CarniDIET 1.0 </title>")),
    
    # Navigation bar: Add file name in before .png
    navbarPage(title = span(img(src = "logosmall.png", style = "margin-top: -14px", height = 65)),
               
               # 1. Welcome page ----
               tabPanel("Welcome",
                        
                        HTML('<center><img src="logo.png" width = "400" ></center>'),
                        
                        h1("Exploring the diets of the world's wild carnivores", align = "center"),
                        h1(" ", align = "center"),
                        
                        HTML('<center><img src="map.png" width = "600"></center>'),
                        
                        h4("The world's carnivores are of major interest for conservation research, creative inpiration, and, often, conflict. One aspect of their ecology that fascinates us is their diets, which can vary to certain degrees across their geographic ranges. For any interested party, CarniDIET aims to make current knowledge of their diets available by digitizing past quantitative studies of their diets for all to access.", align = "center"),
                        h4("CarniDIET is a work in progress, with the first release (CarniDIET 1.0) now available, consisting of data for 103 species of terrestrial, carnivorous mammal.", align = "center"),
                        h4(strong("We will continue to build the database and welcome input from all to continue to improve data accuracy and accessibility. See plans for future updates on the 'Details' page."), align = "center"),
                        tags$img(src = "carnivore-banner.png", align = "center", width = "100%"),
                        
                        h6("Left to right: stoat (Mustela erminea), black-backed jackal (Canis simensis), gey wolf (Canis lupus), cheetah (Acinonyx jubatus), lion (Panthera leo)"),
                        h6(strong("Photo credit: Dr. Christopher J Sandom"))
                        
               ),
               
               
               # 2. Exploratory data page ----
               tabPanel("Explore",
                        
                        #h1(strong("CarniDIET"), align = "center"),
                        #HTML('<center><img src="logo.png" width = "400" ></center>'),
                        
                        h1("Explore here", align = "center"),
                        h3("To begin exploring, click a study site on the map or filter for one, or more, taxonomic family, species, or country first using the side menu...", align = "center"),
                        
                        sidebarPanel(
                          #img(src = "logosmall.png"),
                          h4(strong("What are you interested in?")),
                          selectizeGroupUI(
                            id = "explore_filters",
                            inline = FALSE,
                            params = list(
                              family = list(inputId = "familyCarni", title = "What family?", placeholder = "select"),
                              species = list(inputId = "scientificNameCarni", title = "What species?", placeholder = "select"),
                              country = list(inputId = "country", title = "What country?", placeholder = "select")))
                          # uiOutput("family"),
                          # uiOutput("species"),
                          # uiOutput("country"),
                        ),

                        mainPanel(
                          
                          leafletOutput("mymap")
                          #tableOutput("table")
                        )
                        
                        
                        
                        
               ),
               
               
               # 3. Download page (more information etc) ----
               tabPanel("Download",
                        
                        #HTML('<center><img src="logo.png" width = "400" ></center>'),
                        
                        h1("Downloading data", align = "center"),
                        h3("Hit the 'Download' button on the left to get the full CarniDIET 1.0 dataset." , align = "center"),
                        h5("Filter menu: If you are only interested in a specific family, species, country, or anything else, user the sidebar menu filter first and then hit download." , align = "center"),
                        h5("Table output: The data shown in the table below is an overview of the data you will download. The downloaded data will include all data fields." , align = "center"),
                        h5(strong("Reference and acknolwedgements: Any use of these data should cite the manuscript on the Welcome page, although we acknowledge that the real credit goes to the hard work of field researchers who gathered these data."), align = "center"),
                        
                        fluidRow(column(width = 3,
                                        downloadButton("download", "Download the data"),
                                 selectizeGroupUI(
                                 id = "download_filters",
                                 inline = FALSE,
                                 params = list(
                                   family_download = list(inputId = "familyCarni", title = "Select family", placeholder = "select"),
                                   species_download = list(inputId = "scientificNameCarni", title = "Select species", placeholder = "select"),
                                   country_download = list(inputId = "country", title = "Select country", placeholder = "select"),
                                   source_download = list(inputId = "sourceTitle", title = "Select source", placeholder = "select"),
                                   foodType_download = list(inputId = "foodType", title = "Select food type", placeholder = "select"),
                                   samplingProtocol_download = list(inputId = "samplingProtocol", title = "Select sempling protocol", placeholder = "select"),
                                   methodQuantification_download = list(inputId = "methodQuantification", title = "Select method", placeholder = "select"),
                                   preySpecies_download = list(inputId = "scientificNamePrey", title = "Select prey species", placeholder = "select")
                                   ))),
                                 
                                 column(width = 6,
                                         dataTableOutput("table_download_display")
                                        )
                                 )
                        
                        # sidebarPanel(
                        #   #img(src = "logosmall.png"),
                        #   selectizeGroupUI(
                        #     id = "my-filters",
                        #     inline = FALSE,
                        #     params = list(
                        #       family = list(inputId = "familyCarni", title = "Select family", placeholder = "select"),
                        #       species = list(inputId = "scientificNameCarni", title = "Select species", placeholder = "select"),
                        #       country = list(inputId = "country", title = "Select country", placeholder = "select"))),
                        #   # uiOutput("family"),
                        #   # uiOutput("species"),
                        #   # uiOutput("country"),
                        #   downloadButton('download', "Download the data")
                        # ),
                        # 
                        # mainPanel(
                        #   #leafletOutput("mymap"),
                        #   #p(),
                        #   tableOutput("table")
                        # )
                        # 
                        ),
               
               
               
               
               # 4. Details page -----
               tabPanel("Details",
                        
                        h1(strong("CarniDIET"), align = "center"),
                        h1("Exploring the diets of the world's wild carnivores", align = "center"),
                        
                                 
                        )
    ),   

    
  ),  
  
  


server = function(input, output, session) {

  # 1. Explore page ----
  
  
  # Filter table following user input for plotting on map:
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "explore_filters",
    data = tib,
    vars = c("familyCarni", "scientificNameCarni", "country")
  )
  
    
  # Map of study locations, including any user filtering:
  output$mymap <- renderLeaflet ({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>% 
      addTiles(group = "Countries") %>%
      
      addLayersControl(
        position = "bottomright",
        baseGroups = c("Satellite", "Countries"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      
      setView(0,20, zoom = 1.5) %>% 
      addCircles(data = tib,
                 lng = ~decimalLongitude,
                 lat = ~decimalLatitude,
                 #popup = ~paste(familyCarni, scientificNameCarni, country, sep = "; "),
                 color = "lightgrey") %>%

      # Update to show just  the species selected
      addCircleMarkers(data = res_mod(),
                 lng = ~decimalLongitude,
                 lat = ~decimalLatitude,
                 color = ~colour_scheme(familyCarni),
                 radius = 1,
                 popup = ~paste(sep = "<br/>",
                                paste0("<B>", country ,"</B>"),
                                paste0("<B>", familyCarni,": </B>", "<I>", gsub("_", " ", scientificNameCarni), "</I>"),
                                paste0("Life stage: ", lifeStageCarni),
                                paste0("Sex: ", sexCarni)
                                
                 ),
                 layerId = ~id,
                 weight = 5
                 ) %>% 
      
      addLegend('bottomleft', pal = colour_scheme, values = fams,
                title = 'Family',
                opacity = 1)
  })

  
  # Filter CarniDIET to the study that has been clicked on:
  
  # Trying to create an observe click event to filter the dataframe
  observeEvent(input$Map_marker_click, { # update the map markers and view on map clicks
    
    p <- input$Map_marker_click
    proxy <- leafletProxy("Map")
    if(p$id=="Selected"){
      proxy %>% removeMarker(layerId="Selected")
    } else {
      proxy %>% setView(lng=p$lng, lat=p$lat, input$Map_zoom) %>% acm_defaults(p$lng, p$lat)
    }
  })
  
  # Create a figure to display the diet of that species in this location
  
  
  
  
  
  
  
  # Download page ----
  
  # The data displayed on this page is a subset of what would be downloaded.
  # It includes two sections of code:
  
  # 1) Filtering the subset of data for display
  # 2) Downloading the full dataset depending on user filters
  
  # Create user input filtering set up (for both display and main CarniDIET database filtering)
  res_mod_download_display <- callModule(
    module = selectizeGroupServer,
    id = "download_filters",
    data = cd_summary,
    vars = c("familyCarni",
             "scientificNameCarni",
             "country",
             "sourceTitle",
             "foodType",
             "samplingProtocol",
             "methodQuantification",
             "scientificNamePrey")
  )
  
  
  # Create table of simple information for users to view...
  output$table_download_display <- DT::renderDataTable({
    
    data <- res_mod_download_display()
    
    DT::datatable(data) %>% formatStyle("sourceTitle","white-space"="nowrap")
    
  })
  
  
  # Filter table for download:
  res_mod_download <- callModule(
    module = selectizeGroupServer,
    id = "download_filters",
    data = carnidiet,
    vars = c("familyCarni",
             "scientificNameCarni",
             "country",
             "sourceTitle",
             "foodType",
             "samplingProtocol",
             "methodQuantification",
             "scientificNamePrey")
  )
  
  
  
  # Allow users to download the data.
  output$download <- downloadHandler(
    
    filename = function() {"diet.csv"},
    content = function(fname) {
      write.csv(res_mod_download(), fname)
    }
    
  )
  
  

})

#shinyApp(ui = ui, server = server)

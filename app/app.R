## Luke Ozsanlav-Harris
## Created: 20/06/2022

## Script Aims:
  ## Read in GPS tracking data
  ## Plot the tracking data in a leaflet map
  ## Allow user to fit HMM model and visualize output


## To do on App:
# Make HMM sliders work so they change starting parameters- **DONE**
# Add in HMM model checks and state distributions- **DONE**
# create two different tabs, data visualization and then HMM- **DONE**
# Add ability to change the number of states in HMM- **DONE**
# Make a 2 state model work with Goose data
# Be able to select all animals or a selection, then color by individual
# Change max gap and shortest track in UI
# Add text boxes when hovering over HMM icons
# Stop all error messages when HMM hasn't been run
# Add sampling intervals to info boxes
# Add the option to read in data directly from Move bank
# What is the target audience? Does this effect how the app set up?
# Only runs HMM when go on to the HMM outputs page


## Packages required
library(momentuHMM)
library(adehabitatLT)
library(sf)
library(sp)
library(shiny) 
library(shinydashboard)
library(DT)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(car)

## Reed in the function I created for the HMM models
source("functions/HMM functions.R")


## read in data to check that code works without running the app
testtracks = read.csv("testdata/Two_elephants2.csv")
# testtracks = read.csv("app/testdata/Two_elephants2.csv")



#-----------------------------------#
#### 0.1 Set Custom Styles for UI ####
#-----------------------------------#

## This just allows my to access cutom CSS styles for certain things
## Might be worth putting then in a styles.css file if there are more 
STYLES <- list(
  help_button = "background-color:#E76F62; width:100%;",
  Render_button = "background-color:#4ED671; width:100%"
  # Info_text = "font-weight: bold; font-size: 17px; color: #FF3C3C"
  
)

#-----------------------------------#
#### 0.2 Set up functions for UI ####
#-----------------------------------#

## help module ui
## This function creates the help buttons, uses it a lot so made it into a function
## If this becomes long then it might be worth putting them in an R file that we then call in using source()
help_button <- function(module_id, style = STYLES$help_button, text = "Help") {
  prefix <- NS(module_id)
  actionButton(prefix("help"),
               text,
               icon = shiny::icon("question", verify_fa = FALSE),
               style = style)
}




#------------------------------------#
#### 1. Set up the User Interface ####
#------------------------------------#


#----------------------------#
#### 1.1 Set up UI header ####
#----------------------------#

## Set up the header
header <- dashboardHeader(title = "Animal Movement- Hidden Markov Models", titleWidth = 450)


#-----------------------------#
#### 1.2 Set up UI sidebar ####
#-----------------------------#

## Set up the side bar contents
sidebar <- dashboardSidebar(sidebarMenu(menuItem("App Intro", icon = icon("book"), tabName = "Intro"),
                                        menuItem("Step 1- Data Vis", icon = icon("globe"), tabName = "DataVis"),
                                        menuItem("Step 2- Segment Data", icon = icon("stream"), tabName = "SegData"),
                                        menuItem("Step 3- Run HMM Model", icon = icon("database"), tabName = "HMMOut"),
                                        menuItem("Step 4- HMM Model Checks", icon = icon("th"), tabName = "ModChecks")
                            ))


#--------------------------#
#### 1.3 Set up UI body ####
#--------------------------#

box

## Set up the dashboard contents 
body <- dashboardBody(
  tabItems(tabItem(tabName = "Intro",
                   fluidRow(box(width = 12, title = "App Overview", solidHeader = TRUE, status = "primary",
                                fluidRow(column(width = 10,includeMarkdown("help/p0.0 Introduction Tab.md")))))),
           
          tabItem(tabName = "DataVis", # Boxes need to be put in a row (or column)
                   fluidRow(box(selectInput("BirdSelect", label = "Select Animal ID", choices = c(unique(testtracks$individual.local.identifier))),
                                width = 3, title = "Select Individual", solidHeader = TRUE, status = "primary")),
                   fluidRow(box(width = 12, title = "Summary of Individual", solidHeader = FALSE, status = "primary",
                                infoBoxOutput("IndividualID"),infoBoxOutput("NoLocations"),infoBoxOutput("DateRange"))),
                   fluidRow(box(leafletOutput("leafletmap", height="750px"), width = 12, title = "Interactive map of locations", solidHeader = TRUE, status = "primary"))),
           
           tabItem(tabName = "HMMOut",
                   fluidRow(box(width = 12, title = "Parameter choices for HMM", solidHeader = TRUE, status = "primary",
                                fluidRow(column(width = 3, selectInput("NoStates", "Number of states for Hidden markov model", choices = c(3, 2))),
                                         column(width = 2, offset = 7, help_button("ParamHelp"))),
                                fluidRow(column(width = 4, sliderInput("StepMean1", "State 1- Mean Step Length (Km)", min = 0.01, max = 0.5, step =0.01, value = 0.02)),
                                         column(width = 4, sliderInput("StepSD1", "State 1- Standard Deviation Step Length (Km)", min = 0.01, max = 0.5, step =0.01, value = 0.02)),
                                         column(width = 4, sliderInput("AngleMean1", "State 1- Turning Angle Concentration", min = 0.01, max = 5, step =0.05, value = 0.01))),
                                fluidRow(column(width = 4, sliderInput("StepMean2", "State 2- Mean Step Length (Km)", min = 0.01, max = 0.5, step =0.01, value = 0.1)),
                                         column(width = 4, sliderInput("StepSD2", "State 2- Standard Deviation Step Length (Km)", min = 0.01, max = 0.5, step =0.01, value = 0.1)),
                                         column(width = 4, sliderInput("AngleMean2", "State 2- Turning Angle Concentration", min = 0.01, max = 5, step =0.05, value = 0.1))),
                                fluidRow(column(width = 4, sliderInput("StepMean3", "State 3- Mean Step Length (Km)", min = 0.01, max = 0.5, step =0.01, value = 0.3)),
                                         column(width = 4, sliderInput("StepSD3", "State 3- Standard Deviation Step Length (Km)", min = 0.01, max = 0.5, step =0.01, value = 0.3)),
                                         column(width = 4, sliderInput("AngleMean3", "State 3- Turning Angle Concentration", min = 0.01, max = 5, step =0.05, value = 3))),
                                fluidRow(column(width = 2, offset = 10, actionButton("HMMFitBut", "Fit HMM", style = STYLES$Render_button))))),
                   fluidRow(box(leafletOutput("HMM_Decodedmap"), width = 12, title = "Interactive map of HMM states (Run HMM to visualise States)", 
                                                  solidHeader = TRUE, status = "success")),
                   fluidRow(box(plotOutput("HMM_SLdist"), width = 6, title = "Step length distributions by state", solidHeader = TRUE, status = "primary"),
                            box(plotOutput("HMM_Anglesdist"), width = 6, title = "Turning angle distributions by state", solidHeader = TRUE, status = "primary")),
                   fluidRow(column(width = 8, offset = 2, box(plotOutput("HMM_simpdecoded"), width = 12, title = "Simple plot of HMM states", solidHeader = TRUE, status = "primary")))),
           
           tabItem(tabName = "ModChecks",
                   fluidRow(box(plotOutput("HMM_ModChecks"), width = 12, title = "HMM model checks (Run HMM to visualise plots)", solidHeader = TRUE, status = "danger"))))
  
)


#-------------------------#
#### 1.4 Create the UI ####
#-------------------------#

## Create the dashboard page 
ui <- dashboardPage(header, sidebar, body, skin = "green")




#----------------------------------#
#### 2. Rendering the shiny app ####
#----------------------------------#

## Render the shiny app
server <- function(input, output, session) {
  

  ##*OLD code from when users could read in their own data*##
  ##
  #### 2.1 Read in the csv file the user selects ####
  ##
  
  ## This reads in my csv file, the UI allows me to initially just find the path for my file
  # data <- reactive({
  #   
  #   req(input$filedata)
  #   read.csv(input$filedata$datapath)
  #   
  # })
  ##
  #### 2.2 Update the animal IDs the user can select ####
  ##
  
  ## This updates the select Input drop down options based off of the file that I read in
  # observe({
  #   
  #   x <- data()
  #   
  #   # I need to identify which select input i want to update and then tell it what the new choices should be
  #   updateSelectInput(session, "BirdSelect",
  #                     choices = unique(x$individual.local.identifier)
  #   )
  # })
  
  #-------------------------#
  #### 2.1 Server Modules ####
  #-------------------------#
  
  ## get the working directory, this is need to read in the help files
  APP_wd <- getwd()
  
  # help module server ----
  # need app folder so was placed here, otherwise need to add parameter
  click_help <- function(input, output, session, title, size, file){
    observeEvent(input$help, {
      showModal(modalDialog(
        title = title, size = size,
        fluidPage(includeMarkdown(file.path(APP_wd, file))),
        easyClose = TRUE, fade = FALSE
      ))
    })
  }
  
  
  #-----------------------#
  #### 2.2 Help Buttons ####
  #-----------------------#
  
  ## Help button when choosing starting parameters for HMMs
  callModule(click_help, "ParamHelp", title = "Help page",
             size = "l", file = "help/p3 Starting Params.md")

  
  
  
  #-----------------------------------------------------#
  #### 2.3 Create info boxes that summaries GPS data ####
  #-----------------------------------------------------#
  
  ## Create an Info box with the ID of the individual
  output$IndividualID <- renderInfoBox({
    infoBox(
      "Individual ID", paste0(input$BirdSelect), color = "purple", icon = shiny::icon("piggy-bank")
    )
  })

  ## Create an Info box with the the number of GPS locations
  output$NoLocations <- renderInfoBox({
    
    # data <- data()
    data <- as.data.frame(testtracks)
    ## filter out the individuals i want based of the drop down selected 
    data_sub <- dplyr::filter(data, data$individual.local.identifier == input$BirdSelect)
    
    infoBox(
      "Number of Locations", nrow(data_sub), color = "blue", icon = shiny::icon("globe")
    )
  })
  
  ## Create an Info box with the date rnage of the GP\S data for that individual
  output$DateRange <- renderInfoBox({
    
    # data <- data()
    data <- as.data.frame(testtracks)
    ## filter out the individuals i want based of the drop down selected 
    data_sub <- dplyr::filter(data, data$individual.local.identifier == input$BirdSelect)
    data_sub$timestamp <- lubridate::ymd_hms(data_sub$timestamp)
    
    infoBox(
      "Date Range", paste0(min(data_sub$timestamp), " - ", max(data_sub$timestamp)), color = "teal", icon = shiny::icon("calendar")
    )
  })
  

  
  
  #------------------------------------#
  #### 2.4 Create basic leaflet map ####
  #------------------------------------#
  
  ## now render the leaflet map
  output$leafletmap <- renderLeaflet({
    
    ## filter the data for the individual the user has selected
    data <- as.data.frame(testtracks)
    data_sub <- dplyr::filter(data, data$individual.local.identifier == input$BirdSelect)
    
    ## Filter any NAs in the speed column as this will break the map if the NAs are kept
    data_sub <- filter(data_sub, is.na(speed)==F)
    
    ## create a vector of all the values going to be used to colour the points
    colour_column <- select(data_sub, speed)
    colour_column <- as.numeric(colour_column$speed)
    
    ## make locations an sf object
    sf_locs_inc <- st_as_sf(data_sub, coords = c("location.long","location.lat")) %>% 
      st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84")
    
    ## create the lines between locations (as an sf object)
    sf_lines_inc <- sf_locs_inc %>%
      st_geometry() %>% 
      st_cast("MULTIPOINT",ids = as.integer(as.factor(sf_locs_inc$individual.local.identifier))) %>% 
      st_cast("MULTILINESTRING") %>% 
      st_sf(tripNo.subsampled = as.factor(unique(sf_locs_inc$individual.local.identifier)))
    
    ## create the colour palette
    pal <- colorNumeric(palette = "viridis", domain = colour_column)
    
    ## leaflet map with the default base map and the locations
    leaflet(data_sub) %>% 
          
      ##adding various tiles and map backdrops - here set up to flick between them interactively
      addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google') %>% 
      addProviderTiles("Esri.WorldImagery", group = "ESRI") %>%
      addTiles(group = "OSM(default)") %>% 
      addLayersControl(baseGroup = c("OSM(default)", "ESRI", "GoogleEarth")) %>%
      
      ##add the track as polylines 
      addPolylines(data = sf_lines_inc, weight = 2, opacity = 0.7, color = "grey") %>% 
      
      ## add locations
      addCircleMarkers(data = sf_locs_inc, radius = 3, weight = 2, opacity = 0.8, fillOpacity = 0.8, color = ~pal(colour_column)) %>% 
      
      ## add a legend
      addLegend(position = "bottomleft", pal = pal, values = colour_column, title = "Speed (m/s)", opacity = 1) %>% 
      ## add a scale bar
      addScaleBar(position = "bottomright")
    
  })
  
  
  
  
  #---------------------------------------#
  #### 2.5 Run the hidden markov model ####
  #---------------------------------------#
  
  ## this runs the hidden markov model with all the right starting parameters from the input sliders
  ## The output is the model object and the locations with the decoded states
  ## The main bit of code inside the function is only run if the HMMFitBut is pressed
  HMMOutput <- eventReactive(input$HMMFitBut, {
    
    data <- as.data.frame(testtracks)
    data_sub <- dplyr::filter(data, data$individual.local.identifier == input$BirdSelect)
    
    ## Show notification while the data is sub-sampled
    ModelNotif <- showNotification(
                  shiny::span(shiny:::icon("hourglass-start"), "Running Model.."),
                  type = "message", duration = NULL)
    
    ## run the hidden markov model
    Model_out <- HMMforShiny(data=data_sub, SL_mean1=input$StepMean1, SL_sd1=input$StepSD1, SL_mean2=input$StepMean2, SL_sd2=input$StepSD2,
                SL_mean3=input$StepMean3, SL_sd3=input$StepSD3, Angle_mean1=input$AngleMean1, Angle_mean2=input$AngleMean2,
                Angle_mean3=input$AngleMean3, No_States=3)
    
    ## remove the notification
    removeNotification(ModelNotif)
    
    ## returnj the model output to the reactive value
    return(Model_out)
    
    
  })
  

  
  
  #--------------------------------------------------#
  #### 2.6 Create leaflet map with states colored ####
  #--------------------------------------------------#
  
  ## now render the output of the HMM model in a leaflet map, the GPS points are colored by the HMM states
  output$HMM_Decodedmap <- renderLeaflet({

    data <- as.data.frame(testtracks)
    data_sub <- dplyr::filter(data, data$individual.local.identifier == input$BirdSelect)

      
      ## extract the decoded states and lat/longs from the list
      HMMMod <- HMMOutput()
      HMMMod <- HMMMod[["Decoded"]]
      
      # HMMMod <- filter(HMMMod, is.na(long)==F)
      
      ## create a color palette for plotting
      myColors <- brewer.pal(3,"Dark2") # create colour palette
      factpal <- colorFactor(myColors, HMMMod$state_3st)
      
      # ## make locations an sf object
      sf_locs_inc2 <- st_as_sf(HMMMod, coords = c("long","lat"), na.fail = F) %>%
        st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84")
      # 
      # # ## create the lines between locations (as an sf object)
      # sf_lines_inc2 <- sf_locs_inc2 %>%
      #   st_geometry() %>%
      #   st_cast("MULTIPOINT",ids = as.integer(as.factor(sf_locs_inc2$ID))) %>%
      #   st_cast("MULTILINESTRING") %>%
      #   st_sf(tripNo.subsampled = as.factor(unique(sf_locs_inc2$ID)))

      ## Plot the Lat/long and color by the different states from the HMM
      leaf <-  leaflet(HMMMod) %>%
        
                ##adding various tiles and map backdrops - here set up to flick between them interactively
                addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google') %>% 
                addProviderTiles("Esri.WorldImagery", group = "ESRI") %>%
                addTiles(group = "OSM(default)") %>% 
                addLayersControl(baseGroup = c("OSM(default)", "ESRI", "GoogleEarth")) %>%
                
                ## add the points coloured by state
                addCircleMarkers(data = sf_locs_inc2, color = ~factpal(state_3st),
                                 fillOpacity = 2, stroke = FALSE, radius = 3) %>%
        
                # ##add the track as polylines 
                # addPolylines(data = sf_lines_inc2, weight = 2, color = "grey") %>% 
        
                addLegend("topleft", pal = factpal, values = ~state_3st,
                          title = "HMM States",
                          labFormat = labelFormat(prefix = ""),
                          opacity = 1)


    
    ## Now plot which ever of the two plots was created above
    leaf


  })
  
  
  
  
  #------------------------------------------------------------------#
  #### 2.7 Plot the state distributions for step length and angle ####
  #------------------------------------------------------------------#
  
  ## This extracts the HMM model if running a HMM is selected and then plots the distributions of the states
  output$HMM_SLdist <- renderPlot({
    
    
      ## extract the model from the list
      HMMMod <- HMMOutput()
      HMMMod <- HMMMod[["Decoded"]]
      
      Steps <- dplyr::filter(HMMMod, is.na(step)==F)
      
      SLP<- ggplot(Steps, aes(x=step)) +
            geom_density(aes(group=state_3st, colour=state_3st), size = 1.5) +
            xlab("Step Length (Km)") + ylab("Density") +
            theme_classic() +
            labs(colour="State") +
            scale_colour_manual(values=c("#1B9E77","#D95F02","#7570B3"))+
            theme(panel.grid.minor.y = element_blank(),
                  axis.title=element_text(size=12), 
                  panel.grid.minor.x = element_blank(), 
                  #legend.position = "none",
                  panel.grid.major.x = element_blank(), 
                  axis.text = element_text(size =14), 
                  axis.title.x = element_text(size =18),
                  axis.title.y = element_text(size =18), 
                  strip.text.x = element_text(size =14), 
                  legend.title = element_text(size =14),
                  legend.text = element_text(size =14))

      
    
    
    ## plot the partial residuals from the model
    SLP
    
  })
  
  ## This extracts the HMM model if running a HMM is selected and then plots the distributions of the states
  output$HMM_Anglesdist <- renderPlot({
    
    
      
      ## extract the model from the list
      HMMMod <- HMMOutput()
      HMMMod <- HMMMod[["Decoded"]]
      
      Steps <- dplyr::filter(HMMMod, is.na(angle)==F)
      
      ALP<- ggplot(Steps, aes(x=angle)) +
            geom_density(aes(group=state_3st, colour=state_3st), size = 1.5) +
            xlab("Turning Angle (Radians)") + ylab("Density") +
            theme_classic() +
            labs(colour="State") +
            scale_colour_manual(values=c("#1B9E77","#D95F02","#7570B3"))+
            theme(panel.grid.minor.y = element_blank(),
                  axis.title=element_text(size=12), 
                  panel.grid.minor.x = element_blank(), 
                  #legend.position = "none",
                  panel.grid.major.x = element_blank(), 
                  axis.text = element_text(size =14), 
                  axis.title.x = element_text(size =18),
                  axis.title.y = element_text(size =18), 
                  strip.text.x = element_text(size =14), 
                  legend.title = element_text(size =14),
                  legend.text = element_text(size =14))
      
      
    
    
    ## plot the partial residuals from the model
    ALP
    
  })
  
  ## This extracts the HMM model if running a HMM is selected and then plots the distributions of the states
  output$HMM_simpdecoded <- renderPlot({
    
   
      
      ## extract the model from the list
      HMMMod <- HMMOutput()
      States <- HMMMod[["model"]]
    
   
    
    plot(States, breaks = 25, ask = FALSE)
    
  })
  
  
  
  
  #---------------------------------------------------#
  #### 2.8 Plot the partial residuals from the HMM ####
  #---------------------------------------------------#
  
  ## This extracts the HMM model if running a HMM is selected and then it plots the partial residuals from the model
  output$HMM_ModChecks <- renderPlot({
    
    
      ## extract the model from the list
      Model <- HMMOutput()
      states <- Model[["model"]]
      
   
    
    ## plot the partial residuals from the model
    plotPR(states)
    
  })


  
}# closes server function




#-------------------------------#
#### 3. Create the shiny app ####
#-------------------------------#

shinyApp(ui, server)




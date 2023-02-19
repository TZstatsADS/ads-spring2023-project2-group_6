library(dplyr)
library(data.table)
library(leaflet)
library(shiny)
library(shinydashboard)
library(dygraphs)
library(xts)


# read data
data <- as_tibble(fread('MTA_dataset.csv',header = T))
# modify data
data <- data %>%
  mutate(incident_date = as.Date(data$week_start, format = "%m/%d/%Y"))


server <- function(input, output) {
  filteredData <- reactive({
    data %>%
      filter(fare_type %in% input$fare_type ) %>%
      filter(week_start > input$dates[1] & week_start < input$dates[2]) 
  })
  
  output$subwayMap <- renderLeaflet({
    leaflet(filteredData()) %>%
      addTiles(group = 'OSM') %>%
      addProviderTiles('Esri.WorldStreetMap', group = 'Esri') %>%
      addProviderTiles('CartoDB.Positron', group = 'CartoDB') %>%
      addMarkers(
        ~long, ~lat, popup = ~fares, 
        clusterOptions = markerClusterOptions()
      ) %>%
      addLayersControl(
        baseGroups = c('OSM', 'Esri', 'CartoDB'),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
 
}
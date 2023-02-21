library(dplyr)
library(data.table)
library(leaflet)
library(shiny)
library(shinydashboard)
library(dygraphs)
library(xts)
library(rgdal)
require(rgdal)

# read data
data <- as_tibble(fread('MTA_dataset.csv', header = T))
# modify data
data <- data %>%
  mutate(dates = as.Date(data$week_start, format = "%m/%d/%Y"))
data <- data %>%
  select(lat, long, fares, dates) %>%
  group_by(lat, long, dates) %>% 
  summarise(fares= sum(fares))


server <- function(input, output) {
  filteredData <- reactive({
    data %>%
      filter(fare_type %in% input$type ) %>%
      filter(dates > input$dates[1] & dates < input$dates[2]) 
  })
#setDT(filteredData)
 #filteredData <- filteredData () %>% select (lat, long, sum_fares) %>% group_by(lat,long) %>% summarize(Fares = sum(sum_fares)) %>%
  
  subway_map <- readOGR(dsn = "/Users/denisesonia/ads-spring2023-project2-group_6/data/geo_export_1b991b55-24f5-4e63-9418-b365b941218f.shp")
  
 output$subwayMap <- renderLeaflet({
   
   
   
   leaflet(data=filteredata<-data#%>%filter(fare_type %in% input$fare_type )
           %>%filter(dates > input$dates[1] & dates < input$dates[2])%>%select(long,lat,fares)
           %>%group_by(long,lat)
           %>%summarise(fares=sum(fares))%>%mutate(sum_fares=sum(fares)) %>% distinct) %>%
          
     addPolylines(data = subway_map, color = "blue", weight = 2) %>%
     addTiles(group = 'OSM') %>%
     addProviderTiles('Esri.WorldStreetMap', group = 'Esri') %>%
     addProviderTiles('CartoDB.Positron', group = 'CartoDB') %>%
     addMarkers(
       ~long, ~lat, label= ~sum_fares, 
       clusterOptions = markerClusterOptions(),
       labelOptions = labelOptions(noHide = TRUE)
       
     ) %>%
     addLayersControl(
       baseGroups = c('OSM', 'Esri', 'CartoDB'),
       options = layersControlOptions(collapsed = FALSE)
     )
   
   
   
 })
}

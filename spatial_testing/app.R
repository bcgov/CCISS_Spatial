

library(shiny)
library(leaflet)
library(dplyr)
library(data.table)
library(htmltools)
library(htmlwidgets)
library(shinyalert)
library(shinyWidgets)
library(RSQLite)
library(terra)
library(plotly)
source("./JS_Source.R")
db <- dbConnect(RSQLite::SQLite(), "cciss_db.sqlite")

onStop(function(){dbDisconnect(db)})
t_rast <- rast("Raster_Templated.tif")

mbtk="pk.eyJ1Ijoid2htYWNrZW4iLCJhIjoiY2twaDVkNXU5MmJieTJybGE3cWRtY3Q4aCJ9.ISBkzSHFfrr78AVP2y2FeQ"
mblbsty = "whmacken/ckph5q6d21q1318nz4shnyp20"
mbsty="whmacken/ckph5e7y01fhr17qk5nhnpo10"

gcms <- c("ACCESS-ESM1-5","EC-Earth3","GISS-E2-1-G","MIROC6","MPI-ESM1-2-HR")
periods <- c("2001_2020", "2021_2040", "2041_2060", "2061_2080", "2081_2100")
base_tileserver <- "http://tileserver.thebeczone.ca/data/bgc_GCM_PERIOD/{z}/{x}/{y}.webp"
species_tileserver <- "http://tileserver.thebeczone.ca/data/STAT_PERIOD_EDATOPE_SPECIES/{z}/{x}/{y}.webp"
# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(id = "tabs",
              
              tabPanel("Map Testing",
                       sidebarLayout(
                         sidebarPanel(
                           radioButtons("type","Display BGC or Feasibility", choices = c("BGC","Feasibility"), selected = "Feasibility"),
                           h1("BGC Options"),
                           selectInput("gcm_select","Select GCM", choices = gcms, selected = gcms[1]),
                           selectInput("period_select","Select Period", choices = periods, selected = periods[1]),
                           h1("Feasibility Options"),
                           selectInput("map_stat","Select Map Type", choices = list("Projected Feasibility" = "NewFeas",
                                                                                    "Feasibility Change" = "MeanChange",
                                                                                    "Add/Retreat" = "AddRet"), multiple = FALSE),
                           selectInput("period_feas","Select Period", choices = periods[-1]),
                           selectInput("edatope_feas","Select Edatope", choices = c("B2","C4","E6"), multiple = FALSE),
                           selectInput("species_feas", "Select Species", choices = c("Pl","Sx","Fd","Cw","Hw","Bl"), multiple = FALSE)
                         ),
                         mainPanel(
                           leafletOutput("map", height = "90vh")
                         )
                       )
                       )
              )
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -122.77222, lat = 51.2665, zoom = 6) %>%
      leaflet::addTiles(
        urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mbsty, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
        attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
        group = "Hillshade",
        options = leaflet::pathOptions(pane = "mapPane")) %>%
      leaflet::addTiles(
        urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mblbsty, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
        attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
        group = "Cities",
        options = leaflet::pathOptions(pane = "overlayPane")) %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite",
                                options = leaflet::pathOptions(pane = "mapPane")) %>%
      addPlugin() %>%
      addBGCTiles()
  })
  
  observe({
    if(input$type == "BGC"){
      tile_url <- gsub("GCM", input$gcm_select, base_tileserver)
      tile_url <- gsub("PERIOD", input$period_select, tile_url)
    }else{
      tile_url <- gsub("STAT", input$map_stat, species_tileserver)
      tile_url <- gsub("PERIOD", input$period_feas, tile_url)
      tile_url <- gsub("EDATOPE", input$edatope_feas, tile_url)
      tile_url <- gsub("SPECIES", input$species_feas, tile_url)
      #cat(tile_url)
    }
    
    
    leafletProxy("map", session) %>%
      removeTiles(layerId = "Working") %>%
      addTiles(urlTemplate = tile_url,
               group = "BGC_Preds",
               layerId = "Working",
               options = tileOptions(maxZoom = 15,maxNativeZoom = 9)) %>%
      addLayersControl(
        baseGroups = c("Hillshade","Satellite","BGCs"),
        overlayGroups = c("BGC_Preds"),
        position = "topright")
  })
  
  observeEvent(input$map_click,{
    lat <- input$map_click$lat
    lng <- input$map_click$lng
    
    cell_click <- cellFromXY(t_rast, cbind(lng,lat))
    fp <- substr(input$period_feas,1,4)
    qry <- paste0("select * from bgc_preds where fp_code = ", fp, " and cellid = ",cell_click)
    #cat(qry)
    dat <- dbGetQuery(db, qry)
    
    output$bgc_plot <- renderPlotly({
      fig <- plot_ly(dat, labels = ~bgc_pred, values = ~bgc_prop, type = "pie")
      fig <- fig %>% layout(title = 'BGC Projections for Location',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      fig
    })
    
    showModal(modalDialog(
      title = "Click Values",
      plotlyOutput("bgc_plot"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # observeEvent(input$map_click,{
  #   lat <- input$map_click$lat
  #   lng <- input$map_click$lng
  #   
  #   cell_click <- cellFromXY(t_rast, cbind(lng,lat))
  #   fp <- substr(input$period_feas,1,4)
  #   eda <- switch(input$edatope_feas,
  #     "B2" = 1,
  #     "C4" = 2,
  #     "E6" = 3
  #   )
  #   qry <- paste0("select * from cciss_feas where fp_code = ", fp, " and edatope = ", 
  #                 eda," and species = '", input$species_feas, 
  #                 "' and cellid = ",cell_click)
  #   #cat(qry)
  #   dat <- dbGetQuery(db, qry)
  #   
  #   output$infoTable <- renderTable(dat)
  #   
  #   showModal(modalDialog(
  #     title = "Click Values",
  #     tableOutput("infoTable"),
  #     easyClose = TRUE,
  #     footer = NULL
  #   ))
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)

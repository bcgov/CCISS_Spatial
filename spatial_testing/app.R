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
library(ccissr)
library(RPostgres)
library(pool)
library(stinepack)
library(ggplot2)
library(ggrepel)
library(ggiraph)
library(shinyjs)

source("./JS_Source.R")
source("./plot_functions.R", local = TRUE)
dist_bnds <- fread("./district_bounds.csv")
flp_bnds <- fread("./flp_bounds.csv")
dist_bnds <- rbind(dist_bnds,flp_bnds)
db <- dbConnect(RSQLite::SQLite(), "cciss_db.sqlite") #"/mnt/spatcciss/cciss_db.sqlite"

dbCon <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "cciss_spatial",
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)
onStop(function(){
  dbDisconnect(db)
  poolClose(dbCon)
})
t_rast <- rast("Raster_Templated.tif")

mbtk="pk.eyJ1Ijoid2htYWNrZW4iLCJhIjoiY2twaDVkNXU5MmJieTJybGE3cWRtY3Q4aCJ9.ISBkzSHFfrr78AVP2y2FeQ"
mblbsty = "whmacken/ckph5q6d21q1318nz4shnyp20"
mbsty="whmacken/ckph5e7y01fhr17qk5nhnpo10"

gcms <- c("SZ_Ensemble", "Zone_Ensemble", "ACCESS-ESM1-5","EC-Earth3","GISS-E2-1-G","MIROC6","MPI-ESM1-2-HR")
periods <- c("2001_2020", "2021_2040", "2041_2060", "2061_2080","2081_2100")
base_tileserver <- "https://tileserver.thebeczone.ca/data/bgc_GCM_PERIOD/{z}/{x}/{y}.webp"
novelty_tileserver <- "https://tileserver.thebeczone.ca/data/Novelty_GCM_PERIOD/{z}/{x}/{y}.webp"
species_tileserver <- "https://tileserver.thebeczone.ca/data/STAT_PERIOD_EDATOPE_SPECIES/{z}/{x}/{y}.webp"

colour_ref <- subzones_colours_ref$colour
names(colour_ref) <- subzones_colours_ref$classification

subzones <- sort(subzones_colours_ref$classification)
zones <- sort(zone_colours$classification)

gcms_use <- c("ACCESS-ESM1-5","EC-Earth3","GISS-E2-1-G","MIROC6","MPI-ESM1-2-HR","MRI-ESM2-0")
runs_use <- c("r1i1p1f1","r4i1p1f1","r2i1p3f1","r2i1p1f1","r1i1p1f1","r1i1p1f1")

gcm_run <- data.table(gcm = c("obs", "ACCESS-ESM1-5","EC-Earth3","GISS-E2-1-G","MIROC6","MPI-ESM1-2-HR","MRI-ESM2-0"),
                      run = c(NA,"r1i1p1f1","r4i1p1f1","r2i1p3f1","r2i1p1f1","r1i1p1f1","r1i1p1f1"),
                      keep = TRUE)

ui <- fluidPage(
  useShinyjs(),
  navbarPage(id = "tabs",
             title = HTML('&nbsp;&nbsp;<img src="logo.svg" class="navbar-logo">'), ##navhelplink("The CCISS Tool", "cciss_about_nav")
             theme = {
               theme <- bslib::bs_theme(version = 5,
                                        bootswatch = "sandstone",
                                        primary = "#003366")
               # theme$layers$bootswatch$defaults[[3]][[2]] <-
               #   "$navbar-default-bg: primary !default;"
               theme
             },
             collapsible = TRUE,
             windowTitle = "Spatial CCISS",
              tabPanel("Provincial Map",
                       tags$head(includeCSS("./www/style.css")),
                       tags$head(includeScript("./www/cciss.js")),
                       
                       sidebarLayout(
                         sidebarPanel(
                           radioButtons("region_type","Subregion Type", choices = c("None", "District","FLP Area")),
                           radioButtons("type","Display BGC or Feasibility", choices = c("BGC","Feasibility"), selected = "BGC"),
                           radioButtons("period_type","Choose a Time Period", choices = list(
                                                                                             "Reference (1961-1990)" = "Historic", 
                                                                                             "Observed (2001-2020)" = "obs",
                                                                                             "Future (GCMs)" = "Future")),
                           conditionalPanel(
                             condition = "input.type == 'BGC' & input.period_type == 'Future'",
                             h1("GCM Options"),
                             selectInput("gcm_select","Select GCM", choices = gcms, selected = gcms[1]),
                             selectInput("period_select","Select Period", choices = periods, selected = periods[1])        
                           ),
                           
                           conditionalPanel(
                             condition = "input.type !== 'BGC'",
                             h1("Feasibility Options"),
                             selectInput("edatope_feas","Select Edatope", choices = c("B2","C4","E6"), multiple = FALSE),
                             selectInput("species_feas", "Select Species", choices = c("Pl","Sx","Fd","Cw","Hw","Bl","At", "Ac", "Ep", "Yc", "Pw", "Ss", "Bg", "Lw", "Sb"), multiple = FALSE)
                           ),
                           
                           conditionalPanel(
                             condition = "input.type !== 'BGC' & input.period_type !== 'Historic'",
                             selectInput("map_stat","Select Map Type", choices = list("Projected Feasibility" = "NewFeas",
                                                                                      "Feasibility Change" = "MeanChange"), multiple = FALSE)     
                           ),
                           
                           conditionalPanel(
                             condition = "input.type !== 'BGC' & input.period_type == 'Future'",
                             selectInput("period_feas","Select Period", choices = c(periods[-5])),     
                           ),
                           conditionalPanel(
                             condition = "input.period_type != 'Historic'",
                             checkboxInput("novelty","Display Novelty?", value = FALSE),
                           ),
                           actionButton("clear_map","Hide/Show Layer"),
                           tags$head(tags$style(".modal-body{ min-height:70vh}")),
                           width = 2
                         ),
                         mainPanel(
                           width = 10,
                           tags$head(
                             tags$style(HTML("
                              #map-container {
                                width: 100%;
                                height: 100vh;
                                transition: width 0.5s ease-in-out;
                              }
                              .half-map {
                                width: 60% !important;
                                float: left;
                              }
                              #plot-container {
                                width: 40%;
                                float: right;
                              }
                            "))
                           ),
                           # Map container
                           div(id = "map-container",
                               leafletOutput("map", width = "100%", height = "100vh")
                           ),
                           
                           # Plot container (initially hidden)
                           hidden(
                             div(id = "plot-container",
                                 selectInput("xvariable","X-Axis Variable", choices = c("Time","MAT","MAP","CMD","DD5")),
                                 checkboxInput("zone_sz","Summarise by Zone?",value = TRUE),
                                 actionButton("reset_district","Clear Selected District"),
                                 girafeOutput("summary_plot")
                             )
                           )
                         )
                       )
              ),
              # tabPanel("Summary by Region",
              #          fluidRow(
              #            column(2,
              #                   radioButtons("region_type","Subregion Type", choices = c("None", "District","FLP Area")),
              #                   radioButtons("type_2","Display BGC or Feasibility", choices = c("BGC","Feasibility"), selected = "BGC"),
              #                   uiOutput("ui_select_2"),
              #                   actionButton("clear_map_2","Toggle CCISS"),
              #                   actionButton("reset_district","Clear Selected District"),
              #                   actionButton("action_download","Download Data"),
              #                   tags$head(tags$style(".modal-body{ min-height:70vh}"))
              #                   ),
              #            column(5,
              #                   leafletOutput("map_2", height = "90vh")),
              #            column(5,
              #                   selectInput("xvariable","X-Axis Variable", choices = c("Time","MAT","MAP","CMD","DD5")),
              #                   checkboxInput("zone_sz","Summarise by Zone?",value = TRUE),
              #                   girafeOutput("summary_plot")
              #                   )
              #          )
              #          ),
             tabPanel(value = "tab6", title = "Find-a-BEC",
                      fluidRow(
                        column(2,
                               selectInput("selectBGC","Select Zone", 
                                           choices = c("(N)",zones), 
                                           multiple = F,selected = "(N)"),
                               selectInput("selectSubzone","Select subzone(s)",
                                           choices = "",multiple = T
                               ),
                               span(textOutput("selectedBEC", inline = T),style= "font-size:24px")
                        ),
                        column(10,
                               leafletOutput("findBGCMap", height = "80vh")
                        )
                      )
                      
             )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  dist_nm <- reactiveVal()
  globalLeg <- reactiveValues(Legend = NULL)
  
  
  # period_type <- reactiveVal(value = "Historic")
  # 
  # observeEvent(input$period_type,{
  #   period_type(input$period_type)
  # })
  
  
  
  # output$ui_select <- renderUI({
  #   if(input$type == "BGC"){
  #     if(input$period_type == "Future"){
  #       tagList(
  #         h1("GCM Options"),
  #         selectInput("gcm_select","Select GCM", choices = gcms, selected = gcms[1]),
  #         selectInput("period_select","Select Period", choices = periods, selected = periods[1])        
  #         )
  #     }
  #     
  #   }else{
  #     if(input$period_type %in% c("Historic")) {
  #       tagList(
  #         h1("Historic Feasibility"),
  #         selectInput("edatope_feas","Select Edatope", choices = c("B2","C4","E6"), multiple = FALSE),
  #         selectInput("species_feas", "Select Species", choices = c("Pl","Sx","Fd","Cw","Hw","Bl","At", "Ac", "Ep", "Yc", "Pw", "Ss", "Bg", "Lw", "Sb"), multiple = FALSE)
  #         
  #       )
  #     } else if(input$period_type == "obs") {
  #       tagList(
  #         h1("Current Projected Feasibility"),
  #         selectInput("map_stat","Select Map Type", choices = list("Projected Feasibility" = "NewFeas",
  #                                                                  "Feasibility Change" = "MeanChange"), multiple = FALSE), #"Add/Retreat" = "AddRet"
  #         selectInput("edatope_feas","Select Edatope", choices = c("B2","C4","E6"), multiple = FALSE),
  #         selectInput("species_feas", "Select Species", choices = c("Pl","Sx","Fd","Cw","Hw","Bl","At", "Ac", "Ep", "Yc", "Pw", "Ss", "Bg", "Lw", "Sb"), multiple = FALSE)
  #         
  #       )
  #     } else if(input$period_type == "Future") {
  #       tagList(
  #         h1("Future Projected Feasibility"),
  #         selectInput("map_stat","Select Map Type", choices = list("Projected Feasibility" = "NewFeas",
  #                                                                  "Feasibility Change" = "MeanChange"), multiple = FALSE),
  #         selectInput("period_feas","Select Period", choices = c(periods[-5])),
  #         selectInput("edatope_feas","Select Edatope", choices = c("B2","C4","E6"), multiple = FALSE),
  #         selectInput("species_feas", "Select Species", choices = c("Pl","Sx","Fd","Cw","Hw","Bl","At", "Ac", "Ep", "Yc", "Pw", "Ss", "Bg", "Lw", "Sb"), multiple = FALSE)
  #         
  #       )
  #     }
  #   }
  # })
  
  observeEvent(input$clear_map,{
    if(input$clear_map %% 2 != 0){
      session$sendCustomMessage("clear_tiles","Luna")
      session$sendCustomMessage("remove_novelty","Luna")
    }else{
      session$sendCustomMessage("unclear_tiles","Luna")
      if(input$novelty){
        session$sendCustomMessage("unclear_novelty","Luna")
      }
    }
    
  })
  
  observeEvent({c(input$novelty, input$period_feas, input$gcm_select, input$period_select)},{
    if(input$novelty & input$period_type != "Historic"){
      if(input$period_type == "obs"){
        pnm <- "Obs"
        prd <- "2001_2020"
      }else{
        pnm <- input$gcm_select
        prd <- input$period_select
      }
      if(input$type == "Feasibility"){
        pnm <- "SZ_Ensemble"
        prd <- input$period_feas
      }
      tile_url <- gsub("GCM", pnm, novelty_tileserver)
      tile_url <- gsub("PERIOD", prd, tile_url)
      session$sendCustomMessage("add_novelty",tile_url)
    }else{
      session$sendCustomMessage("remove_novelty","puppy")
    }
  })
  
  curr_cell <- reactiveVal()
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(maxZoom = 12)) %>%
      setView(lng = -122.77222, lat = 54.2665, zoom = 6) %>%
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
      addBGCTiles() %>%
      addRasterTiles() %>%
      addDistricts() %>%
      addLayersControl(
        baseGroups = c("Hillshade","Satellite"),
        overlayGroups = c("BGCs"),
        position = "topright") %>%
      hideGroup("BGCs")
  })
  
  # tile_url <- gsub("GCM", "Historic", base_tileserver)
  # tile_url <- gsub("PERIOD", "1961_1990", tile_url)
  # dat <- list(url = tile_url, type = "SZ")
  # session$sendCustomMessage("update_tiles",dat)
  
  ##add tiles
  observe({
    if(input$type == "BGC"){
      globalLeg$Legend <- NULL
      pnm <- "Historic"
      prd <- "1961_1990"
      ens_type <- "SZ"
      if(input$period_type == "Historic"){
        pnm <- "Historic"
        prd <- "1961_1990"
      } else if (input$period_type == "obs") {
        pnm <- "Obs"
        prd <- "2001_2020"
      } else if (input$period_type == "Future") {
        if(!is.null(input$gcm_select)){
          if(input$gcm_select == "Zone_Ensemble"){
            ens_type <- "Zone"
          }
          pnm <- input$gcm_select
          prd <- input$period_select
        }
      }
      tile_url <- gsub("GCM", pnm, base_tileserver)
      tile_url <- gsub("PERIOD", prd, tile_url)
      print(tile_url)
      dat <- list(url = tile_url, type = ens_type)
      #message("Sending to JS")
      session$sendCustomMessage("update_tiles",dat)
      # if(input$novelty){
      #   session$sendCustomMessage("remove_novelty","puppy")
      #   tile_url <- gsub("GCM", input$gcm_select, novelty_tileserver)
      #   tile_url <- gsub("PERIOD", input$period_select, tile_url)
      #   session$sendCustomMessage("add_novelty",tile_url)
      # }
    }
    if(!is.null(input$species_feas) & input$type != "BGC"){
      #browser()
      if(input$period_type == "Historic"){
        stat <- "HistoricFeas"
        period <- "2001_2020"
      } else if (input$period_type == "obs") {
        stat <- input$map_stat
        period <- "obs_2001_2020"
      } else if (input$period_type == "Future") {
        stat <- input$map_stat
        period <- input$period_feas
      }
      if(!is.null(stat) & !is.null(period) &!is.null(input$edatope_feas)){
        tile_url <- gsub("STAT", stat, species_tileserver)
        tile_url <- gsub("PERIOD", period, tile_url)
        tile_url <- gsub("EDATOPE", input$edatope_feas, tile_url)
        tile_url <- gsub("SPECIES", input$species_feas, tile_url)
        #cat(tile_url)
        session$sendCustomMessage("remove_novelty", "puppy")
        dat <- list(url = tile_url, type = "CCISS")
        session$sendCustomMessage("update_tiles",dat)
      }
      
    }
    
  })
  
  observeEvent(input$map_stat,{
    if(!is.null(input$map_stat)){
      if(input$map_stat == "NewFeas"){
        globalLeg$Legend <- c("Primary","Secondary","Tertiary")
        globalLeg$Colours <- c("#006400", "#1E90FF", "#EEC900")
        globalLeg$Title <- "Climatic Suitability"
      } else if(input$map_stat == "MeanChange") {
        globalLeg$Legend <- c("-3","-2","-1","No change","+1","+2","+3","Becoming unsuitable","Newly Suitable (3)","Newly Suitable (2)","Newly Suitable (1)")
        globalLeg$Colours <- c("#67001F", "#D6604D", "#FDDBC7", "#F7F7F7", 
                               "#D1E5F0", "#4393C3", "#053061", "#000000", 
                               "#FFFFCC", "#FFEDA0", "#FED976")
        globalLeg$Title <- "Change in Suitability"
      }
    }
    
  })
  
  observe({
    if(!is.null(input$period_type) & input$type != "BGC") {
      if(input$period_type == "Historic"){
        globalLeg$Legend <- c("Primary","Secondary","Tertiary")
        globalLeg$Colours <- c("#006400", "#1E90FF", "#EEC900")
        globalLeg$Title <- "Climatic Suitability"
      }
    }
  })
  
  observe({
    if(!is.null(globalLeg$Legend)){
      leafletProxy("map") |>
        addLegend(position = "bottomright",
                  labels = globalLeg$Legend,
                  colors = globalLeg$Colours,
                  title = globalLeg$Title,
                  layerId = "map_legend")
    } else {
      leafletProxy("map") |>
        removeControl("map_legend")
    }
  })
  
  observeEvent(input$map_click,{
    lat <- input$map_click$lat
    lng <- input$map_click$lng
    
    if(input$type == "Feasibility"){
      cell_click <- cellFromXY(t_rast, cbind(lng,lat))
      curr_cell(cell_click)
      print(cell_click)
      # qry <- paste0("select * from bgc_preds where cellid = ",cell_click)
      # #cat(qry)
      # dat <- dbGetQuery(db, qry)
      # 
      # output$bgc_plot <- renderPlotly({
      #   
      #   fig <- plot_ly(data = dat, x = ~fp_code,
      #                  y = ~bgc_prop, split = ~bgc_pred, type = 'bar',
      #                  color = ~bgc_pred, colors = colour_ref, hovertemplate = "%{y}",
      #                  text = ~bgc_pred, textposition = 'inside', textfont = list(color = "black", size = 12),
      #                  texttemplate = "%{text}") %>%
      #     layout(yaxis = list(title = "", tickformat = ".1%"),
      #            # xaxis = list(showspikes = FALSE, title = list(text = "Period"),
      #            #              ticktext = c("2021-2040","2041-2060","2061-2080","2081-2100"),
      #            #              tickvals = dat$fp_code),
      #            barmode = 'stack')
      #   fig
      # })
      
      output$feas_plot <- renderGirafe({
        plot_suitability(dbCon, cellid = cell_click, edatope = input$edatope_feas, spp_name = input$species_feas)
      })
      
      # if(nrow(dat2) < 4){
      #   temp2 <- data.table(fp_code = setdiff(c(2001,2021,2041,2061),dat2$fp_code), newsuit = 5)
      #   temp <- rbind(temp, temp2)
      # }
      shinyalert(title = "Feasibility Plot",
                 text = tagList(
                   girafeOutput("feas_plot")
                 ),
                 html = TRUE
                 )
      
      # showModal(modalDialog(
      #   title = paste0("BGC and Feasibility Projections"),
      #   #plotlyOutput("bgc_plot"),
      #   girafeOutput("feas_plot"),
      #   easyClose = TRUE,
      #   footer = NULL
      # ))
    } else {
      if(input$novelty){
        test_fut <- dbGetQuery(dbCon, paste0("select * from future_climate where \"GCM\" = '",input$gcm_select,
                                             "' and \"PERIOD\" = '",input$period_select,"' and bgc_pred = '",input$bgc_pred_click,"'")) |> as.data.table()
        test_hist <- dbGetQuery(dbCon, paste0("select * from historic_climate where bgc = '",input$bgc_pred_click,"'")) |> as.data.table()
        test_icv <- dbGetQuery(dbCon, paste0("select * from historic_icv where bgc = '",input$bgc_pred_click,"'")) |> as.data.table()
        output$feas_plot <- renderPlotly({
          plot_analog_novelty(clim.target = test_fut, clim.analog = test_hist, clim.icv = test_icv, pcs = NULL)
        })
        
        showModal(modalDialog(
          title = paste0("Analog Novelty Plot"),
          plotlyOutput("feas_plot", height = "70vh"),
          size = "l",
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        cell_click <- cellFromXY(t_rast, cbind(lng,lat))
        curr_cell(cell_click)
        fp <- substr(input$period_feas,1,4)
        qry <- paste0("select * from bgc_preds where cellid = ",cell_click)
        #cat(qry)
        dat <- dbGetQuery(db, qry)
        
        output$bgc_plot_2 <- renderPlotly({
          
          fig <- plot_ly(data = dat, x = ~fp_code,
                         y = ~bgc_prop, split = ~bgc_pred, type = 'bar',
                         color = ~bgc_pred, colors = colour_ref, hovertemplate = "%{y}",
                         text = ~bgc_pred, textposition = 'inside', textfont = list(color = "black", size = 12),
                         texttemplate = "%{text}") %>%
            layout(yaxis = list(title = "", tickformat = ".1%"),
                   # xaxis = list(showspikes = FALSE, title = list(text = "Period"),
                   #              ticktext = c("2021-2040","2041-2060","2061-2080","2081-2100"),
                   #              tickvals = dat$fp_code),
                   barmode = 'stack')
          fig
        })
        
        showModal(modalDialog(
          title = paste0("BGC Projections"),
          plotlyOutput("bgc_plot_2"),
          easyClose = TRUE,
          footer = NULL
        ))
      }
      
    }
    
  })
  
  ##-----------------------------------------
  ## Summary Figures
  ##-----------------------------------------
  plot_vals <- reactiveVal()
  
  observeEvent(input$region_type, {
    if(input$region_type == "None"){
      runjs("
      //console.log('Map clicked!');
      let map = document.getElementById('map-container');
      
      if (map.classList.contains('half-map')) {
        console.log('Expanding map...');
        map.classList.remove('half-map');
        Shiny.setInputValue('toggle_plot', 'hide', {priority: 'event'});
      }
    ")
    } else {
      runjs("
      //console.log('Map clicked!');
      let map = document.getElementById('map-container');
      
      if (!map.classList.contains('half-map')) {
        console.log('Shrinking map...');
        map.classList.add('half-map');
        Shiny.setInputValue('toggle_plot', 'show', {priority: 'event'});
      }
    ")
    }
  })
  
  observeEvent(input$region_type,{
    session$sendCustomMessage("resize_map","waddles")
  })
  
  # observeEvent(input$resize_map, {
  #   leafletProxy("map") %>% invokeMethod(NULL,"invalidateSize")
  # })
  
  # Show or hide the plot based on toggle input
  observeEvent(input$toggle_plot, {
    if (input$toggle_plot == "show") {
      show("plot-container")
    } else {
      hide("plot-container")
    }
  })
  
  # output$ui_select_2 <- renderUI({
  #   if(input$type_2 == "BGC"){
  #     tagList(
  #       h1("BGC Options"),
  #       selectInput("gcm_select_2","Select GCM", choices = gcms, selected = gcms[1]),
  #       selectInput("period_select_2","Select Period", choices = periods, selected = periods[1]),
  #       checkboxInput("novelty_2","Display Novelty?")
  #     )
  #   }else{
  #     tagList(
  #       h1("Feasibility Options"),
  #       selectInput("map_stat_2","Select Map Type", choices = list("Projected Feasibility" = "NewFeas",
  #                                                                "Feasibility Change" = "MeanChange"), multiple = FALSE),
  #       selectInput("period_feas_2","Select Period", choices = c("Obs", periods[-5])),
  #       selectInput("edatope_feas_2","Select Edatope", choices = c("B2","C4","E6"), selected = "C4", multiple = FALSE),
  #       selectInput("species_feas_2", "Select Species", choices = c("Fd","Pl","Sx","Cw","Hw","Bl","At", "Ac", "Ep", "Yc", "Pw", "Ss", "Bg", "Lw", "Sb"), multiple = FALSE)
  #       
  #     )
  #   }
  # })
  # 
  # observeEvent(input$clear_map_2,{
  #   if(input$clear_map_2 %% 2 != 0){
  #     session$sendCustomMessage("clear_tiles_2","Luna")
  #     session$sendCustomMessage("remove_novelty_2","Luna")
  #   }else{
  #     session$sendCustomMessage("unclear_tiles_2","Luna")
  #     if(input$novelty_2){
  #       session$sendCustomMessage("unclear_novelty_2","Luna")
  #     }
  #   }
  #   
  # })
  
  # if(input$novelty){
  #   session$sendCustomMessage("remove_novelty","puppy")
  #   tile_url <- gsub("GCM", input$gcm_select, novelty_tileserver)
  #   tile_url <- gsub("PERIOD", input$period_select, tile_url)
  #   session$sendCustomMessage("add_novelty",tile_url)
  # }
  
  # observeEvent(input$novelty_2,{
  #   if(input$novelty_2){
  #     tile_url <- gsub("GCM", input$gcm_select_2, novelty_tileserver)
  #     tile_url <- gsub("PERIOD", input$period_select_2, tile_url)
  #     # if(input$gcm_select_2 == "SZ_Ensemble"){
  #     #   tile_url <- gsub("png","webp",tile_url)
  #     # }
  #     session$sendCustomMessage("add_novelty_2",tile_url)
  #   }else{
  #     session$sendCustomMessage("remove_novelty_2","puppy")
  #   }
  # })
  
  # output$map_2 <- renderLeaflet({
  #   leaflet(options = leafletOptions(maxZoom = 12)) %>%
  #     setView(lng = -122.77222, lat = 51.2665, zoom = 6) %>%
  #     leaflet::addTiles(
  #       urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mbsty, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
  #       attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
  #       group = "Hillshade",
  #       options = leaflet::pathOptions(pane = "mapPane")) %>%
  #     leaflet::addTiles(
  #       urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mblbsty, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
  #       attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
  #       group = "Cities",
  #       options = leaflet::pathOptions(pane = "overlayPane")) %>%
  #     leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite",
  #                               options = leaflet::pathOptions(pane = "mapPane")) %>%
  #     addPlugin() %>%
  #     addBGCTiles() %>%
  #     addRasterTiles_v2() %>%
  #     addDistricts() %>%
  #     addLayersControl(
  #       baseGroups = c("Hillshade","Satellite"),
  #       overlayGroups = c("BGCs"),
  #       position = "topright")
  # })
  # 
  observe({
    if(input$region_type != "None"){
      if(input$region_type == "District"){
        dat <- list(url = "https://tileserver.thebeczone.ca/data/Districts/{z}/{x}/{y}.pbf", name = "Districts", id = "dist_code")
      }else{
        dat <- list(url = "https://tileserver.thebeczone.ca/data/flp_bnd/{z}/{x}/{y}.pbf", name = "flp", id = "ORG_UNIT")
      }
      session$sendCustomMessage("addRegionTile",dat)
      session$sendCustomMessage("reset_district","Luna")
    }else{
      session$sendCustomMessage("clear_district","Waddles")
    }
  })
  
  # observe({
  #   if(!is.null(input$gcm_select_2) & input$type_2 == "BGC"){
  #     if(input$gcm_select_2 == "Zone_Ensemble"){
  #       ens_type <- "Zone"
  #     }else{
  #       ens_type <- "SZ"
  #     }
  #     tile_url <- gsub("GCM", input$gcm_select_2, base_tileserver)
  #     tile_url <- gsub("PERIOD", input$period_select_2, tile_url)
  #     dat <- list(url = tile_url, type = ens_type) #type = ens_type
  #     message("Sending to JS")
  #     session$sendCustomMessage("update_tiles_2",dat)
  #     if(input$novelty_2){
  #       session$sendCustomMessage("remove_novelty_2","puppy")
  #       tile_url <- gsub("GCM", input$gcm_select_2, novelty_tileserver)
  #       tile_url <- gsub("PERIOD", input$period_select_2, tile_url)
  #       session$sendCustomMessage("add_novelty_2",tile_url)
  #     }
  #   }
  #   if(!is.null(input$map_stat_2) & input$type_2 != "BGC"){
  #     tile_url <- gsub("STAT", input$map_stat_2, species_tileserver)
  #     tile_url <- gsub("PERIOD", input$period_feas_2, tile_url)
  #     tile_url <- gsub("EDATOPE", input$edatope_feas_2, tile_url)
  #     tile_url <- gsub("SPECIES", input$species_feas_2, tile_url)
  #     #cat(tile_url)
  #     if(input$period_feas_2 == "Obs") fmt <- "webp" else fmt <- "png"
  #     tile_url <- gsub("FORMAT", fmt, tile_url)
  #     session$sendCustomMessage("remove_novelty_2", "puppy")
  #     dat <- list(url = tile_url, type = "CCISS")
  #     session$sendCustomMessage("update_tiles_2",dat)
  #   }
  #   
  # })
  # 
  observeEvent(input$dist_click,{
    temp <- dist_bnds[ORG_UNIT == input$dist_click,]
    print(temp)
    leafletProxy("map") %>%
      fitBounds(temp$xmin, temp$ymin, temp$xmax, temp$ymax)
  })
  
  observeEvent(input$reset_district,{
    session$sendCustomMessage("reset_district","Luna")
  })
  
  output$summary_plot <- renderGirafe({
    if(is.null(input$dist_click)) return(NULL)
    stdarea <- input$dist_click
    #print(input$dist_click)
    if(input$period_type %in% c("Historic","obs")){
      gcm_curr <- "ensembleMean"
      run_curr <- "ensembleMean"
    } else {
      if(grepl("Ensemble", input$gcm_select)){
        gcm_curr <- "ensembleMean"
        run_curr <- "ensembleMean"
      }else{
        gcm_curr <- input$gcm_select
        run_curr <- runs_use[gcms_use == input$gcm_select]
      }
    }
    
    if(input$type == "BGC"){
      if(input$zone_sz) smry <- "Zone"
      else smry <- "Subzone"
      plot_bgc(dbCon, stdarea, xvariable = input$xvariable, gcm_nm = gcm_curr, run_nm = run_curr, unit = smry, focal_bgc = plot_vals())
    }else{
      #browser()
      plot_species(dbCon, stdarea, xvariable = input$xvariable, gcm_nm = gcm_curr, 
                   run_nm = run_curr, edatope = input$edatope_feas, spp_select = input$species_feas, focal_species = plot_vals())
    }
  })
  
  observeEvent(input$summary_plot_selected,{
    plot_vals(input$summary_plot_selected)
  })
  
  observeEvent(input$zone_sz,{
    plot_vals(NULL)
  })
  
  observeEvent(input$reset_district,{
    plot_vals(NULL)
  })
  
  observeEvent(input$type,{
    plot_vals(NULL)
  })
  
  observeEvent(input$action_download, {
    if(is.null(input$dist_click)){
      showModal(modalDialog(
        "Please select a district first."
      ))
    }else{
      showModal(modalDialog(
        title = "Download CCISS Raster",
        checkboxInput("clip_download","Clip Raster to Region?"),
        downloadButton("download_cciss","Download Raster"),
        uiOutput("download_legend",inline = F)
      ))
    }
  })
  
  output$download_legend <- renderUI(
    if(input$type_2 == "BGC"){
      a(href="downloadable_docs/BGC_Legend.csv", "Download Legend", download=NA, target="_blank")
    }else{
      if(input$map_stat_2 == "Feasibility"){
        a(href="downloadable_docs/Feasibility_Legend.csv", "Download Legend", download=NA, target="_blank")
      }else{
        a(href="downloadable_docs/MeanChange_Legend.csv", "Download Legend", download=NA, target="_blank")
      }
    }
  )
  
  output$download_cciss <- downloadHandler(
    filename = function(){
      if(input$type_2 == "BGC"){
        paste0("bgc_raw_",input$dist_click, "_", input$gcm_select_2,"_", input$period_select_2,".tif")
      }else{
        paste0(input$map_stat_2,input$dist_click, "_", input$period_feas_2,"_", input$species_feas_2,"_",input$edatopic_feas_2,".tif")
      }
    },
    content = function(file){
      if(input$type_2 == "BGC"){
        lname <- paste0("bgc_raw_",input$gcm_select_2,"_",input$period_select_2,".tif")
        tname <- "bgc_raw"
      }else{
        browser()
        sname <- switch(input$map_stat_2,
                        NewFeas = "Feasibility_",
                        MeanChange = "MeanChange_")
        lname <- paste0(sname,input$period_feas_2,"_",input$edatope_feas_2,"_",input$species_feas_2,".tif")
        tname <- switch(input$map_stat_2,
                        NewFeas = "feasibility_raw2",
                        MeanChange = "meanchange_raw")
      }
      
      bnd <- dist_bnds[ORG_UNIT == input$dist_click,.(ymax, ymin, xmax, xmin)]
      boundary <- t(bnd)[,1]
      rst <- dbGetFeasible(dbCon, table_name = tname, layer_name = lname, boundary = boundary)
      if(input$clip_download){
        bnds <- vect("district_bnds.gpkg")
        bnd <- bnds[bnds$ORG_UNIT == input$dist_click,]
        rst <- mask(rst, bnd)
      }
      #rst <- rst/10
      writeRaster(rst, file, datatype = "INT2S")
    }
  )
  
  ##---------------Find-a-BEC-------------------------

      output$findBGCMap <- renderLeaflet({
        leaflet() %>%
          setView(lng = -122.77222, lat = 51.2665, zoom = 6) %>%
          addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "Positron",
                           options = leaflet::pathOptions(pane = "mapPane")) %>%
          leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite",
                                    options = leaflet::pathOptions(pane = "mapPane")) %>%
          leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap",
                                    options = leaflet::pathOptions(pane = "mapPane")) %>%
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
          addSelectBEC() %>%
          leaflet::addLayersControl(
            baseGroups = c("Hillshade","Positron","Satellite", "OpenStreetMap"),
            overlayGroups = c("BEC","Cities"),
            position = "topright")
      })
  
  
  observeEvent(input$selectBGC,{
    if(input$selectBGC == "(N)"){
      #browser()
      updateSelectInput(session,"selectSubzone",choices = subzones,selected = "")
      session$sendCustomMessage("clearBEC","xxx")
    }else{
      temp <- subzones[grep(input$selectBGC,subzones)]
      updateSelectInput(session,"selectSubzone",choices = temp,selected = temp)
    }
  })
  
  observeEvent(input$selectSubzone,{
    session$sendCustomMessage("highlightBEC",input$selectSubzone)
  })
  
  observeEvent(input$becselect_click,{
    output$selectedBEC <- renderText({
      if(length(input$becselect_click) > 1){
        c("Selected BGC: ",
          input$selectBGC)
      }else{
        c("Selected BGC: ",
          input$becselect_click)
      }
      
    })
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
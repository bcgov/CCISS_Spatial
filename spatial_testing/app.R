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

source("./JS_Source.R")
source("./plot_functions.R", local = TRUE)
dist_bnds <- fread("./district_bounds.csv")
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

gcms <- c("SZ_Ensemble", "Zone_Ensemble", "Obs", "ACCESS-ESM1-5","EC-Earth3","GISS-E2-1-G","MIROC6","MPI-ESM1-2-HR")
periods <- c("2001_2020", "2021_2040", "2041_2060", "2061_2080","2081_2100")
base_tileserver <- "https://tileserver.thebeczone.ca/data/bgc_GCM_PERIOD/{z}/{x}/{y}.webp"
novelty_tileserver <- "https://tileserver.thebeczone.ca/data/Novelty_GCM_PERIOD/{z}/{x}/{y}.webp"
species_tileserver <- "https://tileserver.thebeczone.ca/data/STAT_PERIOD_EDATOPE_SPECIES/{z}/{x}/{y}.FORMAT"

colour_ref <- subzones_colours_ref$colour
names(colour_ref) <- subzones_colours_ref$classification

gcms_use <- c("ACCESS-ESM1-5","EC-Earth3","GISS-E2-1-G","MIROC6","MPI-ESM1-2-HR","MRI-ESM2-0")
runs_use <- c("r1i1p1f1","r4i1p1f1","r2i1p3f1","r2i1p1f1","r1i1p1f1","r1i1p1f1")

ui <- fluidPage(
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
                           radioButtons("type","Display BGC or Feasibility", choices = c("BGC","Feasibility"), selected = "BGC"),
                           uiOutput("ui_select"),
                           actionButton("clear_map","Toggle CCISS"),
                           tags$head(tags$style(".modal-body{ min-height:70vh}"))
                           #  h1("BGC Options"),
                           #  selectInput("gcm_select","Select GCM", choices = gcms, selected = gcms[1]),
                           #  selectInput("period_select","Select Period", choices = periods, selected = periods[1]),
                           #  h1("Feasibility Options"),
                           # selectInput("map_stat","Select Map Type", choices = list("Projected Feasibility" = "NewFeas",
                           #                                                         "Feasibility Change" = "MeanChangeTest",
                           #                                                         "Add/Retreat" = "AddRet"), multiple = FALSE),
                           # selectInput("period_feas","Select Period", choices = periods[-5]),
                           # selectInput("edatope_feas","Select Edatope", choices = c("B2","C4","E6"), multiple = FALSE),
                           # selectInput("species_feas", "Select Species", choices = c("Pl","Sx","Fd","Cw","Hw","Bl","At", "Ac", "Ep", "Yc", "Pw", "Ss", "Bg", "Lw", "Sb"), multiple = FALSE)
                           
                         ),
                         mainPanel(
                           leafletOutput("map", height = "90vh")
                         )
                       )
              ),
              tabPanel("Summary by District",
                       fluidRow(
                         column(2,
                                radioButtons("type_2","Display BGC or Feasibility", choices = c("BGC","Feasibility"), selected = "BGC"),
                                uiOutput("ui_select_2"),
                                actionButton("clear_map_2","Toggle CCISS"),
                                actionButton("reset_district","Clear Selected District"),
                                downloadButton("download_cciss","Download Raster"),
                                tags$head(tags$style(".modal-body{ min-height:70vh}"))
                                ),
                         column(5,
                                leafletOutput("map_2", height = "90vh")),
                         column(5,
                                selectInput("xvariable","X-Axis Variable", choices = c("Time","MAT","MAP","CMD","DD5")),
                                checkboxInput("zone_sz","Summarise by Zone?",value = TRUE),
                                plotOutput("summary_plot")
                                )
                       )
                       )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  dist_nm <- reactiveVal()
  
  output$ui_select <- renderUI({
    if(input$type == "BGC"){
      tagList(
        h1("BGC Options"),
        selectInput("gcm_select","Select GCM", choices = gcms, selected = gcms[1]),
        selectInput("period_select","Select Period", choices = periods, selected = periods[1]),
        checkboxInput("novelty","Display Novelty?")
      )
    }else{
      tagList(
        h1("Feasibility Options"),
        selectInput("map_stat","Select Map Type", choices = list("Projected Feasibility" = "NewFeas",
                                                                 "Feasibility Change" = "MeanChange",
                                                                 "Add/Retreat" = "AddRet"), multiple = FALSE),
        selectInput("period_feas","Select Period", choices = c("Obs", periods[-5])),
        selectInput("edatope_feas","Select Edatope", choices = c("B2","C4","E6"), multiple = FALSE),
        selectInput("species_feas", "Select Species", choices = c("Pl","Sx","Fd","Cw","Hw","Bl","At", "Ac", "Ep", "Yc", "Pw", "Ss", "Bg", "Lw", "Sb"), multiple = FALSE)
        
      )
    }
  })
  
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
  
  observeEvent(input$novelty,{
    if(input$novelty){
      tile_url <- gsub("GCM", input$gcm_select, novelty_tileserver)
      tile_url <- gsub("PERIOD", input$period_select, tile_url)
      if(input$gcm_select == "SZ_Ensemble"){
        tile_url <- gsub("png","webp",tile_url)
      }
      session$sendCustomMessage("add_novelty",tile_url)
    }else{
      session$sendCustomMessage("remove_novelty","puppy")
    }
  })
  
  curr_cell <- reactiveVal()
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(maxZoom = 12)) %>%
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
      addBGCTiles() %>%
      addRasterTiles() %>%
      addLayersControl(
        baseGroups = c("Hillshade","Satellite"),
        overlayGroups = c("BGCs"),
        position = "topright")
  })
  
  ##add tiles
  observe({
    if(!is.null(input$gcm_select) & input$type == "BGC"){
      if(input$gcm_select == "Zone_Ensemble"){
        ens_type <- "Zone"
      }else{
        ens_type <- "SZ"
      }
      tile_url <- gsub("GCM", input$gcm_select, base_tileserver)
      tile_url <- gsub("PERIOD", input$period_select, tile_url)
      dat <- list(url = tile_url, type = ens_type)
      message("Sending to JS")
      session$sendCustomMessage("update_tiles",dat)
      if(input$novelty){
        session$sendCustomMessage("remove_novelty","puppy")
        tile_url <- gsub("GCM", input$gcm_select, novelty_tileserver)
        tile_url <- gsub("PERIOD", input$period_select, tile_url)
        session$sendCustomMessage("add_novelty",tile_url)
      }
    }
    if(!is.null(input$map_stat) & input$type != "BGC"){
      tile_url <- gsub("STAT", input$map_stat, species_tileserver)
      tile_url <- gsub("PERIOD", input$period_feas, tile_url)
      tile_url <- gsub("EDATOPE", input$edatope_feas, tile_url)
      tile_url <- gsub("SPECIES", input$species_feas, tile_url)
      if(input$period_feas == "Obs") fmt <- "webp" else fmt <- "png"
      tile_url <- gsub("FORMAT", fmt, tile_url)
      #cat(tile_url)
      session$sendCustomMessage("remove_novelty", "puppy")
      dat <- list(url = tile_url, type = "CCISS")
      session$sendCustomMessage("update_tiles",dat)
    }
    
  })
  observeEvent(input$map_click,{
    lat <- input$map_click$lat
    lng <- input$map_click$lng
    
    if(input$type == "Feasibility"){
      cell_click <- cellFromXY(t_rast, cbind(lng,lat))
      curr_cell(cell_click)
      fp <- substr(input$period_feas,1,4)
      qry <- paste0("select * from bgc_preds where cellid = ",cell_click)
      #cat(qry)
      dat <- dbGetQuery(db, qry)
      
      output$bgc_plot <- renderPlotly({
        # fig <- plot_ly(dat, labels = ~bgc_pred, values = ~bgc_prop, type = "pie")
        # fig <- fig %>% layout(title = 'BGC Projections for Location',
        #                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        #                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
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
      
      eda <- switch(input$edatope_feas,
                    "B2" = 1,
                    "C4" = 2,
                    "E6" = 3
      )
      qry <- paste0("select * from cciss_feas where edatope = ",
                    eda," and species = '", input$species_feas,
                    "' and cellid = ",cell_click)
      #cat(qry)
      dat2 <- as.data.table(dbGetQuery(db, qry))
      temp <- dat2[,.(fp_code, newsuit)]
      if(nrow(dat2) < 4){
        temp2 <- data.table(fp_code = setdiff(c(2001,2021,2041,2061),dat2$fp_code), newsuit = 5)
        temp <- rbind(temp, temp2)
      }
      temp <- rbind(temp, data.table(fp_code = 1981, newsuit = dat2$curr[1]))
      setorder(temp, fp_code)
      
      output$feas_plot <- renderPlotly({
        fig <- plot_ly(temp, x = ~fp_code, y =~newsuit, type = "scatter", mode = "lines+markers") %>%
          layout(xaxis = list(showspikes = FALSE, title = list(text = "Period"),
                              ticktext = c("Current","2001-2020", "2021-2040","2041-2060","2061-2080"),
                              tickvals = temp$fp_code),
                 yaxis = list(range = c(5,1)))
        fig
      })
      
      showModal(modalDialog(
        title = paste0("Click Values for ",curr_cell()),
        plotlyOutput("bgc_plot"),
        plotlyOutput("feas_plot"),
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      print(input$bgc_pred_click)
      test_fut <- dbGetQuery(dbCon, paste0("select * from future_climate where \"GCM\" = '",input$gcm_select,
                                           "' and \"PERIOD\" = '",input$period_select,"' and bgc_pred = '",input$bgc_pred_click,"'")) |> as.data.table()
      test_hist <- dbGetQuery(dbCon, paste0("select * from historic_climate where bgc = '",input$bgc_pred_click,"'")) |> as.data.table()
      test_icv <- dbGetQuery(dbCon, paste0("select * from historic_icv where bgc = '",input$bgc_pred_click,"'")) |> as.data.table()
      output$feas_plot <- renderPlotly({
        plot_analog_novelty(clim.target = test_fut, clim.analog = test_hist, clim.icv = test_icv, pcs = NULL)
      })
      
      showModal(modalDialog(
        title = paste0("Analog Novelty Plot ",curr_cell()),
        plotlyOutput("feas_plot", height = "70vh"),
        size = "l",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
  })
  
  ##-----------------------------------------
  ## TAB 2
  ##-----------------------------------------
  output$ui_select_2 <- renderUI({
    if(input$type_2 == "BGC"){
      tagList(
        h1("BGC Options"),
        selectInput("gcm_select_2","Select GCM", choices = gcms, selected = gcms[1]),
        selectInput("period_select_2","Select Period", choices = periods, selected = periods[1]),
        checkboxInput("novelty_2","Display Novelty?")
      )
    }else{
      tagList(
        h1("Feasibility Options"),
        selectInput("map_stat_2","Select Map Type", choices = list("Projected Feasibility" = "NewFeas",
                                                                 "Feasibility Change" = "MeanChange",
                                                                 "Add/Retreat" = "AddRet"), multiple = FALSE),
        selectInput("period_feas_2","Select Period", choices = c("Obs", periods[-5])),
        selectInput("edatope_feas_2","Select Edatope", choices = c("B2","C4","E6"), multiple = FALSE),
        selectInput("species_feas_2", "Select Species", choices = c("Pl","Sx","Fd","Cw","Hw","Bl","At", "Ac", "Ep", "Yc", "Pw", "Ss", "Bg", "Lw", "Sb"), multiple = FALSE)
        
      )
    }
  })
  
  observeEvent(input$clear_map_2,{
    if(input$clear_map_2 %% 2 != 0){
      session$sendCustomMessage("clear_tiles_2","Luna")
      session$sendCustomMessage("remove_novelty_2","Luna")
    }else{
      session$sendCustomMessage("unclear_tiles_2","Luna")
      if(input$novelty_2){
        session$sendCustomMessage("unclear_novelty_2","Luna")
      }
    }
    
  })
  
  observeEvent(input$novelty_2,{
    if(input$novelty_2){
      tile_url <- gsub("GCM", input$gcm_select_2, novelty_tileserver)
      tile_url <- gsub("PERIOD", input$period_select_2, tile_url)
      if(input$gcm_select_2 == "SZ_Ensemble"){
        tile_url <- gsub("png","webp",tile_url)
      }
      session$sendCustomMessage("add_novelty_2",tile_url)
    }else{
      session$sendCustomMessage("remove_novelty_2","puppy")
    }
  })
  
  output$map_2 <- renderLeaflet({
    leaflet(options = leafletOptions(maxZoom = 12)) %>%
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
      addBGCTiles() %>%
      addRasterTiles_v2() %>%
      addDistricts() %>%
      addLayersControl(
        baseGroups = c("Hillshade","Satellite"),
        overlayGroups = c("BGCs"),
        position = "topright")
  })
  
  observe({
    if(!is.null(input$gcm_select_2) & input$type == "BGC"){
      if(input$gcm_select_2 == "Zone_Ensemble"){
        ens_type <- "Zone"
      }else{
        ens_type <- "SZ"
      }
      tile_url <- gsub("GCM", input$gcm_select_2, base_tileserver)
      tile_url <- gsub("PERIOD", input$period_select_2, tile_url)
      dat <- list(url = tile_url, type = ens_type) #type = ens_type
      message("Sending to JS")
      session$sendCustomMessage("update_tiles_2",dat)
      if(input$novelty_2){
        session$sendCustomMessage("remove_novelty_2","puppy")
        tile_url <- gsub("GCM", input$gcm_select_2, novelty_tileserver)
        tile_url <- gsub("PERIOD", input$period_select_2, tile_url)
        session$sendCustomMessage("add_novelty_2",tile_url)
      }
    }
    if(!is.null(input$map_stat_2) & input$type_2 != "BGC"){
      tile_url <- gsub("STAT", input$map_stat_2, species_tileserver)
      tile_url <- gsub("PERIOD", input$period_feas_2, tile_url)
      tile_url <- gsub("EDATOPE", input$edatope_feas_2, tile_url)
      tile_url <- gsub("SPECIES", input$species_feas_2, tile_url)
      #cat(tile_url)
      if(input$period_feas_2 == "Obs") fmt <- "webp" else fmt <- "png"
      tile_url <- gsub("FORMAT", fmt, tile_url)
      session$sendCustomMessage("remove_novelty_2", "puppy")
      dat <- list(url = tile_url, type = "CCISS")
      session$sendCustomMessage("update_tiles_2",dat)
    }
    
  })
  
  observeEvent(input$dist_click,{
    temp <- dist_bnds[ORG_UNIT == input$dist_click,]
    print(temp)
    leafletProxy("map_2") %>%
      fitBounds(temp$xmin, temp$ymin, temp$xmax, temp$ymax)
  })
  
  observeEvent(input$reset_district,{
    session$sendCustomMessage("reset_district","Luna")
  })
  
  output$summary_plot <- renderPlot({
    if(is.null(input$dist_click)) return(NULL)
    stdarea <- input$dist_click
    if(grepl("Ensemble", input$gcm_select_2)){
      gcm_curr <- "ensembleMean"
      run_curr <- "ensembleMean"
    }else{
      gcm_curr <- input$gcm_select_2
      run_curr <- runs_use[gcms_use == input$gcm_select_2]
    }
    if(input$type_2 == "BGC"){
      if(input$zone_sz) smry <- "Zone"
      else smry <- "Subzone"
      plot_bgc(dbCon, stdarea, xvariable = input$xvariable, gcm_nm = gcm_curr, run_nm = run_curr, unit = smry)
    }else{
      plot_species(dbCon, stdarea, xvariable = input$xvariable, gcm_nm = gcm_curr, 
                   run_nm = run_curr, edatope = input$edatope_feas_2, spp_select = input$species_feas_2)
    }
  })
  
  output$download_cciss <- downloadHandler(
    filename = function(){
      paste0("Feasibility_",input$dist_click, "_", input$period_feas_2,"_", input$species_feas_2,"_",input$edatopic_feas_2,".tif")
    },
    content = function(file){
      #browser()
      lname <- paste0("Feasibility_",input$period_feas_2,"_",input$edatope_feas_2,"_",input$species_feas_2,".tif")
      bnd <- dist_bnds[ORG_UNIT == input$dist_click,.(ymax, ymin, xmax, xmin)]
      boundary <- t(bnd)[,1]
      rst <- dbGetFeasible(dbCon, table_name = "feasibility_raw2", layer_name = lname, boundary = boundary)
      #rst <- rst/10
      writeRaster(rst, file, datatype = "INT1U")
    }
  )

}

# Run the application 
shinyApp(ui = ui, server = server)
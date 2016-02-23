shpPolyInput <- function(id, label, btn){
  ns <- NS(id)
  tagList(
    bsModal(ns("modal_shp"), "Mask climate map overlays to a shapefile", btn, size="large",
      fluidRow(
        column(12,
          fileInput(ns("shp_file"), label=label, accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE, width="100%")
        )
      ),
      tabsetPanel(
        tabPanel("Original shapefile", plotOutput(ns("Shp_Plot"), height="auto"), value="original"),
        tabPanel("Final overlay", leafletOutput(ns("Map")), value="final"),
        id=ns("tp_shp")
      ),
      actionButton(ns("mask_btn"), "Crop and Mask to Shapefile", class="btn-block")
    )
  )
}

shpPoly <- function(input, output, session){
  ns <- session$ns
  userFile <- reactive({
    validate(need(input$shp_file, message=FALSE))
    input$shp_file
  })
  tp <- reactive({
    validate(need(input$tp_shp, message=FALSE))
    input$tp_shp
  })

  shp <- reactive({
    req(input$shp_file)
    if(!is.data.frame(userFile())) return()
    infiles <- userFile()$datapath
    dir <- unique(dirname(infiles))
    outfiles <- file.path(dir, userFile()$name)
    purrr::walk2(infiles, outfiles, ~file.rename(.x, .y))
    readOGR(dir, strsplit(userFile()$name[1], "\\.")[[1]][1])
  })

  shp_wgs84 <- reactive({ spTransform(shp(), CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) })
  lon <- reactive({ (xmin(shp_wgs84()) + xmax(shp_wgs84()))/2 })
  lat <- reactive({ (ymin(shp_wgs84()) + ymax(shp_wgs84()))/2 })
  plot_ht <- reactive({ if(is.null(shp())) 0 else 400 })
  output$Shp_Plot <- renderPlot({ if(!is.null(shp())) plot(shp(), col="cornflowerblue") }, height=function() plot_ht())
  output$Map <- renderLeaflet({ leaflet() %>% setView(0, 0, zoom=2) %>% addTiles() })

  observe({
    if(!is.null(shp()) && tp()=="final"){
      leafletProxy(ns("Map")) %>% clearShapes() %>% setView(lon(), lat(), zoom=2) %>% addPolygons(data=shp_wgs84(), weight=2)
    }
  })

  out <- reactive({ if(input$mask_btn==0) NULL else shp_wgs84 })
  out
}

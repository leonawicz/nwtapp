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
        tabPanel("Original shapefile", plotOutput(ns("Shp_Plot"), height="auto")),
        tabPanel("Final overlay", leafletOutput(ns("Map")))
      )
    )
  )
}

shpPoly <- function(input, output, session){
  ns <- session$ns

  userFile <- reactive({
    validate(need(input$shp_file, message=FALSE))
    input$shp_file
  })

  shp <- reactive({
    req(input$shp_file)
    if(!is.data.frame(userFile())) return()
    infiles <- userFile()$datapath
    dir <- unique(dirname(infiles))
    outfiles <- file.path(dir, userFile()$name)
    purrr::walk2(infiles, outfiles, ~file.rename(.x, .y))
    x <- readOGR(dir, strsplit(userFile()$name[1], "\\.")[[1]][1])
    print(x)
    x
  })

  shp_wgs84 <- reactive({
    spTransform(shp(), CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  })

  plot_ht <- reactive({ if(is.null(shp())) 0 else 400 })
  output$Shp_Plot <- renderPlot({ if(!is.null(shp())) plot(shp(), col="cornflowerblue") }, height=function() plot_ht())
  output$Map <- renderLeaflet({ leaflet() %>% addTiles() })

  observe({
    if(!is.null(shp())) leafletProxy(ns("Map")) %>% addPolygons(data=shp_wgs84())
  })

  shp_wgs84
}

colpals <- RColorBrewer::brewer.pal.info

shinyServer(function(input, output, session) {

  mon_index <- reactive({ match(input$mon, month.abb) })

  Variable <- reactive({ vars[which(var.labels==input$variable)] })

  ras <- reactive({
    filter(d, Var==Variable() & RCP==input$rcp & Model==input$model)$Data[[1]][[mon_index()]] %>%
      subset(which(decades==input$dec))
  })

  ras_vals <- reactive({ values(ras()) })

  output$Colpal_div_options <- renderUI({
    pals <- c("Custom", rownames(colpals)[colpals["category"]=="div"])
    selectInput("colpal_div", "Palette", pals, pals[1])
  })

  output$Colpal_seq_options <- renderUI({
    pals <- c("Custom", rownames(colpals)[colpals["category"]=="seq"])
    selectInput("colpal_seq", "Palette", pals, pals[1])
  })

  Colors <- reactive({
    req(input$colpal_div)
    type <- input$colpal_type
    custom.colors <- c(input$col_low, input$col_med, input$col_high)
    if(type=="Sequential") custom.colors <- custom.colors[c(1,3)]
    pal <- if(type=="Divergent") input$colpal_div else if(type=="Sequential") input$colpal_seq
    if(pal=="Custom") custom.colors else pal
  })

  pal <- reactive({
    colorNumeric(Colors(), ras_vals(), na.color="transparent")
  })

  Legend_Title <- reactive({ if(input$variable=="Precipitation") "Precipitation (mm)" else if(input$variable=="Temperature") "Temperature (C)" })

  output$Map <- renderLeaflet({
    leaflet() %>% setView(lon, lat, 4) %>% addTiles()
  })

  observe({
    proxy <- leafletProxy("Map")
    proxy %>% removeTiles(layerId="rasimg") %>% addRasterImage(ras(), colors=pal(), opacity=0.8, layerId="rasimg")
  })

  observe({
    proxy <- leafletProxy("Map")
    proxy %>% clearControls()
    if (input$legend) {
      proxy %>% addLegend(position="bottomright", pal=pal(), values=ras_vals(), title=Legend_Title())
    }
  })

})

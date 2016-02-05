library(ggplot2)
colpals <- RColorBrewer::brewer.pal.info

shinyServer(function(input, output, session) {

  mon_index <- reactive({ match(input$mon, month.abb) })
  time_of_year <- reactive({ if(input$mon_or_sea=="Monthly") input$mon else input$sea })

  Variable <- reactive({ vars[which(var.labels==input$variable)] })

  CRU_ras <- reactive({
    idx <- match(time_of_year(), names(cru6190[[Variable()]]))
    subset(cru6190[[Variable()]], idx)
  })

  ras <- reactive({
    dec.idx <- which(decades==input$dec)
    mon.idx <- switch(time_of_year(), Winter=c(1,2,12), Spring=3:5, Summer=6:8, Fall=9:11)
    sea_func <- if(Variable()=="pr") sum else mean
    stat_func <- switch(input$model_stats, Mean=mean,
                        Min=function(x,...) min(x,...),
                        Max=function(x,...) max(x,...),
                        Spread=function(x,...) range(x,...))

    mung_models <- function(x, mon_or_sea, mo, dec, mo2, f_sea){
      if(mon_or_sea=="Monthly"){
        x[[mo]] %>% subset(dec)
      } else {
        calc(brick(lapply(x[mo2], function(x, idx) subset(x, idx), idx=dec)), f_sea) %>% round(1)
      }
    }

    mung_stats <- function(x, mon_or_sea, mo, dec, mo2, f_sea, f_stat, statid){
      if(mon_or_sea=="Seasonal") mo <- mo2
      x <- x %>% do(., Data=.$Data[[1]][mo] %>% purrr::map(~subset(.x, dec)) %>% brick %>% calc(f_sea))
      x <- f_stat(brick(x$Data))
      if(statid=="Spread") x <- calc(x, function(x) x[2]-x[1])
      round(x, 1)
    }

    if(input$mod_or_stat=="Statistic"){
      x <- filter(d, Var==Variable() & RCP==input$rcp) %>% group_by(Model, add=T) %>%
        mung_stats(input$mon_or_sea, mon_index(), dec.idx, mon.idx, sea_func, stat_func, input$model_stats)
      return(x)
    }

    if(input$mod_or_stat=="Single GCM"){
      x <- filter(d, Var==Variable() & RCP==input$rcp & Model==input$model)$Data[[1]]
      x <- mung_models(x, input$mon_or_sea, mon_index(), dec.idx, mon.idx, sea_func)
      if(input$deltas & Variable()=="pr"){
        x <- round(x / CRU_ras(), 2)
        x[is.infinite(x)] <- NA
      }
      if(input$deltas & Variable()=="tas") x <- x - CRU_ras()
    }

    x[is.nan(x)] <- NA
    x
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

  Legend_Title <- reactive({
    p <- input$variable=="Precipitation"
    d <- input$deltas
    if(p & d) "Precip. deltas" else if(p) "Precipitation (mm)" else if(!p & d) "Temp. deltas (C)" else "Temperature (C)"
  })

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

  output$sp_density_plot <- renderPlot({
    x <- ras_vals()
    x <- data.table(`Spatial Distribution`=x[!is.na(x)])
    tp_theme <- theme(plot.background=element_blank())
    ggplot(x, aes(`Spatial Distribution`)) + geom_density(fill="#33333350") + tp_theme + labs(x=Legend_Title())
  }, width=300, height=300, bg="transparent")

  observe({
    if(input$mod_or_stat=="Statistic"){
      updateCheckboxInput(session, "deltas", value=FALSE)
    }
  })

})

library(ggplot2)
colpals <- RColorBrewer::brewer.pal.info

shinyServer(function(input, output, session) {

  # setup
  mon_index <- reactive({ match(input$mon, month.abb) })
  time_of_year <- reactive({ if(input$mon_or_sea=="Monthly") input$mon else input$sea })

  Variable <- reactive({ vars[var.labels==input$variable] })

  sea_func <- reactive({ if(Variable()=="pr") sum else mean })

  stat_func <- reactive({
      switch(input$model_stats,
    Mean=mean,  Min=function(x,...) min(x,...), Max=function(x,...) max(x,...), Spread=function(x,...) range(x,...))
  })

  CRU_ras <- reactive({
    idx <- match(time_of_year(), names(cru6190[[Variable()]]))
    subset(cru6190[[Variable()]], idx)
  })

  # prepping GCM/CRU, raw/deltas, months/seasons, models/stats, temp/precip
  ras <- reactive({
    dec.idx <- which(decades==input$dec)
    mon.idx <- switch(time_of_year(), Winter=c(1,2,12), Spring=3:5, Summer=6:8, Fall=9:11)

    mung_models <- function(x, mon_or_sea, mo, dec, mo2, f_sea){
      if(mon_or_sea=="Monthly"){
        x[[mo]] %>% subset(dec)
      } else {
        calc(brick(lapply(x[mo2], function(x, idx) subset(x, idx), idx=dec)), f_sea) %>% round(1)
      }
    }

    mung_stats <- function(x, mon_or_sea, mo, dec, mo2, f_sea, f_stat, statid){
      if(mon_or_sea=="Seasonal") mo <- mo2
      x <- x %>% do(., Maps=.$Maps[[1]][mo] %>% purrr::map(~subset(.x, dec)) %>% brick %>% calc(f_sea))
      x <- f_stat(brick(x$Maps))
      if(statid=="Spread") x <- calc(x, function(x) x[2]-x[1])
      round(x, 1)
    }

    if(input$mod_or_stat=="Statistic"){
      x <- filter(d, Var==Variable() & RCP==input$rcp) %>% group_by(Model, add=T) %>%
        mung_stats(input$mon_or_sea, mon_index(), dec.idx, mon.idx, sea_func(), stat_func(), input$model_stats)
      return(x)
    }

    if(input$mod_or_stat=="Single GCM"){
      x <- filter(d, Var==Variable() & RCP==input$rcp & Model==input$model)$Maps[[1]]
      x <- mung_models(x, input$mon_or_sea, mon_index(), dec.idx, mon.idx, sea_func())
      if(input$deltas & Variable()=="pr"){
        x <- round(x / CRU_ras(), 2)
        x[is.infinite(x)] <- NA
      }
      if(input$deltas & Variable()=="tas") x <- x - CRU_ras()
    }

    x[is.nan(x)] <- NA
    x
  })

  # store raster values once, separate from raster object
  ras_vals <- reactive({ values(ras()) })

  # Colors and color palettes
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

  # Map legend title, also used for spatial summary density plot
  Legend_Title <- reactive({
    p <- input$variable=="Precipitation"
    d <- input$deltas
    if(p & d) "Precip. deltas" else if(p) "Precipitation (mm)" else if(!p & d) "Temp. deltas (C)" else "Temperature (C)"
  })

  # Initialize map
  output$Map <- renderLeaflet({
    leaflet() %>% setView(lon, lat, 4) %>% addTiles() %>%
      addCircleMarkers(data=locs, radius = ~10, color= ~"#000000", stroke=FALSE, fillOpacity=0.5, group="locations", layerId = ~loc)
  })

  #### Map-related observers ####
  observe({ # raster layers
    proxy <- leafletProxy("Map")
    proxy %>% removeTiles(layerId="rasimg") %>% addRasterImage(ras(), colors=pal(), opacity=0.8, layerId="rasimg")
  })

  observe({ # legend
    proxy <- leafletProxy("Map")
    proxy %>% clearControls()
    if (input$legend) {
      proxy %>% addLegend(position="bottomright", pal=pal(), values=ras_vals(), title=Legend_Title())
    }
  })

  observe({ # show or hide location markers
    proxy <- leafletProxy("Map")
    if (input$show_communities) {
      proxy %>% showGroup("locations")
    } else {
      proxy %>% hideGroup("locations") %>% removeMarker(layerId="Selected")
    }
  })

  observeEvent(input$Map_marker_click, { # update the map markers and view on map clicks
    p <- input$Map_marker_click
    proxy <- leafletProxy("Map")
    if(p$id=="Selected"){
      proxy %>% removeMarker(layerId="Selected")
    } else {
      proxy %>% setView(lng=p$lng, lat=p$lat, input$Map_zoom) %>% addCircleMarkers(p$lng, p$lat, radius=10, color="black", fillColor="orange", fillOpacity=1, opacity=1, stroke=TRUE, layerId="Selected") #add_CM(p)
    }
  })

  observeEvent(input$Map_marker_click, { # update the location selectInput on map clicks
    p <- input$Map_marker_click
    if(!is.null(p$id)){
      if(is.null(input$location) || input$location!=p$id) updateSelectInput(session, "location", selected=p$id)
    }
  })

  observeEvent(input$location, { # update the map markers and view on location selectInput changes
    p <- input$Map_marker_click
    p2 <- subset(locs, loc==input$location)
    proxy <- leafletProxy("Map")
    if(nrow(p2)==0){
      proxy %>% removeMarker(layerId="Selected")
    } else if(length(p$id) && input$location!=p$id){
      proxy %>% setView(lng=p2$lon, lat=p2$lat, input$Map_zoom) %>% addCircleMarkers(p2$lon, p2$lat, radius=10, color="black", fillColor="orange", fillOpacity=1, opacity=1, stroke=TRUE, layerId="Selected") #add_CM(p2)
    } else if(!length(p$id)){
      proxy %>% setView(lng=p2$lon, lat=p2$lat, input$Map_zoom) %>% addCircleMarkers(p2$lon, p2$lat, radius=10, color="black", fillColor="orange", fillOpacity=1, opacity=1, stroke=TRUE, layerId="Selected") #add_CM(p2)
    }
  })
  #### END Map-related observers ####

  # Location data
  Loc_Var <- reactive({ vars[var.labels==input$loc_variable] })

  Data <- reactive({
    x <- select(d, Var, RCP, Model, Locs) %>% group_by(RCP, Model, Var) %>%
      filter(Var==Loc_Var() & RCP==input$loc_rcp) %>%
      do(Locs=filter(.$Locs[[1]], Location==input$location)) %>% unnest

    x <- group_by(d.cru, Var) %>% do(Locs=filter(.$Locs[[1]], Location==input$location)) %>% unnest %>%
      filter(Var==Loc_Var()) %>% mutate(RCP="historical", Model="CRU 3.2") %>% bind_rows(x) %>%
      select(Location, Var, RCP, Model, Year, Month, value) %>%
      group_by(Location, Var, RCP, Model, Month)
    x
  })

  Data_sub <- reactive({ # subset data
    p <- input$loc_toy
    monthly <- p %in% month.abb
    mos <- if(monthly) p else month.abb[sea.idx[[input$loc_toy]]]
    x <- Data() %>% filter(Month %in% mos)
    if(monthly){
      x <- group_by(x)
    } else {
      f_sea <- if(Loc_Var()=="pr") sum else mean
      rnd <- ifelse(Loc_Var()=="pr", 0, 1)
      if(p=="Winter"){
        x <- mutate(x, PrevYear=c(NA, value[1:(length(value)-1)])) %>%
          mutate(value=ifelse(Month=="Dec", PrevYear, value))
      }
      x <- mutate(x, Season=factor(season.labels.long[match(Month, month.abb)], levels=season.labels)) %>%
        group_by(Location, Var, RCP, Model, Year, Season) %>% summarise(value=f_sea(value)) %>% group_by %>%
        mutate(value=round(value, rnd))
    }
    x
  })

  Data_sub2 <- reactive({ # transform to deltas and/or remove CRU 3.2 if required
    x <- Data_sub()
    cru.mean <- (filter(x, Model=="CRU 3.2" & Year %in% 1961:1990) %>% summarise(Mean=mean(value)))$Mean
    if(input$loc_deltas){
      x <- group_by(x, Model)
      if(Loc_Var()=="pr"){
         x <- mutate(x, value=round(value / cru.mean, 2))
        #x[is.infinite(x)] <- NA
      } else {
        x <- mutate(x, value=round(value - cru.mean, 2))
      }
    }
    if(!input$loc_cru) x <- filter(x, Model!="CRU 3.2")
    x
  })

  # Outputs for location modal
  output$TestPlot <- renderPlot({
    p <- Loc_Var()=="pr"
    d <- input$loc_deltas
    ylb <- if(p & d) "Precipitation deltas" else if(p) "Precipitation (mm)" else if(!p & d) "Temperature deltas (C)" else "Temperature (C)"
    g <- ggplot(Data_sub2(), aes(Year, value, colour=Model)) + geom_line() +
      labs(y=ylb) + theme(legend.position="bottom") +
      stat_summary(data=filter(Data_sub2(), Model!="CRU 3.2"), aes(colour=NULL), geom="smooth", colour="black")
    g
  })

  output$TestTable <- renderDataTable({
    Data_sub2()
  }, options = list(pageLength=5))

  # Spatial distribution density plot
  output$sp_density_plot <- renderPlot({
    x <- ras_vals()
    x <- data.table(`Spatial Distribution`=x[!is.na(x)])
    tp_theme <- theme(plot.background=element_blank())
    ggplot(x, aes(`Spatial Distribution`)) + geom_density(fill="#33333350") + tp_theme + labs(x=Legend_Title())
  }, width=300, height=300, bg="transparent")

  observe({ # no deltas allowed when comparing across models
    if(input$mod_or_stat=="Statistic"){
      updateCheckboxInput(session, "deltas", value=FALSE)
    }
  })

})

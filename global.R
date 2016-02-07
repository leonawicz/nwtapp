suppressMessages({
library(shiny)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(leaflet)
library(raster)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
})

options(warn = -1) # # haven't figured out how to suppress warning from colors(.)

load("nwt_data.RData")
load("nwt_data_pr_tas_CRU32_1961_1990_climatology.RData")
load("nwt_locations.RData")

r <- subset(cru6190$pr, 1)
lon <- (xmin(r)+xmax(r))/2
lat <- (ymin(r)+ymax(r))/2
decades <- seq(2010, 2090, by=10)
season.labels <- names(cru6190[[1]])[13:16]
season.labels.long <- season.labels[c(1,1,2,2,2,3,3,3,4,4,4,1)]
sea.idx <- list(Winter=c(1,2,12), Spring=3:5, Summer=6:8, Fall=9:11)
toy_list <- list(Season=season.labels, Month=month.abb)
rcps <- sort(unique(d$RCP))
rcp.labels <- c("RCP 4.5", "RCP 6.0", "RCP 8.5")
models <- unique(d$Model)
vars <- sort(unique(d$Var))
var.labels <- c("Precipitation", "Temperature")

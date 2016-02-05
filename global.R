suppressMessages({
library(shiny)
library(shinythemes)
library(shinyjs)
library(leaflet)
library(raster)
library(data.table)
library(dplyr)
library(tidyr)
})

options(warn = -1) # # haven't figured out how to suppress warning from colors(.)

load("nwt_data_pr_tas_monthly_decadal_means_2010_2099.Rdata")
load("nwt_data_pr_tas_CRU32_1961_1990_climatology.RData")

r <- subset(cru6190$pr, 1)
lon <- (xmin(r)+xmax(r))/2
lat <- (ymin(r)+ymax(r))/2
decades <- seq(2010, 2090, by=10)
season.labels <- names(cru6190[[1]])[13:16]
rcps <- unique(d$RCP)
models <- unique(d$Model)
vars <- sort(unique(d$Var))
var.labels <- c("Precipitation", "Temperature")

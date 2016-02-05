library(shiny)
library(shinythemes)
library(shinyjs)
library(leaflet)
library(raster)
library(data.table)
library(dplyr)
library(tidyr)

load("nwt_data_pr_tas_monthly_decadal_means_2010_2099.Rdata")

r <- subset(d$Data[[1]][[1]], 1)
lon <- (xmin(r)+xmax(r))/2
lat <- (ymin(r)+ymax(r))/2
decades <- seq(2010, 2090, by=10)
season.labels <- c("Dec-Feb", "Mar-May", "Jun-Aug", "Sep-Nov")
pal.names <- "pallete 1"

rcps <- unique(d$RCP)
models <- unique(d$Model)
vars <- sort(unique(d$Var))
var.labels <- c("Precipitation", "Temperature")

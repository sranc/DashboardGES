library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(echarts4r)
library(readxl)
library(tidyverse)
if (!requireNamespace("rnaturalearthhires", quietly = TRUE)) {
  devtools::install_github("ropensci/rnaturalearthhires")
}
library(leaflet)
library(sf)
library(jsonlite)
library(rnaturalearth)
library(rnaturalearthdata)

bolivia <<- ne_states(country = "Bolivia", returnclass = "sf")

source("functions/CNV.R")

source("functions/CyR_BD.R")
# carga de datos desde archivos excel
source("consultas/cargar_datos.R")
# consultas CC
source("consultas/CC.R")
# consultas SR
source("consultas/SR.R")
# consultas SR
source("consultas/UJ.R")
# consultas FC
source("consultas/FC.R")



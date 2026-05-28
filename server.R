library(shiny)
library(dplyr)
library(purrr)
library(lubridate)
library(httr2)
library(hubeau)
library(runner)
library(trend)
library(leaflet)
library(plotly)
library(DT)

source("assets/function/hydrologie/hydro_donnees.R")      # chargement API + stockage central
source("assets/function/hydrologie/hydro_carte.R")        # carte leaflet
source("assets/function/hydrologie/hydro_q90.R")          # tableau Q90 / Q50
source("assets/function/hydrologie/hydro_vcn.R")          # graphique VCN10 / VCN3
source("assets/function/hydrologie/hydro_tendances_q90.R") # tableau tendances durée sécheresse
source("assets/function/hydrologie/hydro_tendances_vcn.R") # tableau tendances VCN10

server <- function(input, output) {

  series_brutes   <- reactiveVal(NULL)
  surface_bv_data <- reactiveVal(NULL)
  seuils_stations <- reactiveVal(NULL)

  stations_dept <- hydro_donnees_server(input, series_brutes, surface_bv_data, seuils_stations)

  hydro_carte_server(input, output, stations_dept)
  hydro_q90_server(input, output, stations_dept, seuils_stations)
  hydro_vcn_server(input, output, stations_dept, series_brutes, surface_bv_data, seuils_stations)
  hydro_tendances_q90_server(input, output, stations_dept, series_brutes, seuils_stations)
  hydro_tendances_vcn_server(input, output, stations_dept, series_brutes, seuils_stations)

}
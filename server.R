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
library(renv)
library(DT)

 
source("assets/function/hydrologie/hydro_donnees.R")
source("assets/function/hydrologie/hydro_carte.R")
source("assets/function/hydrologie/hydro_q90.R")
source("assets/function/hydrologie/hydro_vcn.R")
source("assets/function/hydrologie/hydro_tendances_q90.R")
source("assets/function/hydrologie/hydro_tendances_vcn.R")
 
server <- function(input, output) {
 
  # ── Stockage central — tout est calculé une seule fois au chargement
  series_brutes   <- reactiveVal(NULL)
  surface_bv_data <- reactiveVal(NULL)
  seuils_stations <- reactiveVal(NULL)
  vcn_stations    <- reactiveVal(NULL)   # ← VCN pré-calculés
 
  stations_dept <- hydro_donnees_server(input, series_brutes, surface_bv_data, seuils_stations, vcn_stations)
 
  hydro_carte_server(input, output, stations_dept)
  hydro_q90_server(input, output, stations_dept, seuils_stations)
  hydro_vcn_server(input, output, stations_dept, series_brutes, surface_bv_data, seuils_stations, vcn_stations)
  hydro_tendances_q90_server(input, output, stations_dept, series_brutes, seuils_stations)
  hydro_tendances_vcn_server(input, output, stations_dept, series_brutes, seuils_stations, surface_bv_data, vcn_stations)
 
}
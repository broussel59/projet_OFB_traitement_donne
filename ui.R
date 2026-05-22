library(shiny)
library(leaflet)
library(plotly)
library(DT)

ui <- fluidPage(
  titlePanel("Hydrologiques"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dept", "Choisir un département :",
                  choices = setNames(sprintf("%02d", 1:95), sprintf("Département %02d", 1:95)),
                  selected = "1"),
      
      helpText("Analyse basée sur le Q90 et le VCN10."),
      actionButton("run_all", "Lancer la recherche départementale", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(id = "tabs",
        tabPanel("Carte des Stations", leafletOutput("map_france")),
        tabPanel("Q90/Q50", DTOutput("q90"),),
        tabPanel("VCN10/VCN3", plotlyOutput("vcn", height = "400px"),)
      )
    )
  )
)

# TODO voir pour creer un onglet independent pour une carte qui regrouperai tout les type de valeur étudier sur l'outil
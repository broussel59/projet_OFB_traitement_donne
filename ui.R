library(shiny)
library(leaflet)
library(plotly)
library(DT)

ui <- fluidPage(
  titlePanel("Hydrologiques"),

  sidebarLayout(
    sidebarPanel(
      selectInput("dept", "Choisir un département :",
        choices  = setNames(sprintf("%02d", 1:95), sprintf("Département %02d", 1:95)),
        selected = "59"
      ),
      helpText("Analyse basée sur le Q90 et le VCN10."),
      actionButton("run_all", "Lancer la recherche départementale", class = "btn-primary")
    ),

    mainPanel(
      tabsetPanel(id = "tabs",
        tabPanel("Carte des stations",   leafletOutput("map_france")),
        tabPanel("Q90 / Q50",            DTOutput("q90")),
        tabPanel("VCN10 / VCN3",         plotlyOutput("vcn", height = "400px")),
        tabPanel("Tendances Q90",        DTOutput("tendances_q90")),
        tabPanel("Tendances VCN10",      DTOutput("tendvcn"))
      )
    )
  )
)
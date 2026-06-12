library(shiny)
library(leaflet)
library(plotly)
library(DT)

addResourcePath("static", "www")

ui <- fluidPage(

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "static/style.css"),
    tags$link(rel = "icon", type = "image/png", href = "static/favicon.png")
  ),

  div(
    class = "header-bar",
    h1(class = "TitreAppli", "HydroTrends — Analyse des milieux aquatiques")
  ),

  tags$img(
    src   = "static/logo.png",
    alt   = "Logo OFB",
    style = "position:fixed; bottom:0; right:0; padding:10px; z-index:100; width:180px;"
  ),

  tags$img(
    src   = "static/filigrane.png",
    alt   = "filigrane",
    style = "position:fixed; bottom:0; right:0; padding:0; width:700px; z-index:-1; opacity:0.9;"
  ),

  sidebarLayout(

    sidebarPanel(
      width = 2,
      h2("Panneau de sélection"),

      selectInput("dept", "Département :",
        choices  = setNames(sprintf("%02d", 1:95), sprintf("Département %02d", 1:95)),
        selected = "02"
      ),

      helpText("Analyse hydrologique basée sur le Q90, Q50, VCN10 et VCN3."),

      actionButton("run_all", "Lancer la recherche", class = "btn-primary")
    ),

    mainPanel(
      width = 10,

      tabsetPanel(id = "tabs",

        tabPanel("Carte des stations",
          br(),
          leafletOutput("map_france", height = "500px")
        ),

        tabPanel("Q90 / Q50",
          br(),
          DTOutput("q90")
        ),

        tabPanel("VCN10 / VCN3",
          br(),
          plotlyOutput("vcn", height = "400px"),
          hr(),
          DTOutput("vcn_tableau")
        ),

        tabPanel("Tendances de sécheresse",
          br(),
          leafletOutput("carte_tend_q90", height = "400px"),
          hr(),
          DTOutput("tendances_q90")
        ),

        tabPanel("Tendances VCN",
          br(),
          leafletOutput("carte_tendvcn", height = "400px"),
          hr(),
          DTOutput("tendvcn")
        )

      )
    )
  )
)
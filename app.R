# ── Point d'entrée ────────────────────────────────────────────────
# Pour lancer : shiny::runApp()

library(shiny)

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
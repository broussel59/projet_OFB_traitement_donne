# lancer le code shiny::runApp()
library(DT)
library(shiny)
library(httr2)
library(plotly)
library(ggplot2)
library(dplyr)
library(leaflet)
library(hubeau)
library(purrr)
library(lubridate)

server <- function(input, output) {

  stations_dept <- eventReactive(input$run_all, {
    req <- request("https://hubeau.eaufrance.fr/api/v2/hydrometrie/referentiel/stations") %>%
      req_url_query(code_departement = input$dept, en_service = TRUE, format = "json")

    tryCatch({
      resp <- req %>% req_perform()
      return(resp_body_json(resp, simplifyVector = TRUE)$data)
    }, error = function(e) return(NULL))
  })


  output$map_france <- renderLeaflet({
    data <- stations_dept()

    m <- leaflet() %>% addTiles()

    if (!is.null(data) && nrow(data) > 0) {
      m <- m %>% addCircleMarkers(
        lng         = as.numeric(data$longitude_station),
        lat         = as.numeric(data$latitude_station),
        radius      = 5,
        color       = "blue",
        fillOpacity = 0.7,
        popup       = paste("Station :", data$libelle_station)
      )
    }

    m
  })

  get_serie_hydro <- function(code_station, date_debut, date_fin, param) {
    get_hydrometrie_obs_elab(
      code_entite         = code_station,
      date_debut_obs_elab = date_debut,
      date_fin_obs_elab   = date_fin,
      grandeur_hydro_elab = param
    ) %>%
      select(code_station:resultat_obs_elab) %>%
      mutate(
        annee = ymd(date_obs_elab),
        annee = year(annee)
      )
  }

  series_stations_tot <- function(hydro_stations, date_debut, date_fin, param) {
    get_serie_hydro_possible <- possibly(get_serie_hydro, otherwise = NULL)

    map(
      .x = hydro_stations,
      .f = function(x) {
        get_serie_hydro_possible(
          code_station = x,
          date_debut   = date_debut,
          date_fin     = date_fin,
          param        = param
        )
      }
    )
  }

  # Toutes les séries brutes, accessibles partout dans le server
  series_brutes <- reactiveVal(NULL)

  observeEvent(stations_dept(), {
    stations <- stations_dept()
    req(stations)

    date_debut <- "2000-01-01"
    date_fin   <- paste0(year(Sys.Date()) - 1L, "-12-31")

    withProgress(message = "Chargement des données...", value = 0, {

      series <- series_stations_tot(
        hydro_stations = stations$code_station,
        date_debut     = date_debut,
        date_fin       = date_fin,
        param          = "QmnJ"
      )

      setProgress(1, message = "Terminé")
    })

    # Stocke une liste nommée : nom de station → données brutes
    names(series) <- stations$code_station
    series_brutes(series)
  })


  output$q90 <- renderDT({
    series   <- series_brutes()
    stations <- stations_dept()
    req(series)

    resultats <- map2(series, seq_along(series), function(df, i) {

      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

      debits <- as.numeric(df$resultat_obs_elab)
      debits <- debits[!is.na(debits) & debits >= 0]
      if (length(debits) < 30) return(NULL)

      if (median(debits) > 4000) debits <- debits / 1000

      data.frame(
        `Nom de la station` = stations$libelle_station[i],
        `Code station`      = stations$code_station[i],
        `Q90 (m³/s)`        = round(quantile(debits, 0.10), 3),
        `Q50 (m³/s)`        = round(quantile(debits, 0.50), 3),
        check.names = FALSE
      )
    })

    tableau <- bind_rows(resultats)
    validate(need(nrow(tableau) > 0, "Aucune donnée trouvée."))
    datatable(tableau, rownames = FALSE)
  })

}
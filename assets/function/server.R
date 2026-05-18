# lancer le code shiny::runApp()
library(DT)
library(shiny)
library(httr2)
library(plotly)
library(ggplot2)
library(dplyr)
library(leaflet)

server <- function(input, output) {
  
  stations_dept <- eventReactive(input$run_all, {
    req <- request("https://hubeau.eaufrance.fr/api/v2/hydrometrie/referentiel/stations") %>%
      req_url_query(code_departement = input$dept, en_service = TRUE, format = "json")
    
    tryCatch({
      resp <- req %>% req_perform()
      return(resp_body_json(resp, simplifyVector = TRUE)$data)
    }, error = function(e) return(NULL))
  })


  station_value <- function(station_nb){
    req <- request("https://hubeau.eaufrance.fr/api/v2/hydrometrie/obs_elab") %>%
      req_url_query(code_station = station_nb, format = "json")
    tryCatch({
      resp <- req %>% req_perform()
      return(resp_body_json(resp, simplifyVector = TRUE)$data)
    }, error = function(e) return(NULL))
  }


output$map_france <- renderLeaflet({
  data <- stations_dept()
  
  m <- leaflet() %>% addTiles()
  
  if (!is.null(data) && nrow(data) > 0) {
    m <- m %>% addCircleMarkers(
      lng = as.numeric(data$longitude_station), 
      lat = as.numeric(data$latitude_station),
      radius = 5, 
      # if ( 1==1 ) {
         color = "blue", 
      # }
      fillOpacity = 0.7,
      popup = paste("Station :", data$libelle_station)
    )
  }
  
  m
})

output$q90 <- renderDT({

  # 1. Récupère la liste des stations
  stations <- stations_dept()
  req(stations)

  # 2. Période d'analyse fixe
  date_debut <- "2000-01-01"
  date_fin   <- paste0(as.integer(format(Sys.Date(), "%Y")) - 1L, "-12-31")

  # 3. Tableau vide qui va se remplir au fil des stations
  resultats <- list()

  # 4. Boucle sur chaque station avec barre de progression
  withProgress(message = "Chargement...", value = 0, {
    for (i in seq_len(nrow(stations))) {

      # Avance la barre de progression
      incProgress(1 / nrow(stations),
        message = paste("Station", i, "/", nrow(stations)),
        detail  = stations$libelle_station[i]
      )

      # 5. Appel API pour cette station
      reponse <- tryCatch(
        request("https://hubeau.eaufrance.fr/api/v2/hydrometrie/obs_elab") %>%
          req_url_query(
            code_entite         = stations$code_station[i],
            date_debut_obs_elab = date_debut,
            date_fin_obs_elab   = date_fin,
            size                = 20000,
            format              = "json"
          ) %>%
          req_perform() %>%
          resp_body_json(simplifyVector = TRUE),
        error = function(e) { message("ERREUR : ", e$message) ; NULL }
      )

      # 6. Passe à la station suivante si pas de données
      if (is.null(reponse$data) || nrow(reponse$data) == 0) next
      # message("Grandeurs : ", paste(unique(reponse$data$grandeur_hydro), collapse = ", "))

      # Filtre en R — garde uniquement les débits moyens journaliers
      donnees <- reponse$data[reponse$data$grandeur_hydro_elab == "QmnJ", ]
      if (nrow(donnees) == 0) next

      # 7. Extrait les débits journaliers
      debits <- as.numeric(reponse$data$resultat_obs)
      debits <- debits[!is.na(debits) & debits >= 0]
      if (length(debits) < 3650) next       # pas assez de données : on passe

      # message("Station : ", stations$libelle_station[i])
      # message("Nb valeurs : ", length(debits))
      # message("Min : ", min(debits), " | Max : ", max(debits))
      # message("Q90 brut : ", quantile(debits, 0.10))
      # message("Q50 brut : ", quantile(debits, 0.50))

      # Si la médiane dépasse 100 000, les valeurs sont en L/s → conversion
      # Sinon elles sont déjà en m³/s (cas rare mais existant sur Hub'Eau)
      if (!is.na(median(debits)) && median(debits) > 100000) {
        debits <- debits / 1000
      }
      # 8. Calcule Q90, Q50 et le nombre de jours sous le Q90
      q90 <- quantile(debits, 0.10)   # Q90 = dépassé 90% du temps
      q50 <- quantile(debits, 0.50)    # Q50 = médiane

      # 9. Ajoute une ligne au tableau
      resultats[[i]] <- data.frame(
        `Nom de la station` = stations$libelle_station[i],
        `Code station`      = stations$code_station[i],
        `Q90 (L/s)`        = round(q90, 3),
        `Q50 (L/s)`        = round(q50, 3),
        check.names = FALSE
      )
    }
  })

  # 10. Assemble toutes les lignes et affiche le tableau
  tableau <- bind_rows(resultats)
  validate(need(nrow(tableau) > 0, "Aucune donnée trouvée."))
  datatable(tableau, rownames = FALSE)
})

  # output$graph_vcn10
}

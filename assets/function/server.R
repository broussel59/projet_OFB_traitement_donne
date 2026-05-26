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
library(trend)
library(lubridate)
library(runner)

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

  VCN3_1sta <- function (vecteur_debits_spe, vecteur_dates, jours_glissants_2, code_station) {
  
  dates<-sort(vecteur_dates, decreasing = FALSE)
  
  VCN3<-
    mean_run(vecteur_debits_spe,   # calcul d'une moyenne glissante des débits spécifiques sur le nombre de jours choisis  
             k = jours_glissants_2, 
             idx = dates
    )
  
  VCN3<-data.frame(VCNx_spe = VCN3, annee=substr(dates, 1,4), jours_glissants_2 = rep(jours_glissants_2, each=length(VCN3)),
                   code_sta = rep(code_station, each=length(VCN3))) # création d'un data frame avec les moyennes glissantes, les dates et les années 
  
  VCN3<-VCN3 %>% 
    dplyr::group_by(annee, jours_glissants_2, code_sta) %>% 
    dplyr::summarise(VCN3_annuel_spe=min(VCNx_spe), .groups = "drop") # calcul du minimum par années des moyennes glissantes -> VCN10 annuel 
  
  return(VCN3)
}  

VCNx_1sta <- function (vecteur_debits_spe, vecteur_dates, jours_glissants, code_station) {
  
  dates<-sort(vecteur_dates, decreasing = FALSE)
  
  VCNx<-
    mean_run(vecteur_debits_spe,   # calcul d'une moyenne glissante des débits spécifiques sur le nombre de jours choisis  
             k = jours_glissants, 
             idx = dates
    )
  
  VCNx<-data.frame(VCNx_spe = VCNx, annee=substr(dates, 1,4), jours_glissants = rep(jours_glissants, each=length(VCNx)),
                   code_sta = rep(code_station, each=length(VCNx))) # création d'un data frame avec les moyennes glissantes, les dates et les années 
  
  VCNx<-VCNx %>% 
    dplyr::group_by(annee, jours_glissants, code_sta) %>% 
    dplyr::summarise(VCNx_annuel_spe=min(VCNx_spe), .groups = "drop") # calcul du minimum par années des moyennes glissantes -> VCN10 annuel 
  
  return(VCNx)
}  

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
  surface_bv_data <- reactiveVal(NULL)
  seuils_stations <- reactiveVal(NULL) 

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
    
     codes_sites <- get_hydrometrie_stations(code_departement = input$dept) %>%
      filter(en_service == TRUE) %>%
      select(code_site, code_station)
    
    # Calcul Q90 / Q50 par station sur toute la période
    seuils <- map2(series, stations$code_station, function(df, code) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
      debits <- as.numeric(df$resultat_obs_elab)
      debits <- debits[!is.na(debits) & debits >= 0]
      if (length(debits) < 3650) return(NULL)
      if (median(debits) > 1800) debits <- debits / 1000
      list(q90 = quantile(debits, 0.10), q50 = quantile(debits, 0.50))
    })
    names(seuils) <- stations$code_station
    seuils_stations(seuils)

    surface_bv <- get_hydrometrie_sites(code_departement = input$dept) %>%
      select(code_site, surface_bv) %>%
      right_join(codes_sites, by = "code_site")

    surface_bv_data(surface_bv) 
      
  })


  output$q90 <- renderDT({
    series   <- series_brutes()
    seuils   <- seuils_stations()
    stations <- stations_dept()
    req(series, seuils)

    resultats <- map2(seuils, seq_along(seuils), function(s, i) {
      if (is.null(s)) return(NULL)
      data.frame(
        `Nom de la station` = stations$libelle_station[i],
        `Code station`      = stations$code_station[i],
        `Q90 (m³/s)`        = round(s$q90, 3),
        `Q50 (m³/s)`        = round(s$q50, 3),
        check.names = FALSE
      )
    })

    tableau <- bind_rows(resultats)
    validate(need(nrow(tableau) > 0, "Aucune donnée trouvée."))
    datatable(tableau, rownames = FALSE)
  })

   output$vcn <- renderPlotly({
    series   <- series_brutes()
    stations <- stations_dept()
    surface  <- surface_bv_data()
    req(series, surface)

    # Calcul VCN10 et VCN3 pour chaque station
    tous_vcn10 <- map2(series, seq_along(series), function(df, i) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
      debits <- as.numeric(df$resultat_obs_elab)
      dates  <- as.Date(df$date_obs_elab)
      valides <- !is.na(debits) & debits >= 0 & !is.na(dates)
      debits <- debits[valides] ; dates <- dates[valides]
      if (length(debits) < 30) return(NULL)
      if (median(debits) > 1800) debits <- debits / 1000
      tryCatch(VCNx_1sta(debits, dates, 10, stations$code_station[i]), error = function(e) NULL)
    })

    tous_vcn3 <- map2(series, seq_along(series), function(df, i) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
      debits <- as.numeric(df$resultat_obs_elab)
      dates  <- as.Date(df$date_obs_elab)
      valides <- !is.na(debits) & debits >= 0 & !is.na(dates)
      debits <- debits[valides] ; dates <- dates[valides]
      if (length(debits) < 30) return(NULL)
      if (median(debits) > 1800) debits <- debits / 1000
      tryCatch(VCN3_1sta(debits, dates, 3, stations$code_station[i]), error = function(e) NULL)
    })

    # mise en forme avec surface BV pour les deux indicateurs
    df_vcn10 <- bind_rows(tous_vcn10) %>%
      left_join(surface %>% select(code_station, surface_bv), by = c("code_sta" = "code_station")) %>%
      mutate(spe = VCNx_annuel_spe / surface_bv) %>%
      group_by(annee) %>%
      summarise(mediane = median(spe, na.rm = TRUE), .groups = "drop") %>%
      mutate(annee = as.numeric(annee))

    df_vcn3 <- bind_rows(tous_vcn3) %>%
      left_join(surface %>% select(code_station, surface_bv), by = c("code_sta" = "code_station")) %>%
      mutate(spe = VCN3_annuel_spe / surface_bv) %>%
      group_by(annee) %>%
      summarise(mediane = median(spe, na.rm = TRUE), .groups = "drop") %>%
      mutate(annee = as.numeric(annee))

    validate(need(nrow(df_vcn10) > 0, "Aucune donnée trouvée."))

    df_vcn10$tendance <- predict(lm(mediane ~ annee, data = df_vcn10))
    df_vcn3$tendance  <- predict(lm(mediane ~ annee, data = df_vcn3))

    plot_ly(x = ~df_vcn10$annee) %>%
      add_bars(y = ~df_vcn10$mediane, name = "VCN10",
               marker = list(color = "darkblue", line = list(color = "grey40", width = 1))) %>%
      add_bars(y = ~df_vcn3$mediane,  name = "VCN3",
               marker = list(color = "deepskyblue", line = list(color = "steelblue4", width = 1))) %>%
      add_lines(y = ~df_vcn10$tendance, name = "Tendance VCN10",
                line = list(color = "red",    dash = "dash", width = 2)) %>%
      add_lines(y = ~df_vcn3$tendance,  name = "Tendance VCN3",
                line = list(color = "lime", dash = "dash", width = 2)) %>%
      layout(
        barmode   = "group",
        xaxis     = list(title = "Année", dtick = 5),
        yaxis     = list(title = "Médiane annuelle (m³/s/km²)"),
        legend    = list(x = 0.75, y = 0.95),
        hovermode = "x unified"
      )
  })

  # Ajout dans ui.R :
# tabPanel("Tendances Q90/Q50", DTOutput("tendances_q90"))

# Ajout en haut de server.R :
# library(trend)

# Bloc à ajouter dans server.R avant la dernière }

output$tendances_q90 <- renderDT({ 
    series   <- series_brutes()
    seuils   <- seuils_stations()   # ← Q90 déjà calculés, pas besoin de recalculer
    stations <- stations_dept()
    req(series, seuils)
 
    tester_mk <- function(valeurs) {
      if (length(valeurs) < 4) return(NULL)
      tryCatch(
        list(mk = mk.test(valeurs), slope = sens.slope(valeurs)),
        error = function(e) NULL
      )
    }
 
    interpreter <- function(res) {
      if (is.na(res$mk$p.value) || res$mk$p.value > 0.05 || res$slope$estimates == 0) return("Pas de tendance")
      if (res$slope$estimates > 0) return("Dégradation")   
      if (res$slope$estimates < 0) return("Amélioration")
    }
 
    resultats <- map2(series, seq_along(series), function(df, i) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
 
      code <- stations$code_station[i]
      s    <- seuils[[code]]
      if (is.null(s)) return(NULL)
 
      df <- df %>%
        mutate(debit = as.numeric(resultat_obs_elab)) %>%
        filter(!is.na(debit), debit >= 0)
 
      if (nrow(df) < 30) return(NULL)
      if (median(df$debit) > 1800) df$debit <- df$debit / 1000
 
      # Nombre de jours sous le Q90 par année (durée de sécheresse annuelle)
      duree_sech <- df %>%
        group_by(annee) %>%
        summarise(duree = sum(debit < s$q90), .groups = "drop") %>%
        arrange(annee)
 
      res <- tester_mk(duree_sech$duree)
      if (is.null(res)) return(NULL)
 
      data.frame(
        `Nom de la station` = stations$libelle_station[i],
        `Code station`      = code,
        `P-value`           = round(res$mk$p.value, 10),
        `Sens de la pente`  = round(res$slope$estimates, 5),   # jours/an
        `Tendance`          = interpreter(res),
        check.names = FALSE
      )
    })
 
    tableau <- bind_rows(resultats) %>%
      arrange(factor(Tendance, levels = c("Dégradation", "Pas de tendance", "Amélioration")))
 
    validate(need(nrow(tableau) > 0, "Aucune donnée trouvée."))
 
    couleurs <- c("Dégradation" = "#ffcdd2", "Pas de tendance" = "#fff9c4", "Amélioration" = "#c8e6c9")
 
    datatable(tableau, rownames = FALSE) %>%
      formatStyle("Tendance", backgroundColor = styleEqual(names(couleurs), couleurs))
  })

}
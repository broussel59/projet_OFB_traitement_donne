# ── Module : carte + tableau tendances durée sécheresse Q90/Q50 ───

hydro_tendances_q90_server <- function(input, output, stations_dept, series_brutes, seuils_stations) {

  tester_mk <- function(valeurs) {
    if (length(valeurs) < 4) return(NULL)
    tryCatch(
      list(mk = mk.test(valeurs), slope = sens.slope(valeurs)),
      error = function(e) NULL
    )
  }

  # Plus de jours sous le seuil = dégradation
  interpreter <- function(res) {
    if (is.na(res$mk$p.value) || res$mk$p.value > 0.05 || res$slope$estimates == 0) return("Pas de tendance")
    if (res$slope$estimates > 0) return("Dégradation")
    return("Amélioration")
  }

  couleur_tendance <- function(tendance) {
    case_when(
      tendance == "Dégradation"  ~ "red",
      tendance == "Amélioration" ~ "green",
      TRUE                       ~ "orange"
    )
  }

  # ── Calcul partagé entre carte et tableau ─────────────────────────
  tendances_q90_calculees <- reactive({
    series   <- series_brutes()
    seuils   <- seuils_stations()
    stations <- stations_dept()
    req(series, seuils)

    resultats <- map2(series, seq_along(series), function(df, i) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

      code <- stations$code_station[i]
      s    <- seuils[[code]]
      if (is.null(s)) return(NULL)

      df <- df %>%
        mutate(debit = as.numeric(resultat_obs_elab)) %>%
        filter(!is.na(debit), debit >= 0)

      if (nrow(df) < 30) return(NULL)
      if (median(df$debit) > 2) df$debit <- df$debit / 1000

      duree_q90 <- df %>%
        group_by(annee) %>%
        summarise(duree = sum(debit < s$q90), .groups = "drop") %>%
        arrange(annee)

      duree_q50 <- df %>%
        group_by(annee) %>%
        summarise(duree = sum(debit < s$q50), .groups = "drop") %>%
        arrange(annee)

      res_q90 <- tester_mk(duree_q90$duree)
      res_q50 <- tester_mk(duree_q50$duree)
      if (is.null(res_q90) || is.null(res_q50)) return(NULL)

      data.frame(
        `Nom de la station` = stations$libelle_station[i],
        `Code station`      = code,
        lng                 = as.numeric(stations$longitude_station[i]),
        lat                 = as.numeric(stations$latitude_station[i]),
        `P-value Q50`       = round(res_q50$mk$p.value, 10),
        `Pente Q50 (j/an)`  = round(res_q50$slope$estimates, 5),
        `Tendance Q50`      = interpreter(res_q50),
        `P-value Q90`       = round(res_q90$mk$p.value, 10),
        `Pente Q90 (j/an)`  = round(res_q90$slope$estimates, 5),
        `Tendance Q90`      = interpreter(res_q90),
        check.names = FALSE
      )
    })

    bind_rows(resultats)
  })


  # ── Carte ─────────────────────────────────────────────────────────
  output$carte_tend_q90 <- renderLeaflet({
    t <- tendances_q90_calculees()
    req(nrow(t) > 0)
    leaflet(t) %>% 
    addTiles() %>%
    addCircleMarkers(
        data        = t,
        lng         = ~lng,
        lat         = ~lat,
        radius      = 5,
        color       = ~couleur_tendance(`Tendance Q90`),
        fillOpacity = 0.7,
        stroke      = FALSE,
        popup       = ~paste0(
          "<b>", `Nom de la station`, "</b><br>",
          "Q90 : ", `Tendance Q90`, "<br>",
          "Q50 : ", `Tendance Q50`
        )
      ) %>%
      addLegend(
        position = "bottomright",
        colors   = c("red", "orange", "green"),
        labels   = c("Dégradation", "Pas de tendance", "Amélioration"),
        title    = "Tendance Q90"
      )
  })

  observeEvent(tendances_q90_calculees(), {
    t <- tendances_q90_calculees()
    req(nrow(t) > 0)

    leafletProxy("carte_tend_q90") %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        data        = t,
        lng         = ~lng,
        lat         = ~lat,
        radius      = 5,
        color       = ~couleur_tendance(`Tendance Q90`),
        fillOpacity = 0.7,
        stroke      = FALSE,
        popup       = ~paste0(
          "<b>", `Nom de la station`, "</b><br>",
          "Q90 : ", `Tendance Q90`, "<br>",
          "Q50 : ", `Tendance Q50`
        )
      ) %>%
      addLegend(
        position = "bottomright",
        colors   = c("red", "orange", "green"),
        labels   = c("Dégradation", "Pas de tendance", "Amélioration"),
        title    = "Tendance Q90"
      )
  })


  # ── Tableau ───────────────────────────────────────────────────────
  output$tendances_q90 <- renderDT({
    t <- tendances_q90_calculees()
    validate(need(nrow(t) > 0, "Aucune donnée trouvée."))

    tableau  <- t %>% select(-lng, -lat)
    couleurs <- c("Dégradation" = "#ffcdd2", "Pas de tendance" = "#fff9c4", "Amélioration" = "#c8e6c9")

    datatable(tableau, rownames = FALSE) %>%
      formatStyle("Tendance Q50", backgroundColor = styleEqual(names(couleurs), couleurs)) %>%
      formatStyle("Tendance Q90", backgroundColor = styleEqual(names(couleurs), couleurs))
  })

}
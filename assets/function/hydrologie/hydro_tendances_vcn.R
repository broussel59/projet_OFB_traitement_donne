# ── Module : carte + tableau tendances VCN10 / VCN3 (MK) ──────────

hydro_tendances_vcn_server <- function(input, output, stations_dept, series_brutes, seuils_stations, surface_bv_data, vcn_stations) {

  tester_mk <- function(valeurs) {
    if (length(valeurs) < 4) return(NULL)
    tryCatch(
      list(mk = mk.test(valeurs), slope = sens.slope(valeurs)),
      error = function(e) NULL
    )
  }

  # VCN baisse = moins d'eau = dégradation
  interpreter <- function(res) {
    if (is.na(res$mk$p.value) || res$mk$p.value > 0.05 || res$slope$estimates == 0) return("Pas de tendance")
    if (res$slope$estimates < 0) return("Dégradation")
    return("Amélioration")
  }

  # ── Calcul MK partagé entre carte et tableau ──────────────────────
  tendances_calculees <- reactive({
    vcn      <- vcn_stations()   # ← lecture du cache
    stations <- stations_dept()
    surface  <- surface_bv_data()
    req(vcn, surface)

    resultats <- map2(vcn, seq_along(vcn), function(v, i) {
      if (is.null(v) || is.null(v$vcn10) || is.null(v$vcn3)) return(NULL)

      code <- stations$code_station[i]
      surf <- surface %>% filter(code_station == code) %>% pull(surface_bv)
      if (length(surf) == 0 || is.na(surf) || surf <= 0) return(NULL)

      if (nrow(v$vcn10) < 4) return(NULL)

      vcn10 <- v$vcn10 %>% mutate(spe = VCNx_annuel_spe / surf)
      vcn3  <- v$vcn3  %>% mutate(spe = VCN3_annuel_spe / surf)

      res10 <- tester_mk(vcn10$spe)
      res3  <- tester_mk(vcn3$spe)
      if (is.null(res10) || is.null(res3)) return(NULL)

      data.frame(
        `Nom de la station` = stations$libelle_station[i],
        `Code station`      = code,
        lng                 = as.numeric(stations$longitude_station[i]),
        lat                 = as.numeric(stations$latitude_station[i]),
        `P-value VCN3`      = round(res3$mk$p.value,      10),
        `Pente VCN3`        = round(res3$slope$estimates,  5),
        `Tendance VCN3`     = interpreter(res3),
        `P-value VCN10`     = round(res10$mk$p.value,     10),
        `Pente VCN10`       = round(res10$slope$estimates, 5),
        `Tendance VCN10`    = interpreter(res10),
        check.names = FALSE
      )
    })

    bind_rows(resultats)
  })


  # ── Carte ─────────────────────────────────────────────────────────
  couleur_tendance <- function(tendance) {
    case_when(
      tendance == "Dégradation"  ~ "red",
      tendance == "Amélioration" ~ "green",
      TRUE                       ~ "orange"
    )
  }

  output$carte_tendvcn <- renderLeaflet({
    t <- tendances_calculees()
    validate(need(nrow(t) > 0, "Aucune donnée trouvée."))

    leaflet(t) %>%
      addTiles() %>%
      addCircleMarkers(
        lng         = ~lng,
        lat         = ~lat,
        radius      = 5,
        color       = ~couleur_tendance(`Tendance VCN10`),
        fillOpacity = 0.7,
        stroke      = FALSE,
        popup       = ~paste0(
          "<b>", `Nom de la station`, "</b><br>",
          "VCN10 : ", `Tendance VCN10`, "<br>",
          "VCN3 : ",  `Tendance VCN3`
        )
      ) %>%
      addLegend(
        position = "bottomright",
        colors   = c("red", "orange", "green"),
        labels   = c("Dégradation", "Pas de tendance", "Amélioration"),
        title    = "Tendance VCN10"
      )
  })
  observeEvent(tendances_calculees(), {
  t <- tendances_calculees()
  req(nrow(t) > 0)

  leafletProxy("carte_tendvcn") %>%
    clearMarkers() %>%
    clearControls() %>%                   # ← efface aussi la légende
    addCircleMarkers(
      data        = t,
      lng         = ~lng,
      lat         = ~lat,
      radius      = 5,
      color       = ~couleur_tendance(`Tendance VCN10`),
      fillOpacity = 0.7,
      stroke      = FALSE,
      popup       = ~paste0(
        "<b>", `Nom de la station`, "</b><br>",
        "VCN10 : ", `Tendance VCN10`, "<br>",
        "VCN3 : ",  `Tendance VCN3`
      )
    ) %>%
    addLegend(
      position = "bottomright",
      colors   = c("red", "orange", "green"),
      labels   = c("Dégradation", "Pas de tendance", "Amélioration"),
      title    = "Tendance VCN10"
    )
})


  # ── Tableau ───────────────────────────────────────────────────────
  output$tendvcn <- renderDT({
    t <- tendances_calculees()
    validate(need(nrow(t) > 0, "Aucune donnée trouvée."))

    tableau  <- t %>% select(-lng, -lat)
    couleurs <- c("Dégradation" = "#ffcdd2", "Pas de tendance" = "#fff9c4", "Amélioration" = "#c8e6c9")

    datatable(tableau, rownames = FALSE) %>%
      formatStyle("Tendance VCN3",  backgroundColor = styleEqual(names(couleurs), couleurs)) %>%
      formatStyle("Tendance VCN10", backgroundColor = styleEqual(names(couleurs), couleurs))
  })

}
# ── Module : graphique + tableau VCN10 / VCN3 ─────────────────────

VCNx_1sta <- function(vecteur_debits_spe, vecteur_dates, jours_glissants, code_station) {
  dates <- sort(vecteur_dates, decreasing = FALSE)
  VCNx  <- mean_run(vecteur_debits_spe, k = jours_glissants, idx = dates)
  data.frame(
    VCNx_spe        = VCNx,
    annee           = substr(dates, 1, 4),
    jours_glissants = rep(jours_glissants, each = length(VCNx)),
    code_sta        = rep(code_station,    each = length(VCNx))
  ) %>%
    group_by(annee, jours_glissants, code_sta) %>%
    summarise(VCNx_annuel_spe = min(VCNx_spe), .groups = "drop")
}

VCN3_1sta <- function(vecteur_debits_spe, vecteur_dates, jours_glissants_2, code_station) {
  dates <- sort(vecteur_dates, decreasing = FALSE)
  VCN3  <- mean_run(vecteur_debits_spe, k = jours_glissants_2, idx = dates)
  data.frame(
    VCNx_spe          = VCN3,
    annee             = substr(dates, 1, 4),
    jours_glissants_2 = rep(jours_glissants_2, each = length(VCN3)),
    code_sta          = rep(code_station,       each = length(VCN3))
  ) %>%
    group_by(annee, jours_glissants_2, code_sta) %>%
    summarise(VCN3_annuel_spe = min(VCNx_spe), .groups = "drop")
}


hydro_vcn_server <- function(input, output, stations_dept, series_brutes, surface_bv_data, seuils_stations, vcn_stations) {

  # ── Graphique ─────────────────────────────────────────────────────
  output$vcn <- renderPlotly({
    vcn     <- vcn_stations()    # ← lecture du cache
    surface <- surface_bv_data()
    req(vcn, surface)

    # Agrège par année toutes les stations (médiane des débits spécifiques)
    prep_annuel <- function(col_vcn, col_val) {
      bind_rows(map2(vcn, names(vcn), function(v, code) {
        if (is.null(v) || is.null(v[[col_vcn]])) return(NULL)
        surf <- surface %>% filter(code_station == code) %>% pull(surface_bv)
        if (length(surf) == 0 || is.na(surf) || surf <= 0) return(NULL)
        v[[col_vcn]] %>% mutate(spe = .data[[col_val]] / surf)
      })) %>%
        group_by(annee) %>%
        summarise(mediane = median(spe, na.rm = TRUE), .groups = "drop") %>%
        mutate(annee = as.numeric(annee))
    }

    df10 <- prep_annuel("vcn10", "VCNx_annuel_spe")
    df3  <- prep_annuel("vcn3",  "VCN3_annuel_spe")
    validate(need(nrow(df10) > 0, "Aucune donnée trouvée."))

    df10$tendance <- predict(lm(mediane ~ annee, data = df10))
    df3$tendance  <- predict(lm(mediane ~ annee, data = df3))

    plot_ly(x = ~df10$annee) %>%
      add_bars(y = ~df10$mediane, name = "VCN10",
               marker = list(color = "darkblue",    line = list(color = "grey40",     width = 1))) %>%
      add_bars(y = ~df3$mediane,  name = "VCN3",
               marker = list(color = "deepskyblue", line = list(color = "steelblue4", width = 1))) %>%
      add_lines(y = ~df10$tendance, name = "Tendance VCN10",
                line = list(color = "red",  dash = "dash", width = 2)) %>%
      add_lines(y = ~df3$tendance,  name = "Tendance VCN3",
                line = list(color = "lime", dash = "dash", width = 2)) %>%
      layout(
        barmode   = "group",
        xaxis     = list(title = "Année", dtick = 5),
        yaxis     = list(title = "Médiane annuelle (m³/s/km²)"),
        legend    = list(x = 0.75, y = 0.95),
        hovermode = "x unified"
      )
  })


  # ── Tableau par station ───────────────────────────────────────────
  output$vcn_tableau <- renderDT({
    vcn      <- vcn_stations()   # ← lecture du cache
    stations <- stations_dept()
    surface  <- surface_bv_data()
    req(vcn, surface)

    resultats <- map2(vcn, seq_along(vcn), function(v, i) {
      if (is.null(v) || is.null(v$vcn10) || is.null(v$vcn3)) return(NULL)
      code <- stations$code_station[i]
      surf <- surface %>% filter(code_station == code) %>% pull(surface_bv)
      if (length(surf) == 0 || is.na(surf) || surf <= 0) return(NULL)

      data.frame(
        `Nom de la station`       = stations$libelle_station[i],
        `Code station`            = code,
        `VCN10 médian (m³/s/km²)` = round(median(v$vcn10$VCNx_annuel_spe / surf, na.rm = TRUE), 4),
        `VCN3 médian (m³/s/km²)`  = round(median(v$vcn3$VCN3_annuel_spe  / surf, na.rm = TRUE), 4),
        check.names = FALSE
      )
    })

    tableau <- bind_rows(resultats)
    validate(need(nrow(tableau) > 0, "Aucune donnée trouvée."))
    datatable(tableau, rownames = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: top; font-weight: bold;",
        "VCN10 et VCN3 médians par station — 2000 à aujourd'hui"
      )
    )
  })

}
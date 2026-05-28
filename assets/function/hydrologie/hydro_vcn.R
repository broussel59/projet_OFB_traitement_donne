# ── Module : graphique VCN10 / VCN3 ──────────────────────────────

# Fonctions de calcul VCN (inchangées)
VCNx_1sta <- function(vecteur_debits_spe, vecteur_dates, jours_glissants, code_station) {
  dates <- sort(vecteur_dates, decreasing = FALSE)
  VCNx  <- mean_run(vecteur_debits_spe, k = jours_glissants, idx = dates)
  VCNx  <- data.frame(
    VCNx_spe = VCNx, annee = substr(dates, 1, 4),
    jours_glissants = rep(jours_glissants, each = length(VCNx)),
    code_sta = rep(code_station, each = length(VCNx))
  )
  VCNx %>%
    group_by(annee, jours_glissants, code_sta) %>%
    summarise(VCNx_annuel_spe = min(VCNx_spe), .groups = "drop")
}

VCN3_1sta <- function(vecteur_debits_spe, vecteur_dates, jours_glissants_2, code_station) {
  dates <- sort(vecteur_dates, decreasing = FALSE)
  VCN3  <- mean_run(vecteur_debits_spe, k = jours_glissants_2, idx = dates)
  VCN3  <- data.frame(
    VCNx_spe = VCN3, annee = substr(dates, 1, 4),
    jours_glissants_2 = rep(jours_glissants_2, each = length(VCN3)),
    code_sta = rep(code_station, each = length(VCN3))
  )
  VCN3 %>%
    group_by(annee, jours_glissants_2, code_sta) %>%
    summarise(VCN3_annuel_spe = min(VCNx_spe), .groups = "drop")
}


hydro_vcn_server <- function(input, output, stations_dept, series_brutes, surface_bv_data, seuils_stations) {

  output$vcn <- renderPlotly({
    series   <- series_brutes()
    stations <- stations_dept()
    surface  <- surface_bv_data()
    req(series, surface)

    # Calcul VCN10 et VCN3 pour chaque station
    prep_debits <- function(df) {
      debits <- as.numeric(df$resultat_obs_elab)
      dates  <- as.Date(df$date_obs_elab)
      valides <- !is.na(debits) & debits >= 0 & !is.na(dates)
      debits <- debits[valides] ; dates <- dates[valides]
      if (length(debits) < 30) return(NULL)
      if (median(debits) > 1800) debits <- debits / 1000
      list(debits = debits, dates = dates)
    }

    tous_vcn10 <- map2(series, seq_along(series), function(df, i) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
      d <- prep_debits(df) ; if (is.null(d)) return(NULL)
      tryCatch(VCNx_1sta(d$debits, d$dates, 10, stations$code_station[i]), error = function(e) NULL)
    })

    tous_vcn3 <- map2(series, seq_along(series), function(df, i) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
      d <- prep_debits(df) ; if (is.null(d)) return(NULL)
      tryCatch(VCN3_1sta(d$debits, d$dates, 3, stations$code_station[i]), error = function(e) NULL)
    })

    prep_df <- function(tous, col_val) {
      bind_rows(tous) %>%
        left_join(surface %>% select(code_station, surface_bv), by = c("code_sta" = "code_station")) %>%
        mutate(spe = .data[[col_val]] / surface_bv) %>%
        group_by(annee) %>%
        summarise(mediane = median(spe, na.rm = TRUE), .groups = "drop") %>%
        mutate(annee = as.numeric(annee))
    }

    df_vcn10 <- prep_df(tous_vcn10, "VCNx_annuel_spe")
    df_vcn3  <- prep_df(tous_vcn3,  "VCN3_annuel_spe")

    validate(need(nrow(df_vcn10) > 0, "Aucune donnée trouvée."))

    df_vcn10$tendance <- predict(lm(mediane ~ annee, data = df_vcn10))
    df_vcn3$tendance  <- predict(lm(mediane ~ annee, data = df_vcn3))

    plot_ly(x = ~df_vcn10$annee) %>%
      add_bars(y = ~df_vcn10$mediane, name = "VCN10",
               marker = list(color = "darkblue",    line = list(color = "grey40",     width = 1))) %>%
      add_bars(y = ~df_vcn3$mediane,  name = "VCN3",
               marker = list(color = "deepskyblue", line = list(color = "steelblue4", width = 1))) %>%
      add_lines(y = ~df_vcn10$tendance, name = "Tendance VCN10",
                line = list(color = "red",  dash = "dash", width = 2)) %>%
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

}
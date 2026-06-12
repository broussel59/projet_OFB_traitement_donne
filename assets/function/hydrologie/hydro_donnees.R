# ── Module : chargement API + stockage central ────────────────────

hydro_donnees_server <- function(input, series_brutes, surface_bv_data, seuils_stations, vcn_stations) {

  get_serie_hydro <- function(code_station, date_debut, date_fin, param) {
    get_hydrometrie_obs_elab(
      code_entite         = code_station,
      date_debut_obs_elab = date_debut,
      date_fin_obs_elab   = date_fin,
      grandeur_hydro_elab = param
    ) %>%
      select(code_station:resultat_obs_elab) %>%
      mutate(annee = ymd(date_obs_elab), annee = year(annee))
  }

  series_stations_tot <- function(hydro_stations, date_debut, date_fin, param) {
  get_serie_hydro_possible <- possibly(get_serie_hydro, otherwise = NULL)
  n <- length(hydro_stations)
  imap(hydro_stations, function(x, i) {
    setProgress(i / n, message = paste("Station", i, "/", n))
    get_serie_hydro_possible(x, date_debut, date_fin, param)
  })
}

  # Récupère les stations du département au clic bouton
  stations_dept <- eventReactive(input$run_all, {
    tryCatch({
      request("https://hubeau.eaufrance.fr/api/v2/hydrometrie/referentiel/stations") %>%
        req_url_query(code_departement = input$dept, en_service = TRUE, format = "json") %>%
        req_perform() %>%
        resp_body_json(simplifyVector = TRUE) %>%
        .$data
    }, error = function(e) NULL)
  })

  observeEvent(stations_dept(), {
    stations   <- stations_dept()
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
      setProgress(0.5, message = "Calcul des indicateurs...")
    })

    names(series) <- stations$code_station
    series_brutes(series)

    # ── Q90 / Q50 par station ─────────────────────────────────────
    seuils <- map(series, function(df) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
      debits <- as.numeric(df$resultat_obs_elab)
      debits <- debits[!is.na(debits) & debits >= 0]
      if (length(debits) < 3650) return(NULL)
      if (median(debits) > 2) debits <- debits / 1000
      list(q90 = quantile(debits, 0.10), q50 = quantile(debits, 0.50))
    })
    names(seuils) <- stations$code_station
    seuils_stations(seuils)

    # ── VCN10 / VCN3 par station ──────────────────────────────────
    vcn <- map(names(series), function(code) {
      df <- series[[code]]
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

      debits  <- as.numeric(df$resultat_obs_elab)
      dates   <- as.Date(df$date_obs_elab)
      valides <- !is.na(debits) & debits >= 0 & !is.na(dates)
      debits  <- debits[valides] ; dates <- dates[valides]
      if (length(debits) < 30) return(NULL)
      if (median(debits) > 2) debits <- debits / 1000

      vcn10 <- tryCatch(VCNx_1sta(debits, dates, 10, code), error = function(e) NULL)
      vcn3  <- tryCatch(VCN3_1sta(debits, dates,  3, code), error = function(e) NULL)

      list(vcn10 = vcn10, vcn3 = vcn3)
    })
    names(vcn) <- names(series)
    vcn_stations(vcn)

    # ── Surfaces des bassins versants ─────────────────────────────
    codes_sites <- get_hydrometrie_stations(code_departement = input$dept) %>%
      filter(en_service == TRUE) %>%
      select(code_site, code_station)

    surface_bv <- get_hydrometrie_sites(code_departement = input$dept) %>%
      select(code_site, surface_bv) %>%
      right_join(codes_sites, by = "code_site")

    surface_bv_data(surface_bv)

    setProgress(1, message = "Terminé")
  })

  return(stations_dept)
}

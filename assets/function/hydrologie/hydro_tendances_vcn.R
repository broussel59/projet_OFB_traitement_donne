# ── Module : tendances VCN10 (Mann-Kendall) ───────────────────────

hydro_tendances_vcn_server <- function(input, output, stations_dept, series_brutes, seuils_stations) {

  tester_mk <- function(valeurs) {
    if (length(valeurs) < 4) return(NULL)
    tryCatch(
      list(mk = mk.test(valeurs), slope = sens.slope(valeurs)),
      error = function(e) NULL
    )
  }

  # VCN10 baisse = moins d'eau en étiage = dégradation
  interpreter <- function(res) {
    if (is.na(res$mk$p.value) || res$mk$p.value > 0.05 || res$slope$estimates == 0) return("Pas de tendance")
    if (res$slope$estimates < 0) return("Dégradation")
    return("Amélioration")
  }

  output$tendvcn <- renderDT({
    series   <- series_brutes()
    seuils   <- seuils_stations()
    stations <- stations_dept()
    req(series, seuils)

    resultats <- map2(series, seq_along(series), function(df, i) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

      code <- stations$code_station[i]
      s    <- seuils[[code]]
      if (is.null(s)) return(NULL)

      debits <- as.numeric(df$resultat_obs_elab)
      dates  <- as.Date(df$date_obs_elab)
      valides <- !is.na(debits) & debits >= 0 & !is.na(dates)
      debits <- debits[valides] ; dates <- dates[valides]
      if (length(debits) < 30) return(NULL)
      if (median(debits) > 1800) debits <- debits / 1000

      vcn10 <- tryCatch(VCNx_1sta(debits, dates, 10, code), error = function(e) NULL)
      if (is.null(vcn10) || nrow(vcn10) < 4) return(NULL)

      res <- tester_mk(vcn10$VCNx_annuel_spe)
      if (is.null(res)) return(NULL)

      data.frame(
        `Nom de la station` = stations$libelle_station[i],
        `Code station`      = code,
        `P-value`           = round(res$mk$p.value, 10),
        `Sens de la pente`  = round(res$slope$estimates, 5),
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
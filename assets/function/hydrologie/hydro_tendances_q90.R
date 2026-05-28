# ── Module : tendances durée de sécheresse (MK sur jours < Q90) ──

hydro_tendances_q90_server <- function(input, output, stations_dept, series_brutes, seuils_stations) {

  # Applique Mann-Kendall + pente de Sen
  tester_mk <- function(valeurs) {
    if (length(valeurs) < 4) return(NULL)
    tryCatch(
      list(mk = mk.test(valeurs), slope = sens.slope(valeurs)),
      error = function(e) NULL
    )
  }

  # Plus de jours secs = dégradation
  interpreter <- function(res) {
    if (is.na(res$mk$p.value) || res$mk$p.value > 0.05 || res$slope$estimates == 0) return("Pas de tendance")
    if (res$slope$estimates > 0) return("Dégradation")
    return("Amélioration")
  }

  output$tendances_q90 <- renderDT({
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
      if (median(df$debit) > 1800) df$debit <- df$debit / 1000

      # Nombre de jours sous le Q90 par année
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
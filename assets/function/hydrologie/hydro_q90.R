
hydro_q90_server <- function(input, output, stations_dept, seuils_stations) {

  output$q90 <- renderDT({
    seuils   <- seuils_stations()
    stations <- stations_dept()
    req(seuils)

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

}
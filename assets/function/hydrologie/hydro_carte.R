
hydro_carte_server <- function(input, output, stations_dept) {

  output$map_france <- renderLeaflet({
    data <- stations_dept()
    m    <- leaflet() %>% addTiles()

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

}
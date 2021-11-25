# carga de librerías

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(sf)
library(raster)
library(rgdal)
library(DT)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(ggplot2)
library(graphics)
library(tidyverse)
library(rmapshaper)
library(spDataLarge)
library(spData)
library(utf8)

# Lectura de una capa vectorial (GeoJSON) de división distrial de Santa Ana
limite_distrital <-
  st_read(
    "https://dvictoria2020.github.io/Proyecto1-R/limite_distrital.geojson",
    quiet = TRUE
  )
# Transformación del CRS del objeto división distrital
limite_distrital <-
  limite_distrital %>%
  st_transform(4326)


# Lectura de una capa vectorial (GeoJSON) patentes de Santa Ana
patentesST <-
  st_read("https://dvictoria2020.github.io/tarea3-tablero-shiny/patentesST.geojson",
    quiet = TRUE
  )

# Transformación del CRS del objeto patentes
patentesST <-
  patentesST %>%
  st_transform(4326)

# Lectura de una capa vectorial (GeoJSON) red vial de Santa Ana
red_vial <-
  st_read("https://dvictoria2020.github.io/tarea3-tablero-shiny/red_vial.geojson",
          quiet = TRUE
  )

# Transformación del CRS del objeto patentes
red_vial <-
  red_vial %>%
  st_transform(4326)


# Lectura de archivo CSV de patentes comerciales en Santa Ana
Patente_final <-
  st_read(
    "/vsicurl/https://dvictoria2020.github.io/Proyecto1-R/Patente_final.csv",
    options = c(
      "X_POSSIBLE_NAMES=Latitud",
      "Y_POSSIBLE_NAMES=Longitud"
    ),
    quiet = TRUE
  )
# Asignación de un CRS al objeto patentes
st_crs(Patente_final) <- 4326

# Lista ordenada de actividad + "Todas"
lista_actividad <- unique(patentesST$Actividad)
lista_actividad <- sort(lista_actividad)
lista_actividad <- c("Todas", lista_actividad)

# Lista ordenada de distritos + "Todos"
lista_distritos <- unique(patentesST$Distrito)
lista_distritos <- sort(lista_distritos)
lista_distritos <- c("Todos", lista_distritos)

# Componentes de la aplicación Shiny
# Definición del objeto ui
ui <- 
  dashboardPage(
   dashboardHeader(title = "Actividad comercial en Santa Ana"),
   dashboardSidebar(sidebarMenu(
    menuItem(
      text = "Filtros",
      selectInput(
        inputId = "Actividad",
        label = "Actividad Comercial",
        choices = lista_actividad,
        selected = "Todas"
      ),
      selectInput(
        inputId = "Distrito",
        label = "Distrito",
        choices = lista_distritos,
        selected = "Todos"
      ),
      startExpanded = TRUE
    )
  )),
  dashboardBody(
    fluidRow(
    box(
      title = "Mapa distribución de patentes comerciales en Santa Ana",
      leafletOutput(outputId = "mapa"),
      width = 6
    ),    
    box(
      title = "Registros de patentes comerciales", 
      DTOutput(outputId = "tabla"),
      width = 6
    )
  ),

  fluidRow(
    box(
      title = "Grafico",
      plotOutput(outputId = "grafico"),
      width = 12
    )
  ))
  )
server <- function(input, output, session) {
  filtrarRegistros <- reactive({
  # Remoción de geometrías y selección de columnas
  patente_filtrada <-
    patentesST %>%
    dplyr::select(Nombre_comercio, Aprobacion, Actividad, Tipo_persona, Distrito)
  
  
  # Filtrado de actividad
  if (input$Actividad != "Todas") {
    patente_filtrada <-
      patentesST %>%
      filter(Actividad == input$Actividad)
  }    
   
  
  # Filtrado de actividad por distrito
  if (input$Distrito != "Todos") {
    patente_filtrada <-
      patente_filtrada %>%
      filter(Distrito == input$Distrito)
    }
  
    return(patente_filtrada)
    
  })


### Registros de presencia
  output$tabla <- renderDT({
    registros <- filtrarRegistros()
    
    registros %>%
      st_drop_geometry() %>%
      dplyr::select(Aprobacion, Actividad, Nombre_comercio, Tipo_persona, Distrito) %>%
      datatable(registros, options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.11.3/i18n/es_es.json'), pageLength = 8))
  })
  
  ### Mapa de distribución
  
output$mapa <- renderLeaflet({
  registros <-
    filtrarRegistros()
  

  # Mapa Leaflet con capas de provincias y registros de presencia de felinos
  leaflet() %>%
    addTiles(options = providerTileOptions(noWrap = TRUE), group = "Open Street Maps") %>%
    addProviderTiles("Esri.WorldImagery", group = "Imagen Satelital") %>%
    addPolygons(
      data = limite_distrital,
      color = "purple",
      fillColor = "transparent",
      stroke = TRUE,
      weight = 2.0,
      group = "Limite distrital"
    ) %>% 
    addPolylines(
      data = red_vial,
      color = "black",
      stroke = TRUE,
      weight = 2.0,
      group = "Red vial nacional"
    ) %>%
    addCircleMarkers(
        data = registros,
      stroke = FALSE,
      radius = 3,
      fillColor = 'green',
      fillOpacity = 1,
      group = "Patentes comerciales",
      popup = paste0(
        "<strong>Distrito: </strong>",
        registros$Distrito,
        "<br>",
        "<strong>Actividad Comercial: </strong>",
        registros$Actividad)
   
    ) %>% 
    addSearchOSM() %>%
    addResetMapButton() %>%
    addMouseCoordinates() %>%
    addMiniMap(tiles = providers$OpenStreetMap,
               toggleDisplay = TRUE) %>%
    addLayersControl(
      baseGroups = c("Open Street Maps", "Imagen Satelital"),
      overlayGroups = c("Patentes comerciales", "Limite distrital", "Red vial nacional" ),
      options = layersControlOptions(collapsed = FALSE
      )
    )
  })

### Grafico2
output$grafico <- renderPlot({
  # Preparación de los datos  
  registros <- filtrarRegistros()
  Actividad <-
  registros %>%
  st_drop_geometry() %>%
  select(Actividad) %>%
  rename(Actividad = Actividad) %>%
  group_by(Actividad) %>%
  summarise(suma = n()) %>%
  filter(suma > 0)
  

  ggplot(Actividad, aes(x = reorder(Actividad, -suma),y = suma)) +
    geom_col(colour = "#FF4040", fill = "#7FFFD4",width = 0.6) +
    geom_text(aes(label = suma), vjust = 1, colour = "black") +
    ggtitle("Actividades comerciales en Santa Ana") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 25,hjust = 1, vjust = 1)
    ) +
    xlab("Actividades") +
    ylab("Cantidad")
})
  
}


# Pestaña 2


# Provincias de Costa Rica y sus centroides calculados con st_centroid() y st_point_on_surface()
plot(
  limite_distrital$geometry,
  main = "Centroides de provincias: st_centroid (rojo) y st_point_on_surface (verde)",
  axes = TRUE,
  graticule = TRUE)

plot(st_centroid(limite_distrital),
     add = TRUE,
     pch = 16,
     col = "red")




# Llamado a la función shinyApp()

shinyApp(ui, server)


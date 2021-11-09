# carga de librerías

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(sf)
library(terra)
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

# Lectura de capa raster de uso urbano
uso_urbano_rWGS <-
  rast(
    "/vsicurl/https://dvictoria2020.github.io/Proyecto1-R/uso_urbano_rWGS.tif",
  )


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
      dateRangeInput(
        inputId = "fecha",
        label = "Fecha",
        start = "1981-01-01",
        end   = Sys.Date(),
        separator = " a ",
        language = "es"
      ),
      startExpanded = TRUE
    )
  )),
  dashboardBody(fluidRow(
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
  
  # Filtrado de actividad por fecha de aprobación
  patente_filtrada <-
    patente_filtrada %>%
    filter(
      Aprobacion >= as.Date(input$fecha[1], origin = "1981-01-01") &
        Aprobacion <= as.Date(input$fecha[2], origin = "1981-01-01")
      )  
  
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
  
  uso_urbano_rWGS_rl <- raster::raster(uso_urbano_rWGS)
    
    
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
    addRasterImage(
      uso_urbano_rWGS_rl,
      color= "#DDB892",
      opacity = 0.6,
      group = "Uso Urbano 2005"
    )%>%
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
        registros$Actividad),
      label = paste0(
        "Actividad: ", registros$Actividad,
        ",",
        "Distrito: ", registros$Distrito,
        ", ",
        "Fecha de aprobación:", registros$Aprobacion
      )
    ) %>% 
    addSearchOSM() %>%
    addResetMapButton() %>%
    addMouseCoordinates() %>%
    addMiniMap(tiles = providers$OpenStreetMap,
               toggleDisplay = TRUE) %>%
    addLayersControl(
      baseGroups = c("Open Street Maps", "Imagen Satelital"),
      overlayGroups = c("Patentes comerciales","Uso Urbano 2005", "Limite distrital" ),
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

# Llamado a la función shinyApp()

shinyApp(ui, server)


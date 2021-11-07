#Paquetes
library(dplyr)
library(sf)
library(terra)
library(raster)
library(rgdal)
library(DT)
library(plotly)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(graphics)

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
lista_patentes <- unique(Patente_final$Actividad)
lista_patentes <- sort(lista_patentes)
lista_patentes <- c("Todas", lista_patentes)

# Lista ordenada de distritos + "Todos"
lista_distritos <- unique(Patente_final$Distrito)
lista_distritos <- sort(lista_distritos)
lista_distritos <- c("Todos", lista_distritos)

# Componentes de la aplicación Shiny
ui <- dashboardPage(
  dashboardHeader(title = "Actividad comercial"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      text = "Filtros",
      selectInput(
        inputId = "Actividad",
        label = "Actividad Comercial",
        choices = lista_patentes,
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
      title = "Mapa registros de patentes",
      leafletOutput(outputId = "mapa"),
      width = 6
    ),
    box(
      title = "Registros de patentes",
      DTOutput(outputId = "tabla"),
      width = 6
    )
  ),
  fluidRow(
    box(
      title = "Valoración de los recursos del patrimonio material",
      plotlyOutput(outputId = "grafico"),
      width = 12
    )
  ))
)
server <- function(input, output, session) {
  filtrarRegistros <- reactive({
    # Remoción de geometrías y selección de columnas
    patente_filtrada <-
      Patente_final %>%
      dplyr::select(Nombre_comercio, Aprobacion, Actividad, Tipo_persona, Distrito)
    # Filtrado de actividad 
    if (input$Actividad != "Todas") {
      patente_filtrada <-
        Patente_final %>%
        filter(Actividad == input$Actividad)
    }
    
    # Filtrado de actividad por distrito
    if (input$Distrito != "Todos") {
      patente_filtrada <-
        patente_filtrada %>%
        filter(Distrito == input$Distrito)
    }
    
    # Filtrado de felidae por fecha
    patente_filtrada <-
      patente_filtrada %>%
      filter(
        Aprobacion >= as.Date(input$fecha[1], origin = "1981-01-01") &
          Aprobacion <= as.Date(input$fecha[2], origin = "1981-01-01")
      )
    
    return(patente_filtrada)
    
  })

  ### Mapa de distribución
  

  output$mapa <- renderLeaflet({
    registros <-
      filtrarRegistros()
    
    # Conversión del objeto uso a la clase RasterLayer
    uso_urbano_rWGS_rl <- raster::raster(uso_urbano_rWGS)
    
    # Mapa Leaflet con capas de provincias y registros de presencia de felinos
    leaflet() %>%
      addTiles(options = providerTileOptions(noWrap = TRUE), group="Open Street Maps") %>%
      addProviderTiles("Esri.WorldImagery", group="Imagen Satelital")%>%
      addRasterImage(
        uso_urbano_rWGS_rl,
        color= "#DDB892",
        opacity = 0.6,
        group = "Uso Urbano 2005"
      ) %>%
      addPolygons(
        data = limite_distrital,
        color = "purple",
        fillColor = "transparent",
        stroke = TRUE,
        weight = 2.0,
        group = "Limite distrital"
      ) %>%
      addCircleMarkers(
        data = registros,
        stroke = TRUE,
        radius = 4,
        fillColor = 'orange',
        fillOpacity = 1,
        label = paste0(
          registros$Actividad,
          ", ",
          registros$Distrito,
          ", ",
          registros$Aprobacion
        ),
        popup = paste0(
          "<strong>Distritos: </strong>",
          "<em>",
          registros$Distrito,
          "</em>",
          "<br>",
          "<strong>Actividad Comercial: </strong>",
          registros$Actividad,
        )
      ) %>%
      
      addMiniMap(
        tiles = providers$OpenStreetMap,
        toggleDisplay = TRUE
      )%>%
      addSearchOSM() %>%
      addResetMapButton() %>%
      addResetMapButton()%>%
      addMouseCoordinates()
    addLayersControl(
      baseGroups = c("Open Street Maps","Imagen Satelital"),
      overlayGroups = c("Uso Urbano 2005","Limite distrital", "Patentes comerciales"), 
      options = layersControlOptions(collapsed = FALSE)
    )
  })


  
  
}
shinyApp(ui, server)


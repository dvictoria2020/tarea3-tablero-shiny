#Paquetes

library(flexdashboard)
defaultEncoding <- "UTF8"
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
lista_patentes <- unique(Patente_final$Actividad)
lista_patentes <- sort(lista_patentes)
lista_patentes <- c("Todas", lista_patentes)

# Lista ordenada de distritos + "Todos"
lista_distritos <- unique(Patente_final$Distrito)
lista_distritos <- sort(lista_distritos)
lista_distritos <- c("Todos", lista_distritos)

# Componentes de la aplicación Shiny
ui <- 
  dashboardPage(
   dashboardHeader(title = "Actividad comercial en Santa Ana"),
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
      title = "Registros de patentes", 
      DTOutput(outputId = "tabla"),
      width = 6
    ),
    box(
      title = "Mapa distribución de patentes comerciales en Santa Ana",
      leafletOutput(outputId = "mapa"),
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
      Patente_final %>%
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
}

### Registros de presencia
output$tabla <- renderDT({
  registros <- filtrarRegistros()
  
  registros %>%
    st_drop_geometry() %>%
  dplyr::select(Aprobacion, Nombre_comercio, Tipo_persona, Distrito) %>%
  datatable(Patente_final, options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.11.3/i18n/es_es.json'), pageLength = 8))
  pageLenth =5
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
      addCircleMarkers(
        data = registros,
        stroke = F,
        radius = 4,
        fillColor = 'orange',
        fillOpacity = 1,
        label = paste0(
          registros$Actividad,
          ",",
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
          registros$Actividad
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
  

  
  ### Estacionalidad
  
  
  output$grafico <- renderPlot({
    registros <- filtrarRegistros()
    
    # Convertimos a porcentaje
    
    porcentaje <- Patente_final %>%
      group_by(Distrito) %>%
      count() %>%
      ungroup() %>%
      mutate(percentage = `n` / sum(`n`) * 100)
    ggplot(porcentaje, aes(x = 1, y = percentage, fill = Distrito)) +
      geom_bar(stat = "identity", colour = "black", size = 0.25) +
      geom_text(aes(label = paste0(round(percentage, 1), "%")),
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      labs(title = "Porcentaje de licencias comerciales y de licor según 
                 distrito en el cantón Santa Ana") +
      theme_void() + scale_fill_brewer(palette = "Dark2")
  })

shinyApp(ui, server)


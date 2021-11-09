# carga de librerías

library(dplyr)
library(sf)
library(terra)
library(raster)
library(rgdal)
library(DT)
library(plotly)
library(ggplot2)
library(leaflet)
library(shiny)
library(shinydashboard)

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

# Lectura de capa raster de uso urbano
uso_urbano_rWGS <-
  rast(
    "/vsicurl/https://dvictoria2020.github.io/Proyecto1-R/uso_urbano_rWGS.tif",
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
      title = "Registros de patentes", 
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
  dplyr::select(Aprobacion, Nombre_comercio, Tipo_persona, Distrito) %>%
  datatable(patentesST, options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.11.3/i18n/es_es.json'), pageLength = 8))
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
      stroke = TRUE,
      radius = 4,
      fillColor = 'orange',
      fillOpacity = 1,
      group = "Patentes comerciales",
      popup = paste0(
        "<strong>Distrito: </strong>",
        registros$Distrito,
        "<br>",
        "<strong>Actividad Comercial: </strong>",
        registros$Actividad),
      label = paste0(
        registros$Actividad,
        ",",
        registros$Distrito,
        ", ",
        registros$Aprobacion
      )
    ) %>% 
      
      addMiniMap(
        tiles = providers$OpenStreetMap,
        toggleDisplay = TRUE
      )%>%
      addLayersControl(
        baseGroups = c("Open Street Maps","Imagen Satelital"),
        overlayGroups = c("Uso Urbano 2005","Limite distrital", "Patentes comerciales"), 
        options = layersControlOptions(collapsed = FALSE)
    )
  })

### Grafico2
output$grafico <- renderPlot({
  registros <- filtrarRegistros()
  
  registros %>%
    st_drop_geometry() %>%
ggplot() + geom_col(
  data= filtrarRegistros(),
  aes(x= Distrito,
      y= n, fill = Actividad), width = 0.7)+
  
  ggtitle( "Distribución de patentes comerciales por tipo de personería
                          en los distritos de Santa Ana") +
  xlab("Distrito") + 
  ylab("Actividad") +
  scale_fill_manual(values = c("#FFE4C4", "#8B7D6B")) +
  theme (
    legend.title = element_blank(),
    legend.position = "right",
    plot.title = element_text(size = 14, face = "plain")
    
  )

  })
  
}

# Llamado a la función shinyApp()

shinyApp(ui, server)


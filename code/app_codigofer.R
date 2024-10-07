# Cargar las librerías necesarias
library(readr)
library(tidyr)
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud2)
library(wesanderson)
library(shinyWidgets)
library(geoAr) 
library(reactable)
library(plotly)

# Función para asignar colores a las regiones
paleta_colores_region <- function(provincia) {
  # Definir los colores
  colores <- wes_palette("Darjeeling1")[c(5, 2, 3)]  # Tres colores de la paleta
  nombres_regiones <- c("CABA", "Buenos Aires", "Resto del país")  # Nombres de las regiones
  colores_asignados <- setNames(colores, nombres_regiones)  # Asignar nombres a los colores
  
  # Añadir un color para casos de provincia desconocida
  colores_asignados["Desconocida"] <- "gray"
  
  # Mapear las provincias a regiones utilizando ifelse para vectores
  color_resultante <- ifelse(
    provincia == "CABA", 
    colores_asignados["CABA"], 
    ifelse(
      provincia == "Buenos Aires", 
      colores_asignados["Buenos Aires"], 
      colores_asignados["Resto del país"]
    )
  )
  
  return(color_resultante)
}


# Cargar los datos asegurando la codificación correcta
# conicet <- read_csv("C:/Users/usuario/Desktop/Concurso_Contar_con_Datos/Proyectos_CONICET/conicet_preprocesado.csv", locale = locale(encoding = "UTF-8"))
# conicet <- read_csv("D:/concurso_contar_con_datos/conicet_preprocesado.csv")
conicet <- read_csv("../data/conicet_preprocesado.csv")


# Asegurarse de que no haya valores NA en las columnas 'AÑO' y 'TIPO.CONVOCATORIA'
conicet <- conicet %>%
  filter(!is.na(AÑO), !is.na(TIPO.CONVOCATORIA), !is.na(lon), !is.na(lat), !is.na(Nombre_comision))

# Reemplazar nombres de regiones según tu requerimiento
conicet$region <- ifelse(conicet$region == "Buenos Aires", "Buenos Aires",
                         ifelse(conicet$region == "CABA", "CABA",
                                ifelse(conicet$region == "Resto del país", "Resto del país", "Desconocida")))


# Obtener el segundo color de la paleta "Darjeeling2"
color_fondo_titulo <- wes_palette("Darjeeling2")[2]

#UI---------------------
ui <- 
  navbarPage("",
             
             tabPanel("Página principal",
                      fluidPage(
                        tags$head(
                          tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap"),
                          tags$style(HTML(paste0("
            body, html { font-family: 'Roboto', sans-serif; }
            .titulo-app {
              background-color: ", color_fondo_titulo, "; 
              color: white;
              font-weight: bold;
              padding: 15px;
              border-radius: 10px;
              text-align: center;
            }
            .full-panel {
              height: 100vh !important;
              overflow-y: auto;
            }
          ")))
                        ),
                        
                        div(
                          class = "titulo-app",
                          h1(
                            style = "margin: 20px 0; font-family: 'Nimbus Sans', sans-serif; font-weight: bold;", 
                            "Ciencia sobre crecer, al alcance"
                          )
                        ),
                        
                        markdown("
  <div style='font-size: 18px; margin-left: 75px; margin-right: 150px; text-align: justify; background-color: #ABDDDE;
              padding: 10px; border-radius: 5px; margin: 15px 0;'> 
                          
    CONICET es el principal organismo de ciencia y tecnología del país. Saber qué temáticas se financian y en donde se investigan es
    información valiosa para la elaboración de políticas públicas, la formación de redes de investigadores y la sociedad civil en general.
    Si bien esta información es pública, no es de fácil acceso. 
    Basándonos en datos recopilados en <a href='https://ojs.revistacts.net/index.php/CTS/article/view/410' target='_blank'>este trabajo</a> sobre proyectos financiados
    de desarrollo infantil, desarrollamos el siguiente tablero interactivo. Este puede resultar valioso para fomentar redes de trabajo, 
    colaboración y diálogo entre quienes investigan en el campo del desarrollo.
    El desarrollo de este tipo de herramientas, con el fin de mejorar la accesibilidad a las investigaciones, podría ser replicado en distintas áreas,
    fortaleciendo la comunidad científica argentina.
  </div>
"),
                  
                        sidebarPanel(
                          class = "full-panel",  # Añadir la clase para altura completa
                          sliderInput("yearInput", "Seleccionar período de tiempo",
                                      min = min(conicet$AÑO, na.rm = TRUE),
                                      max = max(conicet$AÑO, na.rm = TRUE),
                                      value = range(conicet$AÑO, na.rm = TRUE),
                                      sep = ""),
                          
                          shinyWidgets::pickerInput("disciplinaInput", 
                                                    "Seleccionar disciplina:", 
                                                    choices = unique(conicet$Nombre_comision),
                                                    selected = unique(conicet$Nombre_comision),
                                                    options = list(`actions-box` = TRUE),
                                                    multiple = TRUE),
                          
                          uiOutput("nubePalabras")  
                        ),
                        
                        mainPanel(
                          fluidRow(
                            column(6, leafletOutput("mapa", height = "100vh")),  # Mapa a la izquierda
                            column(6, 
                                   fluidRow(
                                     column(12, plotOutput("graficoProyectosRegion", height = "50vh")),  
                                     column(12, 
                                            fluidRow(
                                              column(12, 
                                                     shinyWidgets::pickerInput("provincia_filter", "Seleccione una provincia:",
                                                                               choices = c("Todas", unique(conicet$PROVINCIA)), 
                                                                               selected = "Todas", 
                                                                               options = list(`actions-box` = TRUE), 
                                                                               multiple = TRUE)),  
                                              column(12, plotOutput("graficoProyectosTiempo", height = "50vh"))   
                                            )
                                     )
                                   )
                            )
                          )
                        )
                      ),
                      
                      tags$footer(
                        HTML('<span style="font-size:12px;"><i>Autores: Florencia Altschuler, Federico Giovannetti, Fernando Steeb y Mariana Smulski. </i></span><br>
                                  <span style="font-size:10px;">Datos obtenidos de <a href="https://ojs.revistacts.net/index.php/CTS/article/view/410" style="text-decoration:none; color:inherit;">Smulski, M., Giovannetti, F., Steeb, F., Serra, A. L. P., Grasser, F. B., Jove, G. M., & Cevasco, J. (2024). Agendas científicas sobre desarrollo infantil en CONICET: Evolución de becas e ingresos de investigadores en el periodo 2010-2020. Revista Iberoamericana de Ciencia, Tecnología y Sociedad</a> - CTS.</span>'),
                        align = "left",
                        style = "position:fixed; bottom:0; width:100%; padding:10px; background-color:#f8f9fa;"
                      )
             ),
             
             tabPanel("Datos",
                      HTML("<span style='font-size: 14px;'>Acá vas a poder acceder a los datos completos...</span>"),
                      fluidPage(
                        reactableOutput("data")
                      )
             ))


# Server---------------
server <- function(input, output, session) {
  
  # Filtrar los datos por año, tipo de proyecto y disciplina
  filteredData <- reactive({
    conicet %>%
      filter(AÑO >= input$yearInput[1], AÑO <= input$yearInput[2],  # Ajustar filtro de año para rango
             Nombre_comision %in% input$disciplinaInput,
             region != "Desconocida")  # Excluir "Desconocida"
  })
  
  output$mapa <- renderLeaflet({
    mapa_argentina <- get_geo("ARGENTINA", level = "provincia")
    
    if (!is.null(mapa_argentina)) {
      leaflet() %>%
        addPolygons(data = mapa_argentina, fillColor = "blue", color = "white") %>%
        geoAr::addArgTiles() %>%
        setView(lng = -63.6152, lat = -38.4161, zoom = 4) %>%
        setMaxBounds(lng1 = -75.0, lat1 = -56.0, lng2 = -45.0, lat2 = -20.0)
    } else {
      leaflet() %>%
        setView(lng = -63.6152, lat = -38.4161, zoom = 4) %>%
        addTiles()
    }
  })
  
  # Observación para actualizar el mapa según los datos filtrados
  observe({
    datos <- filteredData()
    
    # if (nrow(datos) > 0) {
      # Calcular el número total de proyectos después de aplicar filtros
      total_proyectos <- sum(datos$count)  # Asegúrate de que 'count' es la columna que contiene los proyectos.
      
      leafletProxy("mapa", data = datos) %>%
        clearMarkers() %>%
        addCircleMarkers(
          ~lon, ~lat, 
          popup = ~paste(LOCALIDAD, "<br>", "Número de proyectos: ", count, "<br>"),  
          radius = ~log10(count + 1) * 5,  # Ajustar el tamaño según los filtros
          color = ~paleta_colores_region(region),  # Colores según región
          fillOpacity = 0.6
        )
    # }
  })
  
  # Nube de palabras
  output$nubePalabras <- renderUI({
    palabras_clave <- filteredData()$PALABRAS.CLAVE.PROYECTO
    if (length(palabras_clave) == 0 || all(is.na(palabras_clave))) {
      return(NULL)  # Evitar errores si no hay palabras clave
    }
    
    texto_completo <- paste(na.omit(palabras_clave), collapse = " ")
    corpus <- Corpus(VectorSource(texto_completo))
    
    # Transformaciones sobre el corpus
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, content_transformer(removePunctuation))
    corpus <- tm_map(corpus, content_transformer(removeWords), stopwords("spanish"))
    corpus <- tm_map(corpus, content_transformer(stripWhitespace))
    
    # Crear matriz de términos
    dtm <- TermDocumentMatrix(corpus)
    matriz_dtm <- as.matrix(dtm)
    frecuencia <- sort(rowSums(matriz_dtm), decreasing = TRUE)
    
    # Verificar si hay palabras
    if (length(frecuencia) == 0) {
      return(NULL)  # Evitar errores si no hay palabras
    }
    
    nube <- data.frame(palabra = names(frecuencia), freq = frecuencia)
    
    # Renderizar la nube de palabras en UI usando wordcloud2Output
    wordcloud2Output <- wordcloud2(nube, size = 1.2, minSize = 0.5, gridSize = 10, 
                                   backgroundColor = "white")
    
    return(wordcloud2Output)
  })
  
  # Renderizar gráfico de "Proyectos a lo largo del tiempo"
  output$graficoProyectosTiempo <- renderPlot({
    # Filtrar datos por provincias seleccionadas, considerando "Todas"
    provincias_seleccionadas <- if ("Todas" %in% input$provincia_filter) {
      unique(conicet$PROVINCIA)  # Si "Todas" está seleccionada, incluye todas las provincias
    } else {
      input$provincia_filter  # O incluye las provincias seleccionadas
    }
    
    datos_tiempo <- filteredData() %>%
      filter(PROVINCIA %in% provincias_seleccionadas) %>%
      group_by(AÑO) %>%
      summarise(total_proyectos = n())
    
    max_y <- max(datos_tiempo$total_proyectos, na.rm = TRUE)  # Obtener el valor máximo
    
    ggplot(datos_tiempo, aes(x = AÑO, y = total_proyectos)) +
      geom_line(color = "blue", size = 1) +
      geom_point(size = 3, color = "blue") +
      labs(title = "Proyectos por año", x = "", y = "") +  # Título del gráfico
      scale_x_continuous(breaks = datos_tiempo$AÑO) +  # Mostrar todos los años
      scale_y_continuous(limits = c(0, max_y)) +  # Limitar el eje Y
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14),  # Ajustar tamaño del título
        axis.text.x = element_text(size = 12),  # Ajustar tamaño de fuente en eje X
        axis.text.y = element_text(size = 12)   # Ajustar tamaño de fuente en eje Y
      ) +
      geom_text(aes(label = total_proyectos), 
                vjust = -0.5, 
                size = 4, 
                show.legend = FALSE)  # Etiquetas de puntos
  })
  
  # Renderizar gráfico de "Proyectos por región"
  output$graficoProyectosRegion <- renderPlot({
    datos_region <- filteredData() %>%
      group_by(region) %>%
      summarise(total_proyectos = n())
    
    # Reordenar las regiones para que Buenos Aires esté al lado de CABA
    datos_region <- datos_region %>%
      mutate(region = factor(region, levels = c("CABA", "Buenos Aires", "Resto del país")))
    
    ggplot(datos_region, aes(x = region, y = total_proyectos, fill = region)) +
      geom_bar(stat = "identity") +
      labs(title = "Proyectos por región", x = "", y = "", size = 14) +  # Quitar nombre ejes y cambiar título
      scale_fill_manual(values = c("Buenos Aires" = "#00A08A", "CABA" = "#5BBCD6", "Resto del país" = "#F2AD00")) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 14),  
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14), 
        legend.position = "none"
      )
  })
  
  # Tabla de datos
  output$data <- renderReactable({
    # Configurar las opciones de la tabla
    options(reactable.theme = reactableTheme(
      borderColor = "#dfe2e5",
      stripedColor = "#f6f8fa",
      highlightColor = "#f0f5f9",
      cellPadding = "8px 12px",
      style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
      searchInputStyle = list(width = "100%")
    ))
    
    # Configurar el idioma de la tabla
    options(reactable.language = reactableLang(
      searchPlaceholder = "Buscar",
      noData = "No se encontró información",
      pageInfo = "{rowStart} a {rowEnd} de {rows} proyectos",
      pagePrevious = "\u276e",
      pageNext = "\u276f"
    ))
    
    # Renderizar la tabla
    reactable(
      conicet %>% 
        select(CONVOCATORIA, NOMBRE.POSTULANTE, DISCIPLINA.CODIGO,
               LT.POSTULANTE, DIRECTOR, TITULO.PROYECTO, RESUMEN.PROYECTO,
               PALABRAS.CLAVE.PROYECTO, ESPECIALIDAD.REFERIDA, PAIS, REGION, PROVINCIA, LOCALIDAD),
      filterable = TRUE, 
      minRows = 10, 
      searchable = TRUE,
      defaultPageSize = 10,
      bordered = TRUE,
      highlight = TRUE,
      striped = TRUE,
      # Set table height to ensure scrolling and sticky headers
      style = list(height = "calc(105vh - 150px)"),  # Ajustar 150px para otros elementos UI
      defaultColDef = colDef(
        headerStyle = list(
          position = "sticky",
          top = 0,
          zIndex = 1,
          borderBottom = "2px solid #ddd"
        ),
        minWidth = 150
      ),
      columns = list(
        RESUMEN.PROYECTO = colDef(minWidth = 800),
        TITULO.PROYECTO = colDef(minWidth = 200)
      )
    )
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)

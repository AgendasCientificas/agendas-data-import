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


# Función para asignar colores a las regiones
paleta_colores_region <- function(region) {
  colores <- wes_palette("Darjeeling1")[c(5, 2, 3)]
  nombres_regiones <- c("CABA", "Buenos Aires", "Resto del país")
  colores_asignados <- setNames(colores, nombres_regiones)
  colores_asignados["Desconocida"] <- "gray"
  return(colores_asignados[region])
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
                          background-color: ", color_fondo_titulo, "; /* Segundo color de Darjeeling2 */
                          color: white;
                          font-weight: bold;
                          padding: 15px;
                          border-radius: 10px;
                          text-align: center;
                          }
                                                 ")))
                          ),
                        
                        # Título con nuevo formato y color de fondo de Darjeeling2
                        div(
                          class = "titulo-app",
                          h1(
                            style = "margin: 20px 0; font-family: 'Nimbus Sans', sans-serif; font-weight: bold;", 
                            "Ciencia sobre crecer, al alcance"
                            )
                          ),
                          
                          markdown("<span style='font-size: 18px;display: block; margin-left: 75px; margin-right: 150px; text-align: justify; background-color:#ABDDDE;
                          padding: 10px; border-radius: 5px;margin: 15px 0;'> 
                          
                          CONICET es el principal organismo de ciencia y tecnología del país. Saber qué temáticas se financian y en donde se investigan es
                          información valiosa para la elaboración de políticas públicas, la formación de redes de investigadores y la sociedad civil en general.
                          Si bien esta información es pública, no es de fácil acceso. 
                          Basándonos en datos recopilados en [este trabajo](https://ojs.revistacts.net/index.php/CTS/article/view/410) sobre proyectos financiados
                          de desarrollo infantil, desarrollamos el siguiente tablero interactivo. Este puede resultar valioso para fomentar redes de trabajo, 
                          colaboración y diálogo entre quienes investigan en el campo del desarrollo.
                          El desarrollo de este tipo de herramientas, con el fin de mejorar la accesibilidad a las investigaciones, podría ser replicado en distintas áreas,
                          fortaleciendo la comunidad científica argentina.
                          </span>"),
                        
                        sidebarLayout(
                          
                          sidebarPanel(
                            
                              sliderInput("yearInput", "Seleccionar período de tiempo",
                                          min = min(conicet$AÑO, na.rm = TRUE),
                                          max = max(conicet$AÑO, na.rm = TRUE),
                                          value = range(conicet$AÑO, na.rm = TRUE),
                                          sep = ""),
                              pickerInput("disciplinaInput", 
                                          "Seleccionar disciplina:", 
                                          choices = unique(conicet$Nombre_comision),  # Usar los nombres de las disciplinas
                                          selected = unique(conicet$Nombre_comision),
                                          options = list(
                                            `actions-box` = TRUE, 
                                            `select-all-text` = "Seleccionar todas",  
                                            `deselect-all-text` = "Deseleccionar todas"  
                                          ),
                                          multiple = TRUE),
                              selectInput("graficoSeleccion", 
                                          "Seleccionar Tipo de Gráfico:", 
                                          choices = c("Proyectos a lo largo del tiempo" = "tiempo", 
                                                      "Proyectos por región" = "region")),
                              conditionalPanel(
                                condition = "input.graficoSeleccion == 'tiempo'",
                                plotOutput("graficoProyectosTiempo", height = "50vh")
                              ),
                              conditionalPanel(
                                condition = "input.graficoSeleccion == 'region'",
                                plotOutput("graficoProyectosRegion", height = "50vh")
                              )
                            ),
                          
                          mainPanel(
                            
                            fluidRow(
                              column(6, leafletOutput("mapa", height = "100vh")),
                              column(6, htmlOutput("nubePalabras", height = "100vh"))
                              )
                            )
                          ),
  
                          # Agregar pie de página con estilos personalizados para los autores y fuente de datos
                          tags$footer(
                            HTML('<span style="font-size:12px;"><i>Autores: Florencia Altschuler, Federico Giovannetti, Fernando Steeb y Mariana Smulski. </i></span><br>
                                  <span style="font-size:10px;">Datos obtenidos de <a href="https://ojs.revistacts.net/index.php/CTS/article/view/410" style="text-decoration:none; color:inherit;">Smulski, M., Giovannetti, F., Steeb, F., Serra, A. L. P., Grasser, F. B., Jove, G. M., & Cevasco, J. (2024). Agendas científicas sobre desarrollo infantil en CONICET: Evolución de becas e ingresos de investigadores en el periodo 2010-2020. Revista Iberoamericana de Ciencia, Tecnología y Sociedad</a> - CTS.</span>'),
                            align = "left",
                            style = "position:fixed; bottom:0; width:100%; padding:10px; background-color:#f8f9fa;"
                          )
                        
                        )
                      
                      ),
             
             tabPanel("Datos",
                      
                      markdown("<span style='font-size: 14px;'>
                               Acá vas a poder acceder a los datos completos. En el campo *Buscar* podés explorar cualquier
                               aspecto que te interese. También podés ordenar las columnas de menor a mayor, o filtrar usando
                               el campo vacío que está abajo del nombre de cada columna"),
                      
                      
                      
                      
                      fluidPage(
                        
                        reactableOutput("data")
                        
                      )
                      
                      
             )
             
  )


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
        setView(lng = -64.1830, lat = -40.4167, zoom = 5)%>%
        # Define los límites de Argentina
        fitBounds(lng1 = -74.0, lat1 = -56.0, lng2 = -51.0, lat2 = -21.0) %>%
        setMaxBounds(lng1 = -74.0, lat1 = -56.0, lng2 = -51.0, lat2 = -21.0)   # Establece el nivel mínimo de zoom
    } else {
      leaflet() %>%
        setView(lng = -64.1830, lat = -38.4167, zoom = 5) %>%
        addTiles()  # Mapa básico sin capas si falla la descarga
    }
  })
  
  
  observe({
    leafletProxy("mapa", data = filteredData()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        ~lon, ~lat, 
        popup = ~paste(LOCALIDAD, "<br>", "Número de proyectos: ", count),  
        radius = ~log10(count + 1) * 5,  
        color = ~paleta_colores_region(region),
        fillOpacity = 0.6
      )
  })
  
  output$nubePalabras <- renderUI({
    palabras_clave <- filteredData()$PALABRAS.CLAVE.PROYECTO
    if (length(palabras_clave) == 0 || all(is.na(palabras_clave))) {
      return(NULL)  # Evitar errores si no hay palabras clave
    }
    
    texto_completo <- paste(na.omit(palabras_clave), collapse = " ")
    corpus <- Corpus(VectorSource(texto_completo))
    
    # Verificar si el corpus tiene contenido
    if (length(corpus) == 0) return(NULL)
    
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
    corpus <- tm_map(corpus, stripWhitespace)
    
    # Comprobar si el corpus está vacío después de las transformaciones
    if (length(corpus) == 0 || length(unlist(lapply(corpus, length))) == 0) {
      return(NULL)
    }
    
    dtm <- TermDocumentMatrix(corpus)
    matriz_dtm <- as.matrix(dtm)
    frecuencia <- sort(rowSums(matriz_dtm), decreasing = TRUE)
    
    # Comprobar si hay palabras
    if (length(frecuencia) == 0) {
      return(NULL)  # Evitar errores si no hay palabras
    }
    
    nube <- data.frame(palabra = names(frecuencia), freq = frecuencia)
    wordcloud2(nube, size = 1.5)
  })
  
  output$graficoProyectosTiempo <- renderPlot({
    datos_tiempo <- filteredData() %>%
      group_by(AÑO) %>%
      summarise(total_proyectos = n())
    
    ggplot(datos_tiempo, aes(x = AÑO, y = total_proyectos)) +
      geom_line(color = "blue", size = 1) +
      geom_point(size = 3, color = "blue") +
      labs(title = "Proyectos a lo largo del tiempo",
           x = "Año",
           y = "Número de proyectos") +
      theme_minimal()
  })
  
  output$graficoProyectosRegion <- renderPlot({
    datos_region <- filteredData() %>%
      group_by(region) %>%
      summarise(total_proyectos = n())
    
    ggplot(datos_region, aes(x = reorder(region, -total_proyectos), y = total_proyectos, fill = region)) +
      geom_bar(stat = "identity") +
      labs(title = "Proyectos por Región",
           x = "Región",
           y = "Número de proyectos") +
      scale_fill_manual(values = paleta_colores_region(unique(datos_region$region))) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Table output
  
  output$data <- renderReactable({
    
    #Lets use options to style our reactable to match our dark theme.
    options(reactable.theme = reactableTheme(
      borderColor = "#dfe2e5",
      stripedColor = "#f6f8fa",
      highlightColor = "#f0f5f9",
      cellPadding = "8px 12px",
      style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
      searchInputStyle = list(width = "100%")
    ),
    
    reactable.language = reactableLang(
      searchPlaceholder = "Buscar",
      noData = "No se encontró información",
      pageInfo = "{rowStart} a {rowEnd} de {rows} proyectos",
      pagePrevious = "\u276e",
      pageNext = "\u276f",
    )
    
    )
    
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
      style = list(height = "calc(105vh - 150px)"), # Adjust 150px to account for other UI elements
      defaultColDef = colDef(
        headerStyle = list(
          position = "sticky",
          top = 0,
          # background = "hsl(233, 9%, 19%)",
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
  
  

  # 
  
  
}

# Ejecutar la aplicación
shinyApp(ui, server)


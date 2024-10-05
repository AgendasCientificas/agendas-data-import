library(readr)
library(shiny)
library(leaflet)
library(dplyr)
library(ggmap)
library(osmdata)
library(ggplot2)
library(geoAr)
# conicet <- read_csv("D:/concurso_contar_con_datos/base_becas_e_ingresos_desarrollo_NNyA_2010_2020.csv") ubicación Flor
# conicet <- read_csv("D:/concurso_contar_con_datos/nuevo/base_becas_e_ingresos_desarrollo_NNyA_2010_2020.csv") #Fer
conicet <- read_csv("data/base_becas_e_ingresos_desarrollo_NNyA_2010_2020.csv")

# Crear el diccionario con localidades
diccionario_localidades <- unique(conicet$LOCALIDAD)

# Reemplazar "Esplugues de Llobregat" por "Barcelona" en el diccionario
diccionario_localidades <- gsub("ESPLUGUES DE LLOBREGAT", "BARCELONA", diccionario_localidades)
diccionario_localidades <- gsub("HURLINGAM", "HURLINGHAM", diccionario_localidades)
diccionario_localidades <- gsub("SAN FDO DEL VALLE DE CATAMARCA", "SAN FERNANDO DEL VALLE DE CATAMARCA", diccionario_localidades)
# Reemplazamos "S.S. DE JUJUY" y "JUJUY" por "San Salvador de Jujuy"
diccionario_localidades <- replace(diccionario_localidades, 
                                   diccionario_localidades %in% c("S.S. DE JUJUY", "San Salvador de Jujuy"), 
                                   "JUJUY")
diccionario_localidades <- replace(diccionario_localidades, 
                                   diccionario_localidades %in% c("SAN CARLOS DE BARILOCHE"), 
                                   "BARILOCHE")
diccionario_localidades <- replace(diccionario_localidades, 
                                   diccionario_localidades %in% c("SAN MIGUEL DE TUCUMAN"), 
                                   "TUCUMAN")
# Convertimos todas las localidades a mayúsculas
diccionario_localidades <- toupper(diccionario_localidades)
# Inicializar lista para guardar coordenadas
# Función para obtener latitud y longitud con OpenStreetMap
obtener_coordenadas <- function(localidad) {
  tryCatch({
    # Intentar obtener coordenadas con "Argentina" en el nombre
    resultado <- getbb(localidad, display_name_contains = "Argentina")
    # Si no encuentra resultado, intentar sin "Argentina"
    if (is.null(resultado)) {
      resultado <- getbb(localidad)
    }
    # Retornar el bounding box directamente
    return(resultado)
  }, error = function(e) {
    return(NULL)  # Si hay un error, retornar NULL
  })
}

coordenadas_diccionario <- list()
# Iterar sobre las localidades y obtener coordenadas
for (localidad in diccionario_localidades) {
  coordenadas_diccionario[[localidad]] <- obtener_coordenadas(localidad)
  print(localidad)
}

# Reemplazamos las coordenadas de los siguientes lugares con las coordenadas correctas
coordenadas_diccionario$LUJAN <- data.frame(
  x = c(min = -59.11633267276459, max = -59.11633267276459),  # Longitud correcta de Luján
  y = c(min = -34.54951531220433, max = -34.54951531220433)     # Latitud correcta de Luján
)
coordenadas_diccionario$'TRES DE FEBRERO'<- data.frame(
  x = c(min = -58.550447849613185, max = -58.550447849613185),  # Longitud correcta de 3 de feb
  y = c(min = -34.60486795154811, max = -34.61203058911919)     # Latitud correcta de 3 de feb
)
coordenadas_diccionario$SALTA<- data.frame(
  x = c(min =-65.41118663384584, max = -65.41118663384584),  # Longitud correcta de Salta
  y = c(min = -24.78271719785891, max = -24.78271719785891)     # Latitud correcta de Salta
)
coordenadas_diccionario$JUJUY<- data.frame(
  x = c(min =-65.2959422637326, max = -65.2959422637326),  # Longitud correcta de Jujuy
  y = c(min = -24.184670835949156, max = -24.184670835949156)     # Latitud correcta de Jujuy
)



conicet <- conicet %>%
  rowwise() %>%
  mutate(
    lon = mean(coordenadas_diccionario[[LOCALIDAD]]["x", ]),  # Promedio de las longitudes
    lat = mean(coordenadas_diccionario[[LOCALIDAD]]["y", ])   # Promedio de las latitudes
  )

# Contar cuántas veces aparece cada localidad
conicet <- conicet %>%
  group_by(LOCALIDAD) %>%
  mutate(count = n()) %>%  # Agregar una nueva columna 'count' con el número de filas por localidad
  ungroup() %>%
  mutate(log_count = log10(count)) # o log para base natural # Escalar el conteo usando logaritmo

# Agregar una nueva columna 'region' a la base de datos 'conicet'
conicet <- conicet %>%
  mutate(region = case_when(
    PROVINCIA == "BUENOS AIRES" ~ "Buenos Aires",
    PROVINCIA == "CABA" ~ "CABA",
    TRUE ~ "Resto del país"  # Todas las demás provincias se agrupan aquí
  ))



# Agregar una nueva columna 'Nombre_comision' a 'conicet'


conicet <- conicet %>% 
  mutate(Nombre_comision = case_when(
    
    DISCIPLINA.CODIGO == "KA4" ~ "Informática",
    DISCIPLINA.CODIGO == "KB1" ~ "Medicina",
    DISCIPLINA.CODIGO == "KB2" ~ "Biología",
    DISCIPLINA.CODIGO == "KB3" ~ "Bioquímica",
    DISCIPLINA.CODIGO == "KS7" ~ "Psicología y Ciencias de la Educación",
    DISCIPLINA.CODIGO == "KS8" ~ "Antropología Biológica",
    DISCIPLINA.CODIGO == "KS9" ~ "Ciencias Antropológicas"
    
  ))




# Suponiendo que 'conicet' tiene una columna 'TIPO' para el tipo de proyecto y 'AÑO' para el año.
# Asegurarse de que no haya valores NA en las columnas 'AÑO' y 'TIPO'
 # Filtrar filas sin valores en 'AÑO' o 'TIPO'
# write.csv(conicet, "D:/concurso_contar_con_datos/conicet_preprocesado.csv", row.names = FALSE) #ubicación Flor
#write.csv(conicet, "C:/Users/usuario/Desktop/Concurso_Contar_con_Datos/conicet_preprocesado.csv", row.names = FALSE) # Fer
write.csv(conicet, "data/conicet_preprocesado.csv", row.names = F)

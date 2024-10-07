# Ciencia sobre crecer, al alcance de todos

CONICET, el principal organismo de ciencia y tecnología del país, financia diversas temáticas de investigación. Sin embargo, el acceso a esta información pública es limitado.

Con base en datos sobre proyectos financiados en desarrollo infantil, hemos creado un tablero interactivo que fomenta la colaboración y el diálogo entre investigadores. Esta herramienta puede servir de modelo para mejorar la accesibilidad a investigaciones en otros campos, fortaleciendo la comunidad científica argentina.

En las últimas dos décadas, el desarrollo infantil ha cobrado relevancia en la agenda política argentina, impulsado por políticas científicas que abordan problemas sociales. Un estudio de la Unidad de Neurobiología Aplicada (UNA, CEMIC-CONICET) analizó el impacto de estas políticas en la ciencia, revelando un creciente interés en estos temas. Los datos están disponibles para exploración interactiva.

El artículo original puede leerse [aquí](https://ojs.revistacts.net/index.php/CTS/article/view/410)

## Organización del Repo
Este repositorio contiene un tablero interactivo que proporciona información valiosa sobre proyectos financiados en el área del desarrollo infantil. El tablero se ha desarrollado con el objetivo de fomentar redes de trabajo, colaboración y diálogo entre quienes investigan en este campo. La estructura del repositorio es la siguiente:

- **code/**: Esta carpeta contiene los scripts utilizados para procesar los datos y para construir la aplicación Shiny.
  - **preprocessing.R**: Código para el preprocesamiento de los datos.
  - **shiny_app.R**: Código para la creación de la aplicación Shiny.

- **data/**: Esta carpeta incluye dos conjuntos de datos.
  - **raw_data.csv**: Base de datos sin procesar.
  - **processed_data.csv**: Base de datos procesada.

## Cómo Ejecutar el Tablero
1. Clona este repositorio: 
   ```bash
   git clone https://github.com/AgendasCientificas/nombre_del_repositorio.git

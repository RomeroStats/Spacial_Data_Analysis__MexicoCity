# Spacial_Data_Analysis__MexicoCity
Este repositorio contiene código que forma parte del taller de R **Análisis de datos con enfoque espacial** del seminario interinstitucional: *Instituciones de seguridad y justicia* del Instituto Mora, CIDE, IIS - UNAM y el Colegio Mexiquense.

## Descripción
El script descarga datos abiertos de víctimas de la Fiscalía General de Justicia (FGJ) de la Ciudad de México correspondientes al año 2024. Posteriormente, realiza un filtrado espacial para mapear interactivamente el delito de "ROBO A CASA HABITACION SIN VIOLENCIA" utilizando la librería `leaflet`.

Puedes visualizar el mapa en el siguiente enlace: 
https://rpubs.com/Zanza/spacial_analysis_CDMX

## Requisitos
Para ejecutar este código, necesitas tener instalados los siguientes paquetes en R:
- `tidyverse` (para manipulación de datos)
- `leaflet` (para la visualización de mapas interactivos)

Puedes instalarlos ejecutando:
\`\`\`R
install.packages(c("tidyverse", "leaflet"))
\`\`\`

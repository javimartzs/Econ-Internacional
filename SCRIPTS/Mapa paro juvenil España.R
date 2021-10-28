#PRIMERO CARGAMOS LAS LIBRERIAS QUE VAMOS A USAR PARA EL PROYECTO
library(tidyverse)
library(mapSpain)
library(ggrepel)

#CARGAMOS LA BASE DE DATOS DE DESEMPLEO DE LA EPA
paro.ca <- read.csv("DATOS/INE/Desempleo por CCAA y grupos de edad.csv")

#CREAMOS UN NUEVO DF CON EL PERIODO Y EL CODIGO DE LA COMUNIDAD SEPARADOS
paro_clean <- paro.ca %>% separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  separate(Comunidades.y.Ciudades.Autónomas, into = c("codauto", "comunidades"), sep = 2)

#FILTRAMOS EL DF POR LAS VARIABLES QUE VAMOS A USAR
paro_plot <- paro_clean %>% filter(año == "2020" & Sexo == "Ambos sexos" & Edad == "Menores de 25 años")

#CALCULAMOS LA MEDIA DE LA COLUMNA VALUE Y POSTERIORMENTE CREAMOS UNA COLUMNA QUE CATEGORIZE A LAS CCAA 
#POR ENCIMA O POR DEBAJO DE LA MEDIA CON LA FUNCION CUT
paro_plot <- paro_plot %>% mutate(media = mean(value)) %>% 
  mutate(categoria = cut(paro_plot$value, breaks = c(0, 40.13026, 100),
                                          labels = c("Por debajo de la media", "Por encima de la media")))

#CARGAMOS LOS DATOS GEOGRAFICOS CON EL PAQUETE MAPSAPIN
ca_plot <- esp_get_ccaa()

#UNIMOS LOS DATAFRAMES CON LA FUNCION LEFT_JOIN
plot <- ca_plot %>% left_join(paro_plot)

#CREAMOS UN CENTROIDE PARA DESAGREGAR LAS LONG Y LAT DE LA VARIABLE GEOMETRY
plot <- plot %>% mutate(centroid = map(geometry, st_centroid), coords = map(centroid, st_coordinates), coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords, 2))

#ELIMINAMOS LOS DATAFRAMES QUE NO NECESITAMOS DEL ENVIRONMENT
rm(ca_plot, paro_clean, paro_plot, paro.ca)

#FILTRAMOS POR EL TRIMESTRE QUE VAMOS A GRAFICAR Y CREAMOS LA FIGURA CON GGPLOT Y GEOM_SF
plot %>% filter(trimestre == "T1") %>% 
  ggplot(aes(fill = categoria))+
  geom_sf(color = "white", size = .2)+
  
#CAMBIAMOS EL NOMBRE DE LA LEYENDA Y LOS COLORES DEL RELLENO (FILL) 
  scale_fill_manual(name = "Posicion respecto a la media",
                    values = c("cornflowerblue", "indianred2"))+
  
#AÑADIMOS EL TITULO, EL SUBTITULO Y CAPTION
  labs(title = "Desempleo juvenil por Comunidades autonomas en el 1º trimestre de 2020",
       subtitle = "Situacion sobre la media anual (40,13%)",
       caption = "Javier Martínez (@javimartzs) - Datos: INE")+

#EDITAMOS LOS ATRIBUTOS DEL GRAFICO CON LA FUNCION THEME A GUSTO PERSONAL
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 15, hjust = 0.5, color = "grey20"),
    plot.caption = element_text(size = 10, color = "grey20"),
    legend.title = element_text(color = "grey20", size = 15),
    legend.text = element_text(color = "grey20", size = 11, hjust = 0),
    legend.position = c(0.85, 0.3),
    legend.text.align = 0)

  
  
  
  
  
  
  
  
  


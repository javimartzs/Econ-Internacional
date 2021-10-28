#CARGAMOS LIBRERIAS
library(tidyverse)
library(readr)
library(viridis)
library(viridisLite)

#CARGAMOS DATFRAME Y MODIFICAMOS EL TIPO DE COLUMNAS
UE <- read_csv("../ECON INTERNACIONAL/DATOS/paises/Riesgo pobreza (educ).csv", na = ":", col_types = cols(TIME = col_date(format = "%Y"), Value = col_number()))
UE <- UE %>% filter(ISCED11 == "All ISCED 2011 levels" & AGE == "18 years or over" & SEX == "Total" & TIME == "2019-01-01")



#CAMBIAMOS EL NOMBRE A DETERMINADOS CARACTERES
UE$GEO[UE$GEO == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
UE$GEO[UE$GEO == "European Union - 28 countries (2013-2020)"] <- "UE-28"
UE$GEO[UE$GEO == "North Macedonia"] <- "Macedonia"

#ELIMINAMOS FILAS QUE NO NECESITAMOS
UE <- na.omit(UE)
UE <- UE[-1, ]
UE <- UE[-1, ]
UE <- UE[-2, ]
UE <- UE[-2, ]    
UE <- UE[-2, ]    



#CREAMOS GGPLOT Y AÑADIMOS GEOM_BAR Y GEOM_TXT
ggplot(UE, aes(x = reorder(GEO,-Value), y = Value, fill = GEO))+
  geom_bar(stat = "identity", width = 0.7)+ 
  geom_text(aes(label = Value), vjust = -0.4, size = 3)+

#AÑADIMOS TITULO, SUBTITULO Y FUENTE
  labs(title = "Tasa de riesgo de pobreza en 2019",
       subtitle = "Porcentaje sobre el total de la población",
       caption = "Javier Martinez (@javimartzs)  Datos: Eurostat")+
  
#EDITAMOS LAS ETIQUETAS DE LOS EJES, LAS ESCALAS Y LOS COLORES
  ylab("")+
  xlab("")+
  guides(fill = F)+
  scale_fill_manual(values = c("dodgerblue4", "dodgerblue4","dodgerblue4","dodgerblue4","dodgerblue4",
                               "dodgerblue4","dodgerblue4","dodgerblue4","dodgerblue4","dodgerblue4",
                               "dodgerblue4","dodgerblue4","dodgerblue4","dodgerblue4","dodgerblue4",
                               "dodgerblue4","dodgerblue4","dodgerblue4","dodgerblue4","dodgerblue4",
                               "dodgerblue4","dodgerblue4","dodgerblue4","dodgerblue4","dodgerblue4",
                               "dodgerblue4","dodgerblue4","dodgerblue4","dodgerblue4","red",
                               "dodgerblue4","dodgerblue4","dodgerblue4","lightslateblue","dodgerblue4",
                               "dodgerblue4","dodgerblue4","dodgerblue4","dodgerblue4"))+
#DISEÑAMOS NUESTRO TEMA (EJES, FONDOS, POSICIONES...)
  theme(axis.text.x = element_text(angle = 50, vjust = 0.7),
        axis.line.x = element_line( linetype = 1, color = "black", size = .5),
        axis.line.y = element_line( linetype = 1, color = "black", size = .5),
        panel.grid.major.y = element_line(color = "white", size = 0.4),
        panel.grid.minor.y = element_line(color = "white", size = 0.4),
        panel.grid.major.x = element_line(color = "white", size = 0.1),
        panel.grid.minor.x  = element_blank(),
        panel.background = element_rect (fill = "snow2"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(size = 15, hjust = 0, face = "bold", color = "grey20"),
        plot.subtitle = element_text(size = 11, hjust = 0, color = "grey40"),
        plot.caption = element_text(size = 10, hjust = 0.9, color = "grey40"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 13))


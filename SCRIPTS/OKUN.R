#CARGAMOS LIBRERIAS
library(tidyverse)
library(readr)
library(TTR)
library(viridis)
library(viridisLite)

#CARGAMOS LOS DATOS DEL PIB y PARO DE EUROSTAT Y CONVERTIMOS LA VARIABLE TIME EN DATE
pib <- read_csv("../ECON INTERNACIONAL/DATOS/paises/PIB .csv", na = ":", col_types = cols(TIME = col_date(format = "%Y")))
paro <- read_csv("../ECON INTERNACIONAL/DATOS/paises/Desempleo (sex,age,educ).csv", na = ":", col_types = cols(TIME = col_date(format = "%Y")))

#CAMBIAMOS EL NOMBRE A ALEMANIA PARA TRABAJAR MAS COMODAMENTE 
pib$GEO[pib$GEO == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
paro$GEO[paro$GEO == "Germany (until 1990 former territory of the FRG)"] <- "Germany"

#ELIMINAMOS LOS VALORES NAs
pib <- na.omit(pib)
paro <- na.omit(paro)

#FILTRAMOS POR PAISES, FECHA  Y UNIDAD
pib_clean <- pib %>% filter(GEO %in% c("Germany", "Spain", "Portugal") & TIME >= "1995-01-01" & UNIT == "Current prices, million euro")
paro_clean <- paro %>% filter(TIME >= "1995-01-01" & GEO %in% c("Spain", "Germany", "Portugal") & SEX == "Total" & AGE == "From 15 to 74 years" & ISCED11 == "All ISCED 2011 levels")
  
#ELIMINAMOS LAS COLUMNAS QUE NO NECESITAMOS
pib_clean$UNIT <- NULL
pib_clean$NA_ITEM <- NULL

paro_clean$UNIT <- NULL
paro_clean$SEX <- NULL
paro_clean$AGE <- NULL
paro_clean$ISCED11 <- NULL

#CONVERTIMOS LA VARIABLE GEO EN DOS VARIABLES DIFERENTES CON PIVOT_WIDER (SPAIN & GERMANY) 
#PARA PODER CALCULAR LAS VARIACIONES DE CADA PAIS CON LA FUNCION ROC
pib_clean_long <- pib_clean %>% pivot_wider(names_from = "GEO", values_from = "Value") %>% 
  mutate(Spain = ROC(Spain, n = 1, type = "discrete")*100) %>% 
  mutate(Germany = ROC(Germany, n = 1, type = "discrete")*100) %>% 
  mutate(Portugal = ROC(Portugal, n = 1, type = "discrete")*100)


paro_clean_long <- paro_clean %>% pivot_wider(names_from = "GEO", values_from = "Value") %>% 
  mutate(Spain = Spain - lag(Spain, 1)) %>% 
  mutate(Germany = Germany - lag(Germany, 1)) %>% 
  mutate(Portugal = Portugal - lag(Portugal, 1))
  


#VOLVEMOS A CONVERTIR LOS DATOS EN UN DATAFRAME VERTICAL CON PIVOT_LONGER
pib_clean_good <- pib_clean_long %>% pivot_longer(cols = Germany:Portugal, names_to = "GEO", values_to = "V.pib")

paro_clean_good <- paro_clean_long %>% pivot_longer(cols = Germany:Portugal, names_to = "GEO", values_to = "V.paro")


#ELIMINAMOS LOS VALORES NAs
pib_clean_good <- na.omit(pib_clean_good)
paro_clean_good <- na.omit(paro_clean_good)


#ELIMINAMOS LOS DATAFRAMES QUE FUIMOS CREANDO HASTA QUEDARNOS CON EL FINAL
rm("pib", "pib_clean", "pib_clean_long")
rm("paro", "paro_clean", "paro_clean_long")



#UNIMOS LOS DATAFRAME 

Okun <- pib_clean_good %>% left_join(paro_clean_good)
Okun <- na.omit(Okun)
Okun$GEO <- as.factor(Okun$GEO)

rm("paro_clean_good", "pib_clean_good")


#CREAMOS UN OBJETO GGPLOT CON SUS VARIABLRES
ggplot(Okun, aes(x = V.paro, y = V.pib, color = GEO))+
  
#AÑADIMOS LOS PUNTOS DE LAS VARIABLES Y UNA LINEA DE TENDENCIA POR FACTOR
  geom_point(size=7, shape = 18)+
  stat_smooth(se=F, method = "lm")+
  
#ESTBALECEMOS DOS LINEAS QUE CORTEN LOS EJES EN (0,0)
  geom_vline(xintercept = 0, linetype = 2, size = .5)+
  geom_hline(yintercept = 0, linetype = 2, size = .5)+
  
#ESCRIBIMOS TECXTO DONE QUERAMOS
  annotate("text", x=6.2, y=-3.3, label="2009", hjust="left")+
  annotate("text", x=0.4, y=-3.7, label="2009", hjust="left")+
  annotate("text", x=3, y=-4.2, label="2012", hjust="left")+
  
#ESCRIBIMOS LOS TITULOS Y EL NOMBRE DE LOS EJES
  labs(title = "Relacion entre variación anual del PIB Real y la variación anual de la Tasa de Paro",
       subtitle = "En Alemania, España y Portugal desde 1996 a 2019",
       x = "Variación anual de la tasa de paro")+
  ylab("Variación anual del PIB real")+
  
#USAMOS VIRIDIS PARA LOS COLORES
  scale_color_viridis_d()+

#AÑADIMOS NUESTRO TEMA PREDISEÑADO
  theme(axis.title.y = element_text(vjust = 0, face = "bold"),
        axis.title.x.bottom = element_text(vjust = 0, face = "bold"),
        axis.text.x = element_text(angle = 0, vjust = 0.7),
        axis.line.x = element_line( linetype = 1, color = "black", size = .5),
        axis.line.y = element_line( linetype = 1, color = "black", size = .5),
        panel.grid.major.y = element_line(color = "grey", size = 0.3),
        panel.grid.minor.y = element_line(color = "grey", size = 0.3),
        panel.grid.major.x = element_line(color = "grey", size = 0.1),
        panel.grid.minor.x  = element_blank(),
        panel.background = element_rect (fill = "whitesmoke"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(size = 15, hjust = 0, face = "bold", color = "grey20"),
        plot.subtitle = element_text(size = 11, hjust = 0, color = "grey40"),
        plot.caption = element_text(size = 10, hjust = 0.9, color = "grey40"),
        legend.position = c(0.9, 0.79),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 13),
        legend.background = element_blank(),
        legend.key = element_rect(fill = "whitesmoke"))
#DEJAMOS AQUI THEME_CLASSIC PARA COMPRARLO SI QUEREMOS CON NUESTRO TEMA
  theme_classic()
  
  
  












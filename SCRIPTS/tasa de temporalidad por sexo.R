#CARGAMOS LA LIBRERIAS QUE NECESITAMOS
library(tidyverse) #que hacer sin tidyverse
library(pxR) #esta la uso para cargar los datos que me descargo del ine direcatmente en formato pc-axis

#CARGO EL FICHERO DEL INE CON READ.PX Y LUEGO CON %>% AS_TIBBLE LE DAMOS FORMA DE DATAFRAME 
contrato <- read.px("../../../Downloads/4241 (1).px")%>% as_tibble()

#CONVIERTO LOS NOMBRES EN CARACTERES PARA PODER AÑADIR EL CODIGO 00 AL TOTAL NACIONAL PARA QUE AL SEPARARLOS SE ME SEPARE BIEN AUNQUE EN ESTE CASO YA SOLO ME DESCARGUE DATOS NACIONALES
contrato$Comunidades.y.Ciudades.Autónomas <- as.character(contrato$Comunidades.y.Ciudades.Autónomas)
contrato$Comunidades.y.Ciudades.Autónomas[contrato$Comunidades.y.Ciudades.Autónomas == "Total Nacional"] <- "00 España"

contrato <- contrato %>%
  separate(Comunidades.y.Ciudades.Autónomas, into = c("codauto", "name"), sep = 2) %>% #SEPARO DIEMPRE LOS CODIGOS DEL NOMBRE DE LA CCAA POR SI ME DA POR MAPEARLO EN ALGUN MOMENTO
  separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% #SEPARO EL AÑO DEL TRIMESTRE PARA PODER CONVERTIRLO EN FORMATO FECHA
  mutate(trimestres = trimestre) #CREO UNA NUEVA VARIABLE PARA NO PERDER LOS TRMIESTRES QUE SON CADA UNO

#RECODIFICO LA PRIMERA COLUMNA DE TRIMESTRES 
contrato$trimestre[contrato$trimestre == "T4"] <- "/09/01" #T4 = 09/01
contrato$trimestre[contrato$trimestre == "T3"] <- "/06/01" #T3 = 06/01
contrato$trimestre[contrato$trimestre == "T2"] <- "/03/01" #T2 = 03/01
contrato$trimestre[contrato$trimestre == "T1"] <- "/01/01" #T1 = 01/01


#AHORA UNO LAS COLUMNAS DE AÑO Y EL TRIMESRE QUE CONVERTIMOS EN FECHA 
contrato <- contrato %>% unite(periodo,c(1:2),  sep = "", remove = TRUE)
#Y LO COVERTIMOS EN FORMATO DATE.CHARACTER
contrato$periodo <- as.Date.character(contrato$periodo, format = "%Y/%m/%d")

#CAMBIO EL NOMBRE DE LA VARIABLE PARA QUE SEA MAS CORTA
names(contrato)[names(contrato) == 'Tipo.de.contrato.o.relación.laboral'] <- 'contrato'

#CREAMOS EL DF GRAFICO Y LO FILTRO POR CONTRATO TEMPORAL TERCER TRIMESTRE, 
#PERIODO A PARTIR DEL CUAL EMPIEZA EL GRAFICO Y ELIMINAMOS AMBOS SEXOS PARA QUE SOLO NOS PINTE "HOMBRE Y MUJER"
grafico <- contrato %>% filter(contrato == "Temporal: Total" & codauto == "00" & trimestres == "T3" & 
                                 periodo >= "2006-06-01" & Sexo != "Ambos sexos")

#DEFINIMOS LOS PARAMETROS DENTRO DE GEOM_LINE
 ggplot(grafico)+ geom_line(aes(periodo, value, color = Sexo),
                            size = 1.2)+ #grosor de la linea
   scale_y_continuous(breaks = seq(20,40,by=5))+ #escala del eje y
   scale_x_date(date_breaks = "1 year", 
                date_labels = format("%Y"))+ #escala del eje x
   scale_color_manual(values = c("#419461" ,"#8F428B"))+ #colores de las lineas
   labs(title = "Tasa de temporalidad por género",
        subtitle = "Tercer trimestre de cada año",
        x = "", #elimino xlab
        y = "", #elimino ylab
        color = "Género:")+ #nombre de la leyenda
   theme_minimal()+ #este lo cargo siempre y a partir de ahi lo edito
   
   theme(panel.background = element_rect (fill = NULL, color = "black", size = 1.5), #color es el cuadrado que rodea el grafico
         panel.grid.major = element_line(color = "grey80", size = 0.2), #lineas internas que van desde las referencias de los ejes
         panel.grid.minor = element_blank(), #lineas de referencia que van entre las referencias de los ejes y las elimino con element_blanck
         plot.title = element_text(size = 15, hjust = 0.5, face = "bold", color = "grey20"), #modifico los parametros del titulo 
         plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40"), #y subtitulo
         legend.position = "bottom", #coloco abajo la leyenda
         legend.background = element_rect(), #la rodeo del mismo cuadrado que al grafico
         legend.title = element_blank(), #aqui le quite el nombre a la leyenda porque era bastante claro 
         legend.text = element_text(color = "black", size = 13)) #edito el texto de la leyenda
   
 
#ESTOS ATRIBUTOS NO LOS USE EN ESTE GRAFICO PERO TE LO DEJO POR AQUI PARA QUE VEAS QUE ESTOS SON 
#LAS FUNCIONES QUE MAS USO PARA EDITAR LOS GRAFICOS 
 
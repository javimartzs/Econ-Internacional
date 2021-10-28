# packages
library(tidyverse)
library(sf)
library(biscale)

nivel <- read.csv("DATOS/INTERNACIONAL/NUTS/nivel.csv", na = ":")
names(nivel)[names(nivel) == "GEO"] <- "NUTS_ID"
names(nivel)[names(nivel) == "Value"] <- "nivel"
nivel$GEO_LABEL <- NULL
nivel$TIME <- NULL
nivel$UNIT <- NULL
nivel_clean <- nivel %>% filter(SEX == "Total" & AGE == "From 25 to 64 years" & ISCED11 == "Tertiary education (levels 5-8)")


empleo <- read.csv("tasa empleo educ.csv", na= ":")
names(empleo)[names(empleo) == "GEO"] <- "NUTS_ID"
names(empleo)[names(empleo) == "Value"] <- "empleo"
empleo$TIME <- NULL
empleo$UNIT <- NULL
empleo$AGE <- NULL
empleo$CITIZEN <- NULL

empleo_clean <- empleo %>% filter(SEX == "Total" & ISCED11 == "Tertiary education (levels 5-8)")


grupo <- nivel_clean %>% left_join(empleo_clean)


rm(empleo, empleo_clean, nivel, nivel_clean)

map <- read_sf("NUTS_RG_03M_2021_3035_LEVL_2.shp/NUTS_RG_03M_2021_3035_LEVL_2.shp")


mapa <- map %>% left_join(grupo, by = "NUTS_ID")
mapa <- mapa %>% filter(NUTS_ID != "FRY1") %>% 
  filter(NUTS_ID != "FRY2") %>% filter(NUTS_ID != "FRY3") %>% filter(NUTS_ID != "FRY4") %>% 
  filter(NUTS_ID != "FRY5") %>%  filter(NUTS_ID != "ES70") %>%  filter(NUTS_ID != "PT20") %>% 
  filter(NUTS_ID != "PT30") %>% filter(NUTS_ID != "NO0B")


databi2  <- bi_class ( mapa, nivel, empleo, style  =  "quantile" , dim  =  3) %>% 
  mutate(bi_class = ifelse (str_detect ( bi_class , "NA" ), NA , bi_class ))

databi2 <- databi2 %>% mutate(centroid = map(geometry, st_centroid), coords = map(centroid, st_coordinates), coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords, 2))

legend2  <- bi_legend(pal  =  "DkViolet",
                      dim  =  3 ,
                      xlab  =  "Más estudios superiores",
                      ylab  =  "Más empleo superior",
                      size  =  10)

ggplot(databi2) + 
  geom_sf(aes(fill = bi_class), 
          colour = NA, 
          size = .1, 
          show.legend = FALSE) +
  bi_scale_fill(pal = "DkViolet", 
                dim = 3, 
                na.value = "gray43") +
  labs(title = "Education and Employment in Europe in 2019",
       subtitle = "Employment rate for population with higher education and 
percentage of population with higher education",
       caption = "Source: Eurostat - Made by javier martinez (@javimartzs)")+
  bi_theme()+
  annotation_custom(ggplotGrob(legend2), 
                    xmin = 5700000, xmax = 7000000,
                    ymin = 3000000, ymax = 4550000)+
  theme(plot.title = element_text(size = 25, face = "bold", vjust  = 0.5),
        plot.subtitle = element_text(size = 15, face = "plain", vjust = 0),
        plot.caption = element_text(size = 12, face = "plain"))













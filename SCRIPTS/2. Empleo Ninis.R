library(tidyverse)
library(sf)
library(viridis)
library(viridisLite)


empleo <- read.csv("Datos/empleo ninis.csv", na = ":")
empleo.2 <- empleo %>% filter(SEX == "Total", AGE == "From 15 to 24 years", TIME == "2019")
names(empleo.2)[names(empleo.2) == "GEO"] <- "NUTS_ID"


mapa <- read_sf("../PROYECTOS/ECON INTERNACIONAL/NUTS_RG_03M_2021_3035_LEVL_2.shp/NUTS_RG_03M_2021_3035_LEVL_2.shp")

grupo <- mapa %>% left_join(empleo.2, by = "NUTS_ID")
grupo <- grupo %>% filter(NUTS_ID != "FRY1") %>% 
  filter(NUTS_ID != "FRY2") %>% filter(NUTS_ID != "FRY3") %>% filter(NUTS_ID != "FRY4") %>% 
  filter(NUTS_ID != "FRY5") %>%  filter(NUTS_ID != "PT20") %>% 
  filter(NUTS_ID != "PT30") %>% filter(NUTS_ID != "NO0B")

#CUANTILES------------------
quantiles <- quantile(grupo$Value, prob = seq(0, 1, length = 9), na.rm = T)

labels <- c()
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " – ", 
                             round(quantiles[idx + 1], 2)))}

labels <- labels[1:length(labels)-1]
grupo$Value_q <- cut(grupo$Value, breaks = quantiles, labels = labels, include.lowest = T)



#COMO ACERCAR CANARIAS -----
canarias <- grupo%>%
  filter(NUTS_ID == "ES70")

canarias_new <- st_sf(st_drop_geometry(canarias),
                      geometry = st_geometry(canarias) + c(550000, 520000))

st_crs(canarias_new) <- st_crs(grupo)

grupo2 <- rbind( grupo %>%
                         filter(NUTS_ID != "ES70"),
                       canarias_new)


#MAPA -----
ggplot(grupo2)+
  geom_sf(aes(fill = Value_q),
          color = "grey",
          size = .1)+
  scale_fill_viridis_d(option = "B", alpha = 0.9,
                     direction = -1,
                     name = "%",
                     #label = c(5.4,6.7,8.4,9.7,11.3,13.7,19.7,42, "NA"),
                     na.value ="grey70",
                     guide = guide_legend(
                       direction = "horizontal",
                       keyheight = unit(2, units = "mm"),
                       keywidth = unit(180 / length(labels), units = "mm"),
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5,
                       nrow = 1,
                       byrow = T,
                       label.position = "bottom"))+
  labs(title = "Jóvenes que no tienen empleo ni reciben educación y formación",
       subtitle = "Por regiones NUTS-2 en 2019")+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold", hjust  = 0.5, color = "#40227D"),
        plot.subtitle = element_text(size = 15, face = "plain", hjust = 0.5, color = "#40227D"),
        legend.position = "bottom",
        legend.text = element_text(size = 10))
  



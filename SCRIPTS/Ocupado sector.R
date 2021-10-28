library(tidyverse)
library(pxR)
library(viridisLite)
library(viridis)


library(knitr)
library(kableExtra)


ocupados <- read.px("../../../Downloads/4130.px") %>% as.tibble() %>% 
  filter(Sexo == "Ambos sexos", Edad == "Total", Rama.de.actividad.CNAE.2009 != "Total") %>% 
  separate(Periodo, into = c("año", "trimestre"), sep = 4) %>% 
  separate(Rama.de.actividad.CNAE.2009, into = c("CNAE", "actividad"), sep = 1) %>% 
  mutate(año = case_when(
    año == "2019" ~ "año19",
    año == "2020" ~ "año20"))
ocupados <- ocupados %>% unite(periodo, c(1:2), sep = "", remove = TRUE)


diferenciaT4 <- ocupados %>% filter(periodo %in% c("año19T4", "año20T4")) %>%
  pivot_wider(id_cols = CNAE, names_from = periodo, values_from = value) %>% 
mutate(dif = año20T4 - año19T4,
       brecha = ifelse(dif <=0, 1, 0))

  ggplot(diferenciaT4)+
  geom_col(aes(x=dif, y = reorder(CNAE, dif), fill = factor(brecha)))+
    scale_x_continuous(limits = c(-400, 100))+
    scale_fill_viridis_d(option = "D")+
  guides(fill= FALSE)+
  labs(y = NULL,
       x = "Número de ocupados (miles de personas)",
       title = "Variación de ocupados por rama de actividad 2019 vs 2020",
       subtitle = "4º trimestre",
       caption = "@javimartzs - Datos: INE")+
    theme_minimal()+
    theme(axis.text.y = element_text(size = 10, face = "bold"),
          axis.text.x = element_text(size = 10, face = "bold", vjust = 1),
          axis.title.x = element_text(size = 15, vjust = 0, hjust = 0.5),
          plot.title = element_text(size = 15, hjust = 0.5, face = "bold", color = "grey20"),
          plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40"),
          plot.caption = element_text(size = 10, hjust = 0.9, color = "grey40"))
      
      
    
    

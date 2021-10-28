library(tidyverse)
library(pxR)
library(patchwork)


vivienda <- read.px("../../../Downloads/9994-2.px") %>% as.tibble() %>% 
  mutate(sexo = Sexo.de.la.persona.de.referencia)
vivienda$Régimen.de.tenencia.de.la.vivienda.principal <- as.character(vivienda$Régimen.de.tenencia.de.la.vivienda.principal)
vivienda$Régimen.de.tenencia.de.la.vivienda.principal[vivienda$Régimen.de.tenencia.de.la.vivienda.principal == "Alquiler a precio de mercado"] <- "Alquiler"

  
  
año19 <- vivienda %>% filter(Periodo == "2019")
p2 <- ggplot(año19)+ geom_bar(aes(x= Régimen.de.tenencia.de.la.vivienda.principal, y = value, 
                               fill = Régimen.de.tenencia.de.la.vivienda.principal), stat = "identity")+
  geom_text(aes(x= Régimen.de.tenencia.de.la.vivienda.principal, y = value, label = value), vjust = 1.1, size = 5)+
  facet_wrap(año19$Edad.de.la.persona.de.referencia, scales = "free_x")+
  guides(fill=FALSE)+
  labs(x="", y="",
       title = "2019")+
  scale_fill_manual(values = c("#3a7df2", "#c6333b"))+
  theme_minimal()+
  theme(strip.text = element_text(size = 17, vjust = -0.5),
        axis.text.x = element_text(face = "bold", size = 10),
        plot.title = element_text(size = 17, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 15, hjust = 0.5, color = "grey20"))
  
año13 <- vivienda %>% filter(Periodo == "2013")
p1 <- ggplot(año13)+ geom_bar(aes(x= Régimen.de.tenencia.de.la.vivienda.principal, y = value, 
                              fill = Régimen.de.tenencia.de.la.vivienda.principal), stat = "identity")+
    geom_text(aes(x= Régimen.de.tenencia.de.la.vivienda.principal, y = value, label = value), vjust = 1.1, size = 5)+
    facet_wrap(año13$Edad.de.la.persona.de.referencia, scales = "free_x")+
    guides(fill=FALSE)+
    labs(x="", y="",
         title = "2013")+
    scale_fill_manual(values = c("#3a7df2", "#c6333b"))+
    theme_minimal()+
    theme(strip.text = element_text(size = 17, vjust = -0.5),
          axis.text.x = element_text(face = "bold", size = 10),
          plot.title = element_text(size = 17, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5, color = "grey20"))

p <- p1 | p2
p

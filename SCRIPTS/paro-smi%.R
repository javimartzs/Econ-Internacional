library(tidyverse)
library(readr)
library(viridis)
library(viridisLite)

paro <- read_csv("DATOS/INTERNACIONAL/paises/Desempleo (sex,age,educ).csv", col_types = cols(TIME = col_date(format = "%Y")))
paro <- paro %>% filter(TIME == "2019-01-01" & AGE == "From 15 to 74 years" & ISCED11 == "All ISCED 2011 levels" & SEX == "Total")
paro$GEO[paro$GEO == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
paro$GEO[paro$GEO == "United Kingdom"] <- "UK"


smi <- read_csv("DATOS/INTERNACIONAL/paises/SMI %ingresos.csv", na = ":", col_types = cols(TIME = col_date(format = "%Y")))
smi$UNIT <- NULL
smi$`Flag and Footnotes` <- NULL

smi <- smi %>% filter(TIME == "2019-01-01" & 
                      INDIC_SE == "Monthly minimum wage as a proportion of the mean gross monthly earnings"  &
                      NACE_R2 == "Business economy")
smi$GEO[smi$GEO == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
smi$INDIC_SE[smi$INDIC_SE == "Monthly minimum wage as a proportion of the mean gross monthly earnings"] <- "smi como % sobre la media"
smi$GEO[smi$GEO == "United Kingdom"] <- "UK"

smi <- smi %>%left_join(paro, by = "GEO")
smi <- na.omit(smi)
smi$Value.y <- as.numeric(smi$Value.y)


smi %>% 
  ggplot(aes(reorder(GEO, Value.x), Value.x, fill = GEO))+
  geom_bar(stat = "identity", width = 0.6)+
  geom_point(aes(y = Value.y), size = 7, shape = 18)+
  scale_fill_manual(values = c("royalblue", "royalblue","royalblue","royalblue","royalblue",
                               "royalblue","royalblue","royalblue","royalblue","royalblue",
                               "royalblue","royalblue","royalblue","royalblue","royalblue",
                               "royalblue","royalblue","indianred2","royalblue","royalblue"))+
guides(fill = FALSE)+
  ylab("")+
  xlab("")+
  labs(title = "Minimum wage as a proportion of the mean gross earnings and unemployment rate")+
  theme(axis.text.x = element_text(angle = 50, vjust = 0.7, face = "bold"),
        axis.line.x = element_line( linetype = 1, color = "black", size = .5),
        axis.line.y = element_line( linetype = 1, color = "black", size = .5),
        panel.grid.major.y = element_line(color = "white", size = 0.4),
        panel.grid.minor.y = element_line(color = "white", size = 0.4),
        panel.grid.major.x = element_line(color = "white", size = 0.1),
        panel.grid.minor.x  = element_blank(),
        panel.background = element_rect (fill = "snow2"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(size = 15, hjust = .5, face = "bold", color = "grey20"),
        plot.subtitle = element_text(size = 11, hjust = 0, color = "grey40"),
        plot.caption = element_text(size = 10, hjust = 0.9, color = "grey40"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 13))

library(tidyverse)


vat <- read.csv("../PROYECTOS/ECON INTERNACIONAL/DATOS/NUTS/distribucion VAT.csv", na = ":")
vat$GEO_LABEL <- as.character(vat$GEO_LABEL)
vat$GEO_LABEL[vat$GEO_LABEL == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
names(vat)[names(vat) == "Value"] <- 'Pago'
vat <- na.omit(vat)

vat.2 <- vat %>% filter(GEO %in% c("ES", "FI", "DE", "FR", "UK", "PT"), QUANT_INC != "Total")

vat.2$QUANT_INC <- factor(vat.2$QUANT_INC, levels = c("First quintile", "Second quintile", "Third quintile", "Fourth quintile", "Fifth quintile"),
                            labels=c("1", "2", "3", "4", "5"))



ingresos <- read.csv("../PROYECTOS/ECON INTERNACIONAL/DATOS/nama_10_gdp_1_Data.csv", na = ":")
ingresos$GEO_LABEL <- as.character(ingresos$GEO_LABEL)
ingresos$GEO_LABEL[ingresos$GEO_LABEL == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
names(ingresos)[names(ingresos) == 'Value'] <- 'Ingreso'
ingresos <- na.omit(ingresos)

ingresos.2 <- ingresos %>% filter(GEO %in% c("ES", "FI", "DE", "FR", "UK", "PT"))

grupo <- vat.2 %>% left_join(ingresos.2, by = c("GEO", "GEO_LABEL", "TIME"))
grupo$UNIT.x <- NULL
grupo$UNIT.y <- NULL

rm(ingresos, ingresos.2, vat, vat.2)


ggplot(grupo)+
  geom_bar(aes(reorder(QUANT_INC, -Pago), Pago), stat = "identity", width = .7, fill = "#447EAE")+
  geom_line(aes(group = 1,QUANT_INC, Ingreso), linetype=1, size = 1, color = "red")+
  facet_wrap(vars(GEO_LABEL),
             scales = "fixed",
             as.table = F)+
  labs(x="",
       y="",
       title = "IVA pagado por los hogares como porcentaje de sus ingresos brutos por quintil de ingresos",
       subtitle = "Y recaudacion de impuestos sobre productos en relacion al PIB en 2015")+
  theme_minimal()+
  theme(strip.text = element_text(size = 20),
        strip.background = element_rect(color = "black"),
        axis.text.x = element_text(face = "bold", size = 10),
        plot.title = element_text(size = 17, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 15, hjust = 0.5, color = "grey20"))


           
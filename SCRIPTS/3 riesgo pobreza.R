library(tidyverse)

riesgo <- read.csv("Datos/riesgo pobreza.csv", na = ":")
riesgo <- riesgo %>% filter(AGE == "16 years or over", GEO != "MK" & GEO != "TR")
riesgo <- na.omit(riesgo)

total <- riesgo %>% filter(SEX == "Total")
names(total)[names(total) == "SEX"] <- "Total"
names(total)[names(total) == "Value"] <- "valor"

sexos <- riesgo %>% filter(SEX != "Total")
names(sexos)[names(sexos) == "Value"] <- "valor.sex"


ggplot()+
  geom_bar(data = total ,aes(reorder(GEO, valor), valor), stat = "identity", fill = "#447eae")+
  geom_point(data = sexos, aes(GEO, valor.sex, color = SEX), size = 3)+
  theme_minimal()





library(tidyverse)

obes <- read.csv("../../../Downloads/share-of-adults-defined-as-obese.csv")
gdp <- read.csv("../../../Downloads/gdp-per-capita-maddison-2020.csv")

df <- obes %>% left_join(gdp, by = c("Code", "Year", "Entity"))
df$X145446.annotations <- NULL


df %>% filter(Year == 2016) %>% ggplot(aes(Prevalence.of.obesity..both.sexes....WHO..2019., log(GDP.per.capita)))+
  geom_smooth(method = "lm", se=F)+
  geom_point(color="darkblue", size = 3, alpha = 0.9)+
  xlim(0,40)+
  labs(x="% de adultos con obesidad", y="Log de PIB per capita",
       title = "Relación entre el porcentaje de adultos obesos y el PIB per capita en 2016",
       caption = "@javimartzs")+
  theme_minimal()+
  theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
        axis.line = element_line(color = "black"),
        plot.caption = element_text(color = "gray31", size = 10, hjust = 0.9, vjust = 1))
  
  


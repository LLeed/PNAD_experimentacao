library(tidyverse)
library(ggplot2)
library(gridExtra)

dados <- gra_fico_tx_crescimento
mylist_1 <-list("4T/2019", "1T/2020", "2T/2020", "3T/2020",
                "4T/2020", "1T/2021", "2T/2021", "3T/2021")

l <- list()
for (i in seq_along(mylist_1)) {
  dados_1 <- dados %>% filter(Data == mylist_1[[i]])
  
  a <- dados_1 %>% 
    ggplot(aes(x=OC, y=Produtividade)) +
    geom_point(aes(color = Setor,fill = Setor)) +
    geom_text(aes(label=Setor), nudge_x = -0.004, nudge_y = -0.0004) +
    geom_smooth(method=lm, se=F) +
    theme(legend.position = "none") +
    labs(caption = as.character(mylist_1[[i]])) +
    labs(x="Número de Ocupados")+
    theme(plot.caption = element_text(hjust = 0.5)) 
  
  
  assign(paste0("g_", as.character(mylist_1[[i]])),a) -> l[[i]]
}  

grid.arrange(grobs= l, ncol =2, nrow=4)







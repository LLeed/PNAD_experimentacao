library(tidyverse)
library(data.table)
library(ggplot2)
library(gridExtra)

dados <-tx_de_crescimento_dados
categorias <- as.list(tx_de_crescimento_dados[9,-c(1,10:length(dados))])
dados <- tx_de_crescimento_dados[-9,]

produt_reshape <- melt(data = as.data.table(
  dados[,c(1:9)]), 
  id.vars = "Date",
  variable.names = "Produtividade",
  value.name = "Produtividade"
)

produt_reshape$variable<- as.character(categorias) %>% 
  rep(each = 8)

oc_reshape <- melt(data = as.data.table(
  dados[,c(1,10:length(dados))]), 
  id.vars = "Date",
  variable.names = "Produtividade",
  value.name = "OC"
) 
oc_reshape$variable<- as.character(categorias) %>% 
  rep(each = 8)

PRODUT_OC <- (.)

names(PRODUT_OC) [2] <- "Setor"

mylist_1 <-list("1T/2020", "2T/2020", "3T/2020",
                "4T/2020", "1T/2021", "2T/2021", "3T/2021")
datas <- c("1T/2020", "2T/2020", "3T/2020",
          "4T/2020", "1T/2021", "2T/2021", "3T/2021")
  
#### VA
reshape_va <- melt(
  data = as.data.table(tx_de_crescimento_dados_2_),
  id.vars = "VA",
  variable.name = "Date",
  value.name = "Tx. de Crescimento"
)
names(reshape_va)[1] <- "Setor"
names(reshape_va)[3] <- "VA"

l <- list()
l1 <- list()
l2 <- list()
l3 <-list()

correlations <- vector()

for (i in seq_along(mylist_1)) {
  dados_1 <- PRODUT_OC %>% filter(Date == mylist_1[[i]])
  dados_2 <- reshape_va %>% filter(Date == mylist_1[[i]])
  VA <- dados_2$VA
  dados_c <- data.frame(dados_1,VA)
  dados_c$VA <- round(dados_c$VA,2)
  dados_c$OC <- round(dados_c$OC,2)
  dados_c$Produtividade <- round(as.numeric(dados_c$Produtividade)
                                 ,2)
  
  a <- dados_c %>% 
    ggplot(aes(x= OC, y = Produtividade)) +
    geom_point(aes(color = Setor,fill = Setor)) +
    geom_text(aes(label=Setor), nudge_x = -0.004, nudge_y = -0.0004) +
    #geom_smooth(method=lm, se=F) +
    theme(legend.position = "none") +
    labs(caption = as.character(mylist_1[[i]])) +
    labs(x="Taxa de Crescimento do Número de Ocupados",
         y= "Taxa de Crescimento da Produtividade")+
    theme(plot.caption = element_text(hjust = 0.5))
  
  assign(paste0("OC_PRODUT_", as.character(mylist_1[[i]])),a) -> l[[i]]  
  
  b <- dados_c %>% 
    ggplot(aes(x=VA, y=Produtividade)) +
    geom_point(aes(color = Setor,fill = Setor)) +
    geom_text(aes(label=Setor), nudge_x = -0.004, nudge_y = -0.0004) +
    geom_smooth(method=lm, se=F) +
    theme(legend.position = "none") +
    labs(caption = as.character(mylist_1[[i]])) +
    labs(x="Taxa de Crescimento do VA",
         y= "Taxa de Crescimento da Produtividade")+
    theme(plot.caption = element_text(hjust = 0.5))

  assign(paste0("VA_PRODUT_", as.character(mylist_1[[i]])),b) -> l1[[i]]
  assign(paste0("relacao_", as.character(mylist_1[[i]])),c) -> l3[[i]]
   
}

grid.arrange(grobs= l, ncol =2) # Produtividade x OC
grid.arrange(grobs= l1, ncol =2) # Produtividade x VA
#A3	297 x 420 mm	11.7 x 16.5 in



c <- cbind(datas, correlations)

dados_p <- dados[-c(1:8),]
dados_p1 <- dados_p[-c(24:56),]
dados_p2 <- dados_p[c(16:56),]

c1 <- cor(dados_p1$Produtividade,
          y=dados_p1$OC)
c2 <- cor(dados_p2$Produtividade,
          y=dados_p2$OC)

writexl::write_xlsx(as.data.frame(c),"correlações.xlsx")


















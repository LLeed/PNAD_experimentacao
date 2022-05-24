library(tidyverse)
library(data.table)
library(ggplot2)
library(gridExtra)

dados <-tx_de_crescimento_dados # segundo sheet 
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

PRODUT_OC <- data.frame(produt_reshape,oc_reshape)

names(PRODUT_OC) [2] <- "Setor"

mylist_1 <-list("1T/2020", "2T/2020", "3T/2020",
                "4T/2020", "1T/2021", "2T/2021", "3T/2021")
dates <- c("1T/2020", "2T/2020", "3T/2020",
          "4T/2020", "1T/2021", "2T/2021", "3T/2021")
  
#### VA
reshape_va <- melt(
  data = as.data.table(tx_de_crescimento_dados_2_), # primeiro sheet 
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
l4 <-list()


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
    geom_smooth(method=lm, se=F) +
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
 
    
}

grid.arrange(grobs= l, ncol =2) # Produtividade x OC
grid.arrange(grobs= l1, ncol =2) # Produtividade x VA

#A2	420 x 594 mm	16.5 x 23.4 in


#categorias <- as.list(tx_de_crescimento_dados[9,-c(1,10:length(dados))])

l4 <-list()
l5 <-list()

for (i in seq_along(categorias)) {
  dados_1 <- PRODUT_OC %>% filter(Date != "4T/2019")
  dados_1 <- dados_1 %>% filter(Setor == categorias[[i]])
  dados_2 <- reshape_va %>% filter(Setor == categorias[[i]])
  VA <- dados_2$VA
  dados_c <- data.frame(dados_1,VA) %>% filter(Date != "4T/2019")
  dados_c$VA <- round(dados_c$VA,2)
  dados_c$OC <- round(dados_c$OC,2)
  dados_c$Produtividade <- round(as.numeric(dados_c$Produtividade)
                                 ,2)
  
  a <- dados_c %>% 
    ggplot(aes(x= OC, y = Produtividade)) +
    geom_point(aes(color = Date,fill = Date)) +
    geom_text(aes(label=Date), nudge_x = -0.004, nudge_y = -0.0004) +
    geom_smooth(method=lm, se=F) +
    theme(legend.position = "none") +
    labs(caption = as.character(categorias[[i]])) +
    labs(x="Taxa de Crescimento do Número de Ocupados",
         y= "Taxa de Crescimento da Produtividade")+
    theme(plot.caption = element_text(hjust = 0.5))
  
  
  b <- dados_c %>% 
    ggplot(aes(x= VA, y = Produtividade)) +
    geom_point(aes(color = Date,fill = Date)) +
    geom_text(aes(label=Date), nudge_x = -0.004, nudge_y = -0.0004) +
    geom_smooth(method=lm, se=F) +
    theme(legend.position = "none") +
    labs(caption = as.character(categorias[[i]])) +
    labs(x="Taxa de Crescimento do Número de VA",
         y= "Taxa de Crescimento da Produtividade")+
    theme(plot.caption = element_text(hjust = 0.5))
  
  assign(paste0("OC_VA_PRODUT_", as.character(categorias[[i]])),a) -> l4[[i]]  
  
  assign(paste0("VA_PRODUT_", as.character(categorias[[i]])),b) -> l5[[i]]  
}


grid.arrange(grobs= l4, ncol =2) # Produtividade x OC

dados_1 <- PRODUT_OC %>% filter(Date != "4T/2019")
dados_1 <- dados_1 %>% filter(Setor == "ICAFI")
dados_2 <- reshape_va %>% filter(Setor == "ICAFI")
VA <- dados_2$VA
dados_c <- data.frame(dados_1,VA)
dados_c$VA <- round(dados_c$VA,2)
dados_c$OC <- round(dados_c$OC,2)
dados_c$Produtividade <- round(as.numeric(dados_c$Produtividade)
                               ,2)

l5[[5]] <- NA
l5[[5]] <- dados_c %>% 
  ggplot(aes(x= VA, y = Produtividade)) +
  geom_point(aes(color = Date,fill = Date)) +
  geom_text(aes(label=Date)) +
  geom_smooth(method=lm, se=F) +
  theme(legend.position = "none") +
  labs(caption = as.character("ICAFI"), nudge_x = 0.008, nudge_y = 0.0007) +
  labs(x="Taxa de Crescimento do Número de VA",
       y= "Taxa de Crescimento da Produtividade")+
  theme(plot.caption = element_text(hjust = 0.5))



grid.arrange(grobs= l5, ncol =2) # Produtividade x VA



# A3, 297 x 420 mm, 11.7 x 16.5 in



#######correlações

correlations <- list()
correlations_1 <- list()

for (i in seq_along(mylist_1)) {
  dados_1 <- PRODUT_OC %>% filter(Date == mylist_1[[i]])
  dados_2 <- reshape_va %>% filter(Date == mylist_1[[i]])
  VA <- dados_2$VA
  dados_c <- data.frame(dados_1,VA)
  dados_c$Produtividade <- as.numeric(dados_c$Produtividade)
  
  
  assign(paste0("relacao_", as.character(mylist_1[[i]])),dados_c) -> l2[[i]]
  
  cor(x=dados_c$Produtividade, y=dados_c$OC) -> correlations[[i]]
  cor(x=dados_c$Produtividade, y=dados_c$VA) -> correlations_1[[i]]
  
}

correlations <- data_frame(dates, correlations)
names(correlations) [2] <- "Correlações_OC_PRODT" 

correlations_1 <- data_frame(dates, correlations_1)
names(correlations_1) [2] <- "Correlações_VA_PRODT" 

c <- rbindlist(l2)
c <- c[,-c(4,5)]

c1 <- cor(c[(1:24),]$Produtividade,
          y=c[(1:24),]$OC)

c2 <- cor(c[(17:nrow(c)),]$Produtividade,
          y=c[(17:nrow(c)),]$OC)

correlations <- rbind(correlations %>% 
                        as.data.frame(correlations$Correlações_OC_PRODT), 
                      c("1T/2020 a 3T/2020", as.numeric(c1)),
                      c("3T/2020 a 3T/2021", as.numeric(c2)))

d <- rbindlist(l2)
d <- d[,-c(4,5)]

d1 <- cor(d[(1:24),]$Produtividade,
          y=d[(1:24),]$VA)

d2 <- cor(d[(17:nrow(d)),]$Produtividade,
          y=d[(17:nrow(d)),]$VA)

correlations_1 <- rbind(correlations_1,
                        c("1T/2020 a 3T/2020", d1),
                        c("3T/2020 a 3T/2021", d2))


correlations_f <-data.frame(correlations,correlations_1)
correlations_f <- correlations_f[,-3]

q <- as.data.table(correlations_f)

writexl::write_xlsx(data_frame(q[,1],
  as.numeric(q$Correlações_OC_PRODT),
  as.numeric(q$Correlações_VA_PRODT)),
                    "correlacoes.xlsx")














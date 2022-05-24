library(tidyverse)
library(data.table)

HET <- HET_PRODUT[,c(1,10:17)]
PRODT <- HET_PRODUT[,c(1,18:ncol(HET_PRODUT))]
VA <- HET_PRODUT[,c(1:9)]

categorias <- as.list(c("Agropecuária", "Indústria Geral", "Construção", "Comércio",
                        "Transporte", "ICAFI", "Outros Serviços", "APDSS"))

HET_reshape <- melt(data = as.data.table(HET), 
                           id.vars = "Date")

HET_reshape$variable <- as.character(categorias) %>% 
  rep(each = 7)

names(HET_reshape) [2] <- "Setor"
names(HET_reshape) [3] <- "HET"


PRODUT_reshape <- melt(data = as.data.table(PRODT), 
                    id.vars = "Date"
)

PRODUT_reshape$variable <- as.character(categorias) %>% 
  rep(each = 7)

names(PRODUT_reshape) [2] <- "Setor"
names(PRODUT_reshape) [3] <- "Produtividade"

VA_reshape <- melt(data = as.data.table(VA), 
                   id.vars = "Date"
)

names(VA_reshape) [2] <- "Setor"
names(VA_reshape) [3] <- "VA"

COMP <- data_frame(HET_reshape, PRODUT_reshape[,3], VA_reshape[,3])





####

mylist_1 <-list("1T/2020", "2T/2020", "3T/2020",
                "4T/2020", "1T/2021", "2T/2021", "3T/2021")
dates <- c("1T/2020", "2T/2020", "3T/2020",
           "4T/2020", "1T/2021", "2T/2021", "3T/2021")


l <- list()
l_1 <- list()



for (i in seq_along(mylist_1)) {
  dados_c <- data.frame(COMP) %>% filter(Date == mylist_1[[i]])
  dados_c$VA <- round(as.numeric(dados_c$VA)
                       ,2)
  dados_c$HET <- round(as.numeric(dados_c$HET)
                      ,2)
  dados_c$Produtividade <- round(as.numeric(dados_c$Produtividade)
                      ,2)

  a <- dados_c %>% 
    ggplot(aes(x= HET, y = Produtividade)) +
    geom_point(aes(color = Setor,fill = Setor)) +
    geom_text(aes(label=Setor), nudge_x = -0.004, nudge_y = -0.0004) +
    geom_smooth(method=lm, se=F) +
    theme(legend.position = "none") +
    labs(caption = as.character(mylist_1[[i]])) +
    labs(x="Taxa de Crescimento das HET",
         y= "Taxa de Crescimento da Produtividade")+
    theme(plot.caption = element_text(hjust = 0.5))
  
  b <- dados_c %>% 
    ggplot(aes(x= VA, y = Produtividade)) +
    geom_point(aes(color = Setor,fill = Setor)) +
    geom_text(aes(label=Setor), nudge_x = -0.004, nudge_y = -0.0004) +
    geom_smooth(method=lm, se=F) +
    theme(legend.position = "none") +
    labs(caption = as.character(mylist_1[[i]])) +
    labs(x="Taxa de Crescimento das VA",
         y= "Taxa de Crescimento da Produtividade")+
    theme(plot.caption = element_text(hjust = 0.5))
  
  assign(paste0("HET_PRODUT_", as.character(mylist_1[[i]])),a) -> l[[i]]  
  assign(paste0("VA_PRODUT_", as.character(mylist_1[[i]])),b) -> l_1[[i]] 

  
}

grid.arrange(grobs= l, ncol =2) # Produtividade x HET
grid.arrange(grobs= l_1, ncol =2) # Produtividade x VA
# A3, 297 x 420 mm, 11.7 x 16.5 in

l1 <-list()
l2 <-list()

for (i in seq_along(categorias)) {
  dados_c <- data.frame(COMP) %>% filter(Setor == categorias[[i]])
  dados_c$HET <- round(as.numeric(dados_c$HET)
                       ,2)
  dados_c$Produtividade <- round(as.numeric(dados_c$Produtividade)
                                 ,2)
  
  a <- dados_c %>% 
    ggplot(aes(x= HET, y = Produtividade)) +
    geom_point(aes(color = Date,fill = Date)) +
    geom_text(aes(label=Date), nudge_x = -0.004, nudge_y = -0.0004) +
    geom_smooth(method=lm, se=F) +
    theme(legend.position = "none") +
    labs(caption = as.character(categorias[[i]])) +
    labs(x="Taxa de Crescimento das HET",
         y= "Taxa de Crescimento da Produtividade")+
    theme(plot.caption = element_text(hjust = 0.5))
  
  assign(paste0("HET_PRODUT_", as.character(categorias[[i]])),a) -> l1[[i]]  
  
  b <- dados_c %>% 
    ggplot(aes(x= VA, y = Produtividade)) +
    geom_point(aes(color = Date,fill = Date)) +
    geom_text(aes(label=Date), nudge_x = -0.004, nudge_y = -0.0004) +
    geom_smooth(method=lm, se=F) +
    theme(legend.position = "none") +
    labs(caption = as.character(categorias[[i]])) +
    labs(x="Taxa de Crescimento do VA",
         y= "Taxa de Crescimento da Produtividade")+
    theme(plot.caption = element_text(hjust = 0.5))
  
  assign(paste0("VA_PRODUT_", as.character(categorias[[i]])),b) -> l2[[i]]
  
}

grid.arrange(grobs= l1, ncol =2) # Produtividade x HET
grid.arrange(grobs= l2, ncol =2) # Produtividade x VA
# A3, 297 x 420 mm, 11.7 x 16.5 in

##Correlações
correlations <- list()
correlations_1 <- list()
l2 <- list()
l3 <- list()

for (i in seq_along(mylist_1)) {
  dados_c <- COMP %>% filter(Date == mylist_1[[i]])
  assign(paste0("relacao_", as.character(mylist_1[[i]])),dados_c) -> l2[[i]]
  
  cor(x=dados_c$Produtividade, y=dados_c$HET) -> correlations[[i]]
  cor(x=dados_c$Produtividade, y=dados_c$VA) -> correlations_1[[i]]
}

correlations <- data_frame(dates, correlations,correlations_1)
names(correlations) [2] <- "PRODUT_HET"
names(correlations) [3] <- "PRODUT_VA"


correlations <- rbind(correlations,c("Total",as.numeric(cor(c$Produtividade,y=c$HET)
                                           ,as.numeric(cor(c$Produtividade,c$VA)))))



###ACUMULADO





c <- rbindlist(l2)



c1 <- cor(c[(1:24),]$Produtividade,
          y=c[(1:24),]$HET)

c2 <- cor(c[(17:nrow(c)),]$Produtividade,
          c[(17:nrow(c)),]$HET)

d1 <- cor(c[(1:24),]$Produtividade,
          c[(1:24),]$VA)

d2 <- cor(c[(17:nrow(c)),]$Produtividade,
          c[(17:nrow(c)),]$VA)


correlations <- rbind(correlations, 
                      c("1T/2020 a 3T/2020", as.numeric(c1), as.numeric(d1)),
                      c("3T/2020 a 3T/2021", as.numeric(c2), as.numeric(d2)))

correlations$PRODUT_HET <- as.numeric(correlations$PRODUT_HET)
correlations$PRODUT_VA <- as.numeric(correlations$PRODUT_VA)


writexl::write_xlsx(correlations, "correlacoes_HET.xlsx")  

writexl::write_xlsx(c, "correlacoes_HET_teste.xlsx")  





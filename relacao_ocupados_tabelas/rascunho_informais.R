categorias <- as.list(c("Agropecuária", "Indústria Geral", "Construção", "Comércio",
                        "Transporte", "ICAFI", "Outros Serviços", "APDSS"))
l <- list()
correlations <- list()

for (i in seq_along(categorias)) {
  dados_c_1 <- data.frame(trabalhadores_agregado) %>% filter(setor == categorias[[i]])
  dados_c <- data.frame(PRODUT_OC) %>% filter(Setor == categorias[[i]])
  dados_c <- data_frame(dados_c,dados_c_1)
  dados_c$Produtividade <- round(as.numeric(dados_c$Produtividade),2)
  dados_c$informais_relativo <- round(as.numeric(dados_c$informais_relativo),4)
  dados_c <- dados_c %>% select("Setor","Date", "Produtividade", "informais_relativo")
  dados_c$tx_informais <- with(dados_c,((informais_relativo/lag(informais_relativo))) - 1)

  a <- dados_c %>% 
    ggplot(aes(x= informais_relativo, y = Produtividade)) +
    geom_point(aes(color = Date,fill = Date)) +
    geom_text(aes(label=Date), nudge_x = -0.0025, nudge_y = -0.0004) +
    geom_smooth(method=lm, se=F) +
    theme(legend.position = "none") +
    labs(caption = as.character(categorias[[i]])) +
    labs(x="Participação Relativa dos Informais",
         y= "Taxa de Crescimento da Produtividade")+
    theme(plot.caption = element_text(hjust = 0.5))
  
  round(cor(x=dados_c$Produtividade, y=dados_c$informais_relativo),2) -> correlations[[i]]
  assign(paste0("SETOR_INFORM", as.character(categorias[[i]])),a) -> l[[i]]  
  
}

c <- data_frame(categorias,correlations)


grid.arrange(grobs= l, ncol =2)
# A3, 297 x 420 mm, 11.7 x 16.5 in

####HET
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


l <- list()
correlations <- list()

for (i in seq_along(categorias)) {
  dados_c_1 <- data.frame(trabalhadores_agregado) %>% 
    filter(setor == categorias[[i]]) %>% filter(trimestre != "2019/4T")
  
  dados_c <- data.frame(COMP) %>% filter(Setor == categorias[[i]])
  dados_c <- data_frame(dados_c,dados_c_1)
  dados_c$Produtividade <- round(as.numeric(dados_c$Produtividade),2)
  dados_c$informais_relativo <- round(as.numeric(dados_c$informais_relativo),4)
  dados_c <- dados_c %>% select("Setor","Date", "Produtividade", "informais_relativo")
  assign("saida",dados_c)

  
  a <- dados_c %>% 
    ggplot(aes(x= informais_relativo, y = Produtividade)) +
    geom_point(aes(color = Date,fill = Date)) +
    geom_text(aes(label=Date), nudge_x = -0.0025, nudge_y = -0.0004) +
    geom_smooth(method=lm, se=F) +
    theme(legend.position = "none") +
    labs(caption = as.character(categorias[[i]])) +
    labs(x="Participação Relativa dos Informais",
         y= "Taxa de Crescimento da Produtividade")+
    theme(plot.caption = element_text(hjust = 0.5))
  
  round(cor(x=dados_c$Produtividade, y=dados_c$informais_relativo),2) -> correlations[[i]]
  assign(paste0("SETOR_INFORM", as.character(categorias[[i]])),a) -> l[[i]]  
  
}

grid.arrange(grobs= l, ncol =2)


### Correlações

correlations <- list()
correlations_1 <- list()


for (i in seq_along(categorias)) {
  dados_b <- PRODUT_OC %>% filter(Setor == categorias[[i]]) %>% 
    filter(Date != "4T/2019")
  dados_c <- COMP %>% filter(Setor == categorias[[i]])
  dados_d <- trabalhadores_agregado %>% filter(setor == categorias[[i]]) %>% 
    filter(trimestre != "2019/4T")

    
  round(cor(x=as.numeric(dados_c$Produtividade), y=as.numeric(dados_d$informais_relativo)),2) -> correlations[[i]]
  round(cor(x=as.numeric(dados_b$Produtividade), y=as.numeric(dados_d$informais_relativo)),2) -> correlations_1[[i]]
  
}

correlations <- data_frame(categorias, correlations,correlations_1)
names(correlations) [2] <- "PRODUT_HET_INFORMAIS"
names(correlations) [3] <- "PRODUT_PO_INFORMAIS"

correlations <- rbind(correlations, 
                           c("Total",
                             round(cor(x=with(COMP, Produtividade), 
                                       y= with(trabalhadores_agregado %>% filter(trimestre != "2019/4T"),
                                               informais_relativo)),2),
                             
                             round(cor(x=as.numeric(with(PRODUT_OC %>%filter(Date != "4T/2019"), Produtividade)), 
                                       y= with(trabalhadores_agregado %>% filter(trimestre != "2019/4T"),
                                               informais_relativo)),2)))
                                           
                                           
                                        


correlations$categorias <- as.character(correlations$categorias)
correlations$PRODUT_HET_INFORMAIS<- as.numeric(correlations$PRODUT_HET_INFORMAIS)
correlations$PRODUT_PO_INFORMAIS <- as.numeric(correlations$PRODUT_PO_INFORMAIS)




correlations <- rbind(correlations,c("Total",round(as.numeric(cor(COMP$Produtividade,y=COMP$HET)),2)
                                     ,round(as.numeric(cor(COMP$Produtividade,COMP$VA))),2))




writexl::write_xlsx(correlations, "correlacoes_informais.xlsx")

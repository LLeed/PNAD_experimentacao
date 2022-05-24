#####HET

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
                       id.vars = "Date")

PRODUT_reshape$variable <- as.character(categorias) %>% 
  rep(each = 7)

names(PRODUT_reshape) [2] <- "Setor"
names(PRODUT_reshape) [3] <- "Produtividade"

VA_reshape <- melt(data = as.data.table(VA), 
                   id.vars = "Date")

names(VA_reshape) [2] <- "Setor"
names(VA_reshape) [3] <- "VA"

COMP <- data_frame(HET_reshape, PRODUT_reshape[,3], VA_reshape[,3])

categorias <- as.list(c("Agropecuária", "Indústria Geral", "Construção", "Comércio",
                        "Transporte", "ICAFI", "Outros Serviços", "APDSS"))


##Correlações
correlations <- list()
correlations_1 <- list()

for (i in seq_along(categorias)) {
  dados_c <- COMP %>% filter(Setor == categorias[[i]])
  
  round(cor(x=dados_c$Produtividade, y=dados_c$HET),2) -> correlations[[i]]
  round(cor(x=dados_c$Produtividade, y=dados_c$VA),2) -> correlations_1[[i]]
}

correlations <- data_frame(categorias, correlations,correlations_1)
names(correlations) [2] <- "PRODUT_HET"
names(correlations) [3] <- "PRODUT_VA"

correlations$categorias <- as.character(correlations$categorias)
correlations$PRODUT_HET <- as.numeric(correlations$PRODUT_HET)
correlations$PRODUT_VA <- as.numeric(correlations$PRODUT_VA)

correlations <- rbind(correlations,c("Total",round(as.numeric(cor(COMP$Produtividade,y=COMP$HET)),2)
                                            ,round(as.numeric(cor(COMP$Produtividade,COMP$VA))),2))


writexl::write_xlsx(correlations, "correlacoes_HET_novo.xlsx")

#####PO

correlations <- list()
correlations_1 <- list()



for (i in seq_along(categorias)) {
  dados_1 <- PRODUT_OC %>% filter(Setor == categorias[[i]]) %>% 
    filter(Date != "4T/2019")
  dados_2 <- reshape_va %>% filter(Setor == categorias[[i]])
  VA <- dados_2$VA
  dados_c <- data.frame(dados_1,VA)
  dados_c$Produtividade <- as.numeric(dados_c$Produtividade)
  
  round(cor(x=dados_c$Produtividade, y=dados_c$OC),2) -> correlations[[i]]
  round(cor(x=dados_c$Produtividade, y=dados_c$VA),2) -> correlations_1[[i]]
  
}

correlations <- data_frame(categorias, correlations,correlations_1)
names(correlations) [2] <- "PRODUT_OC"
names(correlations) [3] <- "PRODUT_VA"

correlations$categorias <- as.character(correlations$categorias)
correlations$PRODUT_OC <- as.numeric(correlations$PRODUT_OC)
correlations$PRODUT_VA <- as.numeric(correlations$PRODUT_VA)

PRODUT_OC <-PRODUT_OC%>% filter(Date != "4T/2019")

correlations <- 
  rbind(correlations,c("Total",round(as.numeric(cor(as.numeric(PRODUT_OC$Produtividade),
                                                                  y=as.numeric(PRODUT_OC$OC))),2),
                       round(cor(as.numeric(PRODUT_OC$Produtividade),
                                                           y=as.numeric(reshape_va$VA)),2)))


writexl::write_xlsx(correlations, "correlacoes_oc_novo.xlsx")






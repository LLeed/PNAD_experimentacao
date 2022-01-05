##Posição na Ocupação - Total da Economia ##

##droplevels

rm(list = ls())

## Scrit formulado por Victor Nobre, Ledson Gomes e Igor Soares

### Este script serve para manipular as bases do IBGE, pelos diferentes posicoes na ocupacao 

# carregando pacotes
#library(PNADcIBGE)
library(tidyverse)
#library(survey)
library(data.table)
library(writexl)

# Combinando categorias - para compatibilização com o PIB Trimestral

##Classificações de Referência##
e <-pnad_2019$variables$VD4010[21] # "Administração pública, defesa e seguridade social "
h <-pnad_2019$variables$VD4010[40] # "Informação, comunicação e atividades financeiras, 
#imobiliárias, profissionais e administrativas"
f <-pnad_2019$variables$VD4010[30] # "Outros Serviços"
g <-pnad_2019$variables$VD4008[1] # "Empregado no Setor Privado"
aea <-pnad_2019$variables$VD4010[10] # "Alojamento e alimentação"
r <- pnad_2020_1$variables$VD4010[204] # "Atividades Mal Definidas"
##Definindo Listas##

mylist <- list(pnad_2019, pnad_2020_1, pnad_2020_2, pnad_2020_3,
               pnad_2020_4, pnad_2020_1, pnad_2021_2, pnad_2021_3)
mylist_1 <-list("2019","2020_1", "2020_2",
                "2020_3", "2020_4", "2021_1", "2021_2","2021_3")


#Loop para a Substituição
for (i in seq_along(mylist)){
  
  a <- mylist[[i]]
  
  # ***Separando Emprego Público e Privado em Adm. Pública(etc) e Outros (respectivamente) ***
  a$variables$VD4010[a$variables$VD4010 == "Educação, saúde humana e serviços sociais" &
                       a$variables$VD4008 == g ] <- as.factor(f)
  a$variables$VD4010[a$variables$VD4010 == "Educação, saúde humana e serviços sociais" &
                       a$variables$VD4008 != g ] <- as.factor(e)
  # *** Agregando categorias em Outros ***
  a$variables$VD4010[a$variables$VD4010 == h & 
                       a$variables$VD4011 == "Diretores e gerentes" & 
                       a$variables$VD4011 == "Profissionais das ciências e intelectuais"&
                       a$variables$VD4011 == "Técnicos e profissionais de nível médio"&
                       a$variables$VD4011 == "Trabalhadores de apoio administrativo"] <- as.factor(f)
  a$variables$VD4010[a$variables$VD4010 == "Serviços domésticos"] <- as.factor(f)
  a$variables$VD4010[a$variables$VD4010 == aea] <- as.factor(f)
  a$variables$VD4010[a$variables$VD4010 == r] <- as.factor(f)

  
  
  assign(paste0("pnad_", mylist_1[[i]]),a)
  ;
}



rm(r,e,f,aea,h,g,var_select)






mylist <- list(pnad_2019, pnad_2020_1, pnad_2020_2, pnad_2020_3,
               pnad_2020_4, pnad_2020_1, pnad_2021_2, pnad_2021_3)




#Ocupação - Total da Economia #

mylist_1 <-list("2019/4T", "2020/1T", "2020/2T", "2020/3T",
                "2020/4T", "2021/1T", "2021/2T", "2021/3T")

for (i in seq_along(mylist)) {
  p <- mylist[[i]]
  a <- as.data.frame(summary(na.omit(droplevels(p$variables$VD4010)))) %>% 
    mutate(trimestre = mylist_1[[i]])
  a <- a %>% mutate(row.names(a)) 
  d <- rbindlist(list(a), use.names=FALSE)
  assign(paste0("pnad_ocupacao_setores_",i), d);
}


pnad_ocupacao_setores_agregado <-rbindlist(list(pnad_ocupacao_setores_1,
                                        pnad_ocupacao_setores_2,
                                        pnad_ocupacao_setores_3,
                                        pnad_ocupacao_setores_4,
                                        pnad_ocupacao_setores_5,
                                        pnad_ocupacao_setores_6,
                                        pnad_ocupacao_setores_7,
                                        pnad_ocupacao_setores_8), use.names=FALSE)


pnad_ocupacao_setores_agregado$Numero_de_Ocupados <- pnad_ocupacao_setores_agregado$`summary(na.omit(droplevels(p$variables$VD4010)))`
pnad_ocupacao_setores_agregado$setor<- pnad_ocupacao_setores_agregado$`row.names(a)`

pnad_ocupacao_setores_agregado <- pnad_ocupacao_setores_agregado %>% select(trimestre, setor, Numero_de_Ocupados)

##Exportando Resultados
write_xlsx(pnad_ocupacao_setores_agregado,"pnad_ocupacao_setores_agregado.xlsx")




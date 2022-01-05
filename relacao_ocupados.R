# Definindo limite de memoria para compilacao do programa
aviso <- getOption("warn")
options(warn=-1)
memory.limit(size=20000)
options(warn=aviso)
rm(aviso)

library(PNADcIBGE)
library(tidyverse)
#library(survey)
library(data.table)
library(readxl)
library(rio)

var_select <- c("VD4002", "V2009", "VD4019", "VD3004", "VD4010", 
                "VD4008", "VD4009", "VD4011" ,"VD4012")

#pnad_2019 <- get_pnadc(year = 2019, quarter = 4, vars = var_select)
#pnad_2020_1 <- get_pnadc(year = 2020, quarter = 1, vars = var_select)

e <-pnad_2019$variables$VD4010[21] # "Administração pública, defesa e seguridade social "
h <-pnad_2019$variables$VD4010[40] # "Informação, comunicação e atividades financeiras, 
#imobiliárias, profissionais e administrativas"
f <-pnad_2019$variables$VD4010[30] # "Outros Serviços"
g <-pnad_2019$variables$VD4008[1] # "Empregado no Setor Privado"
aea <-pnad_2019$variables$VD4010[10] # "Alojamento e alimentação"
r <- pnad_2020_1$variables$VD4010[204] # "Atividades Mal Definidas"

rm(pnad_2020_1)
anos <-as.list(2012:2013)
#trimestre <-as.data.frame(rep(1:2, times=2))
#trimestre <- list(trimestre)#[-40,])
trimestre <-as.list(rep(1:2, times=2))


for (i in seq_along(anos)) {
  l = list()
  ano = anos[[i]]
  for (j in seq_along(trimestre)) {
    q <- get_pnadc(year = ano, quarter = trimestre[[j]] , vars = var_select)
   
###    
    q$variables$VD4010[q$variables$VD4010 == "Educação, saúde humana e serviços sociais" &
                         q$variables$VD4008 == g ] <- as.factor(f)
    q$variables$VD4010[q$variables$VD4010 == "Educação, saúde humana e serviços sociais" &
                         q$variables$VD4008 != g ] <- as.factor(e)
    # *** Agregando categorias em Outros ***
    q$variables$VD4010[q$variables$VD4010 == h & 
                         q$variables$VD4011 == "Diretores e gerentes" & 
                         q$variables$VD4011 == "Profissionais das ciências e intelectuais"&
                         q$variables$VD4011 == "Técnicos e profissionais de nível médio"&
                         q$variables$VD4011 == "Trabalhadores de apoio administrativo"] <- as.factor(f)
    q$variables$VD4010[q$variables$VD4010 == "Serviços domésticos"] <- as.factor(f)
    q$variables$VD4010[q$variables$VD4010 == aea] <- as.factor(f)
    q$variables$VD4010[q$variables$VD4010 == r] <- as.factor(f)
    q <- as.data.frame(summary(na.omit(q$variables$VD4009))) %>% 
      mutate(trimestre = paste0(ano, sep="_" ,trimestre[[i]]))
    q <- q / sum(q)
    assign(paste0(ano,sep="_",trimestre[[j]]), q) -> l[[j]]
  }
  l <- as.data.frame(l)
  writexl::write_xlsx(l, paste(relacao,ano, sep = "_"))
  
}





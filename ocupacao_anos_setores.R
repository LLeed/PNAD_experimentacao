library(PNADcIBGE)
library(tidyverse)
#library(descr)
library(survey)
#library(sidrar)
library(data.table)

# baixando as bases

var_select <- c("VD4019", "VD3004", "VD4010", "VD4009","VD4008","VD4012")

pnad_2019 <- get_pnadc(year = 2019, quarter = 4, vars = var_select)

a <- pnad_2019
e <-a$variables$VD4010[21] # "Administração pública, defesa e seguridade social "
f <-a$variables$VD4010[30] # "Outros Serviços"
g <-a$variables$VD4008[1] # "Empregado no Setor Privado"
aea <-a$variables$VD4010[10] # "Alojamento e alimentação"



#***Separando Emprego Público e Privado em Adm. Pública(etc) e Outros (respectivamente) ***
pnad_2019$variables$VD4010[pnad_2019$variables$VD4010 == "Educação, saúde humana e serviços sociais" &
                             pnad_2019$variables$VD4008 == g ] <- as.factor(e)
pnad_2019$variables$VD4010[pnad_2019$variables$VD4010 == "Educação, saúde humana e serviços sociais" &
                             pnad_2019$variables$VD4008 != g ] <- as.factor(f)

#*** Agregando categorias em Outros ***


pnad_2019$variables$VD4010[pnad_2019$variables$VD4010 == "Serviços domésticos"] <- as.factor(f)
pnad_2019$variables$VD4010[pnad_2019$variables$VD4010 == aea] <- as.factor(f) 






    p <-pnad_2019
    a<-as.data.frame(summary(na.omit(
      interaction((p$variables$VD4010),
                  (p$variables$VD4009),drop = T)))) %>% 
      mutate(trimestre = 2009)
    a <- a %>% mutate(row.names(a)) 
    
    a <- a %>% 
      separate( 
        col = "row.names(a)", 
        into = c("setor", "ocupacao"),
        sep = "\\.")
    
    a <- a[a$ocupacao != "Empregador" & a$ocupacao != "Conta-própria" ,]
    
    b <-as.data.frame(summary(na.omit(
      interaction((p$variables$VD4010),
                  (p$variables$VD4009),
                  (p$variables$VD4012),
                  drop = T)))) %>% 
      mutate(trimestre = 2009)
    
    q <- nrow(b)
    b <- b[-c(q),]
    b <- b %>% mutate(row.names(b))
    print(nrow(b))

    b <- b %>% 
      separate( 
        col = "row.names(b)", 
        into = c("setor", "ocupacao", "PS"),
        sep = "\\.")
    
    b_1 <- b[b$ocupacao == "Empregador",]
    b_2 <- b[b$ocupacao == "Conta-própria",]
    b_c <- rbindlist(list(b_1,b_2))
    
    b <- b %>% unite(col = "row.names(b)", 
                     ocupacao:PS,
                     sep = "\\.")
    
    
    
    print(nrow(b_1))
    
    a <- a[a$ocupacao != "Empregador" & a$ocupacao != "Conta-própria" ,]
    
    
    
    
    
    b <- b %>% 
      separate( 
        col = "row.names(b)", 
        into = c("instrucao", "setor"),
        sep = "\\.")
    
    
    
    pnad_rendimento_agregado <- pnad_rendimento_agregado %>% select(trimestre,instrucao, setor, VD4019)
    
    pnad_rendimento_agregado <- pnad_rendimento_agregado %>% rename(rendimento_medio_nominal = VD4019)
    
    pnad_rendimento_agregado$rendimento_medio_nominal <- round(pnad_rendimento_agregado$rendimento_medio_nominal, digits = 2)
    
    
    

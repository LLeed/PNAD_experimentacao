rm(list = ls())

## Scrit formulado por Victor Nobre, Ledson Gomes e Igor Soares

### Este script serve para manipular as bases do IBGE

# carregando pacotes

library(PNADcIBGE)
library(tidyverse)
library(survey)
library(data.table)

# baixando as bases

var_select <- c("VD3004", "VD4009", "VD4010")

pnad_2019 <- get_pnadc(year = 2019, quarter = 4, vars = var_select)

a <-pnad_2019
complete.cases()

b <- as.data.frame(summary(a$variables$VD4009)) %>% mutate(trimestre = "2019/4T")
b_1 <- as.data.frame(summary(a$variables$VD4009)) %>% mutate(trimestre = "2019/3T")



bb <-rbindlist(list(b,b_1), use.names=TRUE, fill=TRUE)


c <-b
c_r <- as.data.frame(c[-c(""),])


m

pnad_rendimento_medio_setor_2019 <- svyby(formula =~VD4019, by = VD4010, design = pnad_2019, FUN = svymean, na.rm = TRUE )

a <-pnad_2019

rm(list = ls())

## Scrit formulado por Victor Nobre, Ledson Gomes e Igor Soares

### Este script serve para manipular as bases do IBGE

# carregando pacotes

library(PNADcIBGE)
library(tidyverse)
library(survey)
library(data.table)

# baixando as bases

var_select <- c("VD3004", "VD4009", "VD4010", "VD4012")

pnad_2019 <- get_pnadc(year = 2019, quarter = 4, vars = var_select)

a <-pnad_2019


b_1 <-as.data.frame(summary(
  na.omit(a$variables$VD4009)))

b_1_carteira <-as.data.frame(summary(
  na.omit(interaction((a$variables$VD4009),(a$variables$VD4012), drop = T))))


b_1 <-as.data.frame(summary(
  na.omit(interaction((a$variables$VD4009),(a$variables$VD4010), drop = T))))

b_1 <-as.data.frame(summary(
  na.omit(interaction((a$variables$VD4009),(a$variables$VD4010), drop = T))))








b_1 <-as.data.frame(summary(
  na.omit((a$variables$VD4009)))) %>% 
  mutate(row.names(b_1)) %>% 
  mutate(trimestre = "4T/2019")





b_2 <- b_1

bb <-rbindlist(list(b_1,b_2))
view(bb)

row.names(b_1)

)k







%>% mutate(trimestre = "2019/4T")
b_1 <- as.data.frame(summary(a$variables$VD4009)) %>% mutate(trimestre = "2019/3T")



bb <-rbindlist(list(b,b_1), use.names=TRUE, fill=TRUE)


c <-b
c_r <- as.data.frame(c[-c(""),])


m

pnad_rendimento_medio_setor_2019 <- svyby(formula =~VD4019, by = VD4010, design = pnad_2019, FUN = svymean, na.rm = TRUE )

a <-pnad_2019


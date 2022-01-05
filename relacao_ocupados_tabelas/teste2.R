library(PNADcIBGE)
library(tidyverse)

anos <-list(pnad_2019, pnad_2020_1)
#trimestre <-as.data.frame(rep(1:2, times=2))
#trimestre <- list(trimestre)#[-40,])
trimestre <-as.list(1)


for (i in seq_along(anos)) {
  l = list()
  #ano = anos[[i]]
  q <- anos[[i]]
  for (j in seq_along(trimestre)) {
    #q <- get_pnadc(year = ano, quarter = trimestre[[j]] , vars = var_select)
    
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
################    
    q <- as.data.frame(summary(na.omit(droplevels(q$variables$VD4010))))
    q1 <- row.names(q)
    q <-q[-c(1,2,3),] 
    q <- (q/sum(q))
    q <- as.data.frame(cbind(q, q1[4:length(q1)]))
    q <- q %>% mutate(trimestre = paste0(as.character(anos[[i]]),
                                         sep="_",
                                         as.character(trimestre[[j]])))
    q -> l[[i]]
  }
  
  relacao <- as.data.frame(l)
  names(relacao)[1] <- "porcentagem_no_servico"
  names(relacao)[2] <- "categorias"
  writexl::write_xlsx(relacao, paste0(as.character(anos[[i]]), sep = "_", i, ".xlsx"));
  
}




rm(q)
view(relacao)

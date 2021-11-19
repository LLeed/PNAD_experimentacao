var_select <- c("VD4019", "VD3004", "VD4010", "VD4012")

pnad_2019 <- get_pnadc(year = 2019, quarter = 4, vars = var_select)
pnad_2020_1 <- get_pnadc(year = 2020, quarter = 1, vars = var_select)
pnad_2020_2 <- get_pnadc(year = 2020, quarter = 2, vars = var_select)
pnad_2020_3 <- get_pnadc(year = 2020, quarter = 3, vars = var_select)
pnad_2020_4 <- get_pnadc(year = 2020, quarter = 4, vars = var_select)
pnad_2021_1 <- get_pnadc(year = 2021, quarter = 1, vars = var_select)
pnad_2021_2 <- get_pnadc(year = 2021, quarter = 2, vars = var_select)






pnad_ocup_categ_2009 <-as.data.frame(summary(
  na.omit(pnad_2019$variables$VD4009))) %>%
  mutate(row.names(pnad_ocup_categ_2009)) %>% 
  mutate(trimestre = "2019/4T")

pnad_ocup_categ_2020_1  <-as.data.frame(summary(
  na.omit(pnad_2020_1$variables$VD4009))) %>%
  mutate(row.names(pnad_ocup_categ_2020_1)) %>% 
  mutate(trimestre = "2020/1T")

pnad_ocup_categ_2020_2 <-as.data.frame(summary(
  na.omit(pnad_2020_2$variables$VD4009))) %>%
  mutate(row.names(pnad_ocup_categ_2020_2)) %>% 
  mutate(trimestre = "2020/2T")

pnad_ocup_categ_2020_3 <-as.data.frame(summary(
  na.omit(pnad_2020_3$variables$VD4009))) %>%
  mutate(row.names(pnad_ocup_categ_2020_3)) %>% 
  mutate(trimestre = "2020/3T")

pnad_ocup_categ_2020_4 <-as.data.frame(summary(
  na.omit(pnad_2020_4$variables$VD4009))) %>%
  mutate(row.names(pnad_ocup_categ_2020_4)) %>% 
  mutate(trimestre = "2020/4T")

pnad_ocup_categ_2021_1<-as.data.frame(summary(
  na.omit(pnad_2021_1$variables$VD4009))) %>%
  mutate(row.names(pnad_ocup_categ_2021_1)) %>% 
  mutate(trimestre = "2021/2T")

pnad_ocup_categ_2021_2<-as.data.frame(summary(
  na.omit(pnad_2021_2$variables$VD4009))) %>%
  mutate(row.names(pnad_ocup_categ_2021_2)) %>% 
  mutate(trimestre = "2021/2T")



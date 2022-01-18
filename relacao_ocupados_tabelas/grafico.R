library(tidyverse)
library(data.table)
library(readxl)


#Releção Número de Ocupados Serviços
ocupados_servicos <- read_excel("ocupados_servicos.xlsx")
base <- as.data.frame(ocupados_servicos[,-c(1,7)])
trimestre <- as.data.frame(ocupados_servicos[,1])
total_servicos <- as.data.frame(ocupados_servicos[,7])
###

time <- as.data.frame(paste0(rep(2012:2021, each = 4), sep=".", 
                             rep(1:4,times =10 )))
time1 <- as.data.frame(time[-40,])
names(time1)[1] <- "date"
relacao <- base/as.data.frame(rep(total_servicos,times = as.numeric(length(base))))
relacao_a <-data_frame(time1,relacao)

###

relacao_b<- data.table::melt(data = as.data.table(relacao_a),
                             id.vars = "date",
                             variable.names = "setores",
                             value.name = "prop")

##

relacao_c <- relacao_b %>% filter(date>2019.3)
###

relacao_c %>% ggplot(aes(x=date, y=(prop*100), fill=variable)) +
  geom_col() +
  scale_fill_discrete(name=NULL) +
  scale_x_discrete(breaks=as.list(relacao_b$date)) +
  labs(x=NULL,
       y="(%)") +
  theme_classic() +
  theme(legend.key.size = unit(9, "pt"),
        legend.text = element_text(size=13),
        legend.position = "top") +
  guides(fill=guide_legend(nrow=3, byrow=TRUE))

## size export: 1920x1080

##########

#Relação VA
base1 <- read_excel("va_servicos.xlsx")
va_total <- base1[,length(base1)]
base1 <- base1[,-length(base1)]

relacao1 <- base1/as.data.frame(rep(va_total,times = as.numeric(length(base1))))
relacao1_a <-data_frame(time1,relacao1)

relacao1_b<- data.table::melt(data = as.data.table(relacao1_a),
                             id.vars = "date",
                             variable.names = "setores",
                             value.name = "prop")

##

relacao1_c <- relacao1_b %>% filter(date>2019.3)

relacao1_c %>% ggplot(aes(x=date, y=(prop*100), fill=variable)) +
  geom_col() +
  scale_fill_discrete(name=NULL) +
  scale_x_discrete(breaks=as.list(relacao_b$date)) +
  labs(x=NULL,
       y="(%)") +
  theme_classic() +
  theme(legend.key.size = unit(9, "pt"),
        legend.text = element_text(size=13),
        legend.position = "top") +
  guides(fill=guide_legend(nrow=3, byrow=TRUE))





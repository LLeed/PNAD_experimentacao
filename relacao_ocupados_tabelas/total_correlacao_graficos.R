library(tidyverse)
names(totais)[1] <- "Date"
### Gráficos

## PO
totais %>% 
  ggplot(aes(x= Ocupação_Total, y = Produtividade_Total_PO)) +
  geom_point(aes(color = Date,fill = Date)) +
  geom_text(aes(label=Date), nudge_x = -0.004, nudge_y = -0.0004) +
  geom_smooth(method=lm, se=F) +
  theme(legend.position = "none") +
  labs(caption = "Total da Economia") +
  labs(x="Taxa de Crescimento do Número de Ocupados",
       y= "Taxa de Crescimento da Produtividade")+
  theme(plot.caption = element_text(hjust = 0.5))

totais %>% 
  ggplot(aes(x= VA_Total, y = Produtividade_Total_PO)) +
  geom_point(aes(color = Date,fill = Date)) +
  geom_text(aes(label=Date), nudge_x = -0.004, nudge_y = -0.0004) +
  geom_smooth(method=lm, se=F) +
  theme(legend.position = "none") +
  labs(caption = "Total da Economia") +
  labs(x="Taxa de Crescimento do VA",
       y= "Taxa de Crescimento da Produtividade")+
  theme(plot.caption = element_text(hjust = 0.5))

totais %>% 
  ggplot(aes(x= informais_relativo, y = Produtividade_Total_PO)) +
  geom_point(aes(color = Date,fill = Date)) +
  geom_text(aes(label=Date), nudge_x = -0.004, nudge_y = -0.0004) +
  geom_smooth(method=lm, se=F) +
  theme(legend.position = "none") +
  labs(caption = "Total da Economia") +
  labs(x="Participação Relativa dos Informais",
       y= "Taxa de Crescimento da Produtividade")+
  theme(plot.caption = element_text(hjust = 0.5))




##HET
totais %>% 
  ggplot(aes(x= VA_Total, y = Produtividade_HET_Total)) +
  geom_point(aes(color = Date,fill = Date)) +
  geom_text(aes(label=Date), nudge_x = -0.004, nudge_y = -0.0004) +
  geom_smooth(method=lm, se=F) +
  theme(legend.position = "none") +
  labs(caption = "Total da Economia") +
  labs(x="Taxa de Crescimento do VA",
       y= "Taxa de Crescimento da Produtividade")+
  theme(plot.caption = element_text(hjust = 0.5))

totais %>% 
  ggplot(aes(x= HET_Total, y = Produtividade_HET_Total)) +
  geom_point(aes(color = Date,fill = Date)) +
  geom_text(aes(label=Date), nudge_x = -0.004, nudge_y = -0.0004) +
  geom_smooth(method=lm, se=F) +
  theme(legend.position = "none") +
  labs(caption = "Total da Economia") +
  labs(x="Taxa de Crescimento das HET",
       y= "Taxa de Crescimento da Produtividade")+
  theme(plot.caption = element_text(hjust = 0.5))

totais %>% 
  ggplot(aes(x= informais_relativo, y = Produtividade_HET_Total)) +
  geom_point(aes(color = Date,fill = Date)) +
  geom_text(aes(label=Date), nudge_x = -0.004, nudge_y = -0.0004) +
  geom_smooth(method=lm, se=F) +
  theme(legend.position = "none") +
  labs(caption = "Total da Economia") +
  labs(x="Participação Relativa dos Informais",
       y= "Taxa de Crescimento da Produtividade")+
  theme(plot.caption = element_text(hjust = 0.5))

#Correlações

a <- with(totais,cor(Produtividade_Total_PO, y=VA_Total))
b <- with(totais,cor(Produtividade_Total_PO, y=Ocupação_Total))
c <- with(totais,cor(Produtividade_Total_PO, y=informais_relativo))
d <- with(totais,cor(Produtividade_HET_Total, y=VA_Total))
e <- with(totais,cor(Produtividade_HET_Total, y=HET_Total))
f <- with(totais,cor(Produtividade_HET_Total, y=informais_relativo))

correlações_total <- data_frame(
  c("Produtividade_PO_VA",
    "Produtividade_PO_OC",
    "Produtividade_PO_Informais_Relativos",
    "Produtividade_HET_VA",
    "Produtividade_HET_OC",
    "Produtividade_HET_Informais_Relativos"
  ), c(a,b,c,d,e,f))

names(correlações_total)[1] <- "Variáveis"
names(correlações_total)[2] <- "Correlações"


writexl::write_xlsx(correlações_total, "correlacoes_total.xlsx")




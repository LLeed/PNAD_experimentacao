### taxas acumuladas

VA <- VA_b[c(21:29),1:3]
names(VA)[2] <- as.character(VA[1,2])
names(VA)[3] <- as.character(VA[1,3])
VA <- VA[-1,]
###diferentes data frames
OC <- OC_PRODUT_b[c(37,46:53), 1:3]
names(OC)[2] <- as.character(OC[1,2])
names(OC)[3] <- as.character(OC[1,3])


PRODUT <- OC_PRODUT_b[c(37:45), 1:3]
names(PRODUT)[2] <- as.character(PRODUT[1,2])
names(PRODUT)[3] <- as.character(PRODUT[1,3])

PRODUT <- PRODUT[-1,]
OC <- OC[-1,]

OC$Date <- VA$VA
PRODUT$Date <- VA$VA


reshape_va <- melt(
  data = as.data.table(VA),
  id.vars = "VA",
  variable.name = "Date",
  value.name = "Tx. de Crescimento"
)
names(reshape_va)[1] <- "Setor"
names(reshape_va)[3] <- "VA"

reshape_oc <- melt(
  data = as.data.table(OC),
  id.vars = "Date",
  variable.name = ,
  value.name = "Tx. de Crescimento"
)
names(reshape_oc)[1] <- "Setor"
names(reshape_oc)[3] <- "OC"


reshape_produt <- melt(
  data = as.data.table(PRODUT),
  id.vars = "Date",
  variable.name = ,
  value.name = "Tx. de Crescimento"
)
names(reshape_produt)[1] <- "Setor"
names(reshape_produt)[3] <- "PRODUT"

compilado <- data_frame(reshape_produt, 
                        reshape_oc$OC, 
                        reshape_va$VA)
names(compilado) [2] <- "Date"
names(compilado) [4] <- "OC"
names(compilado) [5] <- "VA"  

mylist_1 <- as.list(colnames(OC[,-1]))
g_oc <- list()
g_va <- list()
list2 <- list("1T/2020 a 3T/2020", "3T/2020 a 3T/2021")

 
for (i in seq_along(list1)) {
  dados_c <- compilado %>% filter(Date == mylist_1[[i]])
  dados_c$VA <- round(as.numeric(dados_c$VA),2)
  dados_c$OC <- round(as.numeric(dados_c$OC),2)
  dados_c$PRODUT <- round(as.numeric(dados_c$PRODUT)
                                 ,2)
  
  a <- dados_c %>% 
    ggplot(aes(x= OC, y = PRODUT)) +
    geom_point(aes(color = Setor,fill = Setor)) +
    geom_text(aes(label=Setor), nudge_x = -0.004, nudge_y = -0.0004) +
    geom_smooth(method=lm, se=F) +
    theme(legend.position = "none") +
    labs(caption = as.character(list2[[i]])) +
    labs(x="Taxa de Crescimento do Número de Ocupados",
         y= "Taxa de Crescimento da Produtividade")+
    theme(plot.caption = element_text(hjust = 0.5))
  assign(paste0("OC_PRODUT_", as.character(mylist_1[[i]])),a) -> g_oc[[i]]  
  
  
  b <- dados_c %>% 
    ggplot(aes(x= VA, y = PRODUT)) +
    geom_point(aes(color = Setor,fill = Setor)) +
    geom_text(aes(label=Setor), nudge_x = -0.004, nudge_y = -0.0004) +
    geom_smooth(method=lm, se=F) +
    theme(legend.position = "none") +
    labs(caption = as.character(list2[[i]])) +
    labs(x="Taxa de Crescimento do VA",
         y= "Taxa de Crescimento da Produtividade")+
    theme(plot.caption = element_text(hjust = 0.5))
  
  assign(paste0("VA_PRODUT_", as.character(mylist_1[[i]])),b) -> g_va[[i]]
  
}  

grid.arrange(grobs= g_oc) # Produtividade x OC
grid.arrange(grobs= g_va) # Produtividade x VA  
  


a <-pnad_2019
a_1 <-pnad_2019

b_1 <-as.data.frame(summary(
  na.omit(a$variables$VD4009)))

l <-list(a$variables$VD4009,a_1$variables$VD4009)




for (i in a$variables$VD4009) {
k <- i
  
assign(paste0("dados", i), i) <- as.data.frame(summary(
  na.omit(k))) 
  
}



mylist <- list(df1, df2, df3)
for (i in seq_along(mylist)) {
  mylist[[i]]$date <- as.Date(mylist[[i]]$date, "%Y-%m-%d")
}




assign(paste0("test", i), i)


Object = get(paste0("Season", i))
Object[1] = 0
assign(paste0("Season", i), Object)






rm(b_1,b_1_carteira)





library(PNADcIBGE)
library(tidyverse)
library(survey)
library(data.table)

var_select <- c("VD4002", "V2009", "VD4019", "VD3004", "VD4010", 
                "VD4008", "VD4009", "VD4011" ,"VD4012")

anos <-as.list(rep(2012:2021, times=4))
trimestre <-as.list(rep(1:4, times=10))

for (i in seq_along(anos)) {
  
  a <- get_pnadc(year = anos[[i]], quarter = trimestre[[i]], vars = var_select)
  assign(paste0("pnad",anos[[i]], trimestre[[i]], sep = "_" ), a )
  
  
}

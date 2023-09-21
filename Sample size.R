# rm(list = ls())
library(ggplot2)
library(dplyr)
library(magrittr)
library(openxlsx)
library(tidyr)#spread
library(irr)# corr intra clas icc

setwd("Documentos/Imarpe/Seguimiento")# path for windows
setwd("F:/Imarpe/Seguimiento")#path for linux
# Datos -------------------------------------------------------------------
data <- read.xlsx(xlsxFile = "BD-Lorna.xlsx", sheet = "Biometrico")
dato <- dato %>% filter(ANIO %in% (2000:2020)) 
dato[, 30] <- round(as.numeric(dato[, 30]))# sapply(dato[, 29], as.numeric)
data_nm <- dato %>% group_by(ANIO, MES, DIA, LABORATORIO, EMBARCACION, ARTE) %>% summarise(n=sum(FREC_ABSOLUTA),m=length(LONGITUD))

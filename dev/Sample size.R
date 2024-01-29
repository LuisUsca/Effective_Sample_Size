# rm(list = ls())
library(ggplot2)
library(dplyr)
library(magrittr)
library(openxlsx)
library(tidyr)#spread
library(irr)# corr intra clas icc

setwd("Documentos/Github/Effective_Sample_Size")# path for linux
setwd("F:/Imarpe/Seguimiento")#path for windows
# Datos -------------------------------------------------------------------
data <- read.xlsx(xlsxFile = "Data/Database.xlsx")


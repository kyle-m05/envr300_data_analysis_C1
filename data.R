# Data: Loaded the datasets and libraries

library(here)
library(dplyr)
library(ggplot2)




#Datasets

pred_temp_df <- read.csv(here("data", "Temp_GCMs.csv"))
NC_gas_df <- read.csv(here("data", "NG_CONS_SUM_DCU_SNC_M.csv"))

RDU_df <- read.csv(here("data", "USW00013722.csv"))
GSO_df <- read.csv(here("data", "USW00013881.csv"))
CLT_df <- read.csv(here("data", "USW00013881.csv"))

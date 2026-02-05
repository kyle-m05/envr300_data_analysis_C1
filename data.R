# Data: Loaded the datasets and libraries

library(tidyverse)
library(here)


#Loading the Datasets

#predicted temperature dataframe
pred_temp_df <- read.csv(here("data", "Temp_GCMs.csv"))
#natural gas consumption dataframe
NC_gas_df <- read.csv(here("data", "NG_CONS_SUM_DCU_SNC_M.csv"))
NC_gas_df <- NC_gas_df[-1, ] %>%
  select(Back.to.Contents,
         X,
         X.1) %>%
  rename(date = Back.to.Contents ,
         residential = X,
         commercial = X.1)
#airport weather dataframes
RDU_df <- read.csv(here("data", "USW00013722.csv"))

RDU_df <- RDU_df %>%
  select(DATE, CLDD, HTDD, TAVG, TMAX, TMIN)

GSO_df <- read.csv(here("data", "USW00013881.csv"))

GSO_df <- GSO_df %>%
  select(DATE, CLDD, HTDD, TAVG, TMAX, TMIN)

CLT_df <- read.csv(here("data", "USW00013881.csv"))

CLT_df <- CLT_df %>%
  select(DATE, CLDD, HTDD, TAVG, TMAX, TMIN)

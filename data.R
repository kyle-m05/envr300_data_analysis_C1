# Data: Loaded the datasets and libraries

library(tidyverse)
library(tidymodels)
library(here)
library(broom)
library(purrr)


#Loading the Datasets

#predicted temperature dataframe
pred_temp_df <- read.csv(here("data", "Temp_GCMs.csv"))
#natural gas consumption dataframe
NC_gas_df <- read.csv(here("data", "NG_CONS_SUM_DCU_SNC_M.csv"))
NC_gas_df <- NC_gas_df[-c(1,2), ] %>%
  select(Back.to.Contents,
         X,
         X.1) %>%
  rename(DATE = Back.to.Contents ,
         residential = X,
         commercial = X.1) %>%
  mutate(DATE = my(DATE),
         residential = as.numeric(residential),
         commercial = as.numeric(commercial)
  )

#airport weather dataframes
RDU_df <- read.csv(here("data", "USW00013722.csv"))

RDU_df <- RDU_df %>%
  select(DATE, CLDD, HTDD, TAVG, TMAX, TMIN) %>%
  mutate(
    DATE = {
      mon <- sub("-.*", "", DATE)
      yr2 <- as.integer(sub(".*-", "", DATE))
      yr4 <- if_else(yr2 <= 23, 2000 + yr2, 1900 + yr2)
      format(my(paste(mon, yr4)), "%Y-%m")
    }
  )

GSO_df <- read.csv(here("data", "USW00013881.csv"))

GSO_df <- GSO_df %>%
  select(DATE, CLDD, HTDD, TAVG, TMAX, TMIN) %>%
  mutate(DATE = format(ym(DATE), "%Y-%m"))

CLT_df <- read.csv(here("data", "USW00013881.csv"))

CLT_df <- CLT_df %>%
  select(DATE, CLDD, HTDD, TAVG, TMAX, TMIN) %>%
  mutate(DATE = format(ym(DATE), "%Y-%m"))







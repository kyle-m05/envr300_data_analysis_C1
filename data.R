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

#annual mean for gas consumption data
NC_gas_avg_df <- NC_gas_df %>%
  mutate(year = year(DATE)) %>%
  group_by(year) %>%
  summarize(
    mean_residential = mean(residential, na.rm = TRUE),
    mean_commercial = mean(commercial, na.rm = TRUE)
  )

#splitting the months into 'warm' and 'cold' months

NC_gas_cold_df <- NC_gas_df %>%
  mutate(
    year = year(DATE),
    month = month(DATE),
    year = if_else(month >= 11, year, year - 1)
  ) %>%
  filter(month %in% c(11, 12, 1, 2, 3, 4)) %>%
  group_by(year) %>%
  summarise(
    residential_cold = mean(residential, na.rm = TRUE),
    commercial_cold  = mean(commercial, na.rm = TRUE)
  )


NC_gas_warm_df <- NC_gas_df %>%
  filter(month(DATE) %in% c(5:10)) %>%
  mutate(year = year(DATE)) %>%
  group_by(year) %>%
  summarize(
    residential_warm = mean(residential, na.rm = TRUE),
    commercial_warm = mean(commercial, na.rm =)
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







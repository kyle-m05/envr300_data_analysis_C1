#averaging the weather data from the three airports

RDU_df_rename <- RDU_df %>%
    rename(CLDD_RDU = CLDD,
           HTDD_RDU = HTDD,
           TAVG_RDU = TAVG,
           TMAX_RDU = TMAX,
           TMIN_RDU = TMIN)

CLT_df_rename <- CLT_df %>%
  rename(CLDD_CLT = CLDD,
         HTDD_CLT = HTDD,
         TAVG_CLT = TAVG,
         TMAX_CLT = TMAX,
         TMIN_CLT = TMIN)

GSO_df_rename <- GSO_df %>%
  rename(CLDD_GSO = CLDD,
         HTDD_GSO = HTDD,
         TAVG_GSO = TAVG,
         TMAX_GSO = TMAX,
         TMIN_GSO = TMIN)

combined_df <- full_join(CLT_df_rename, GSO_df_rename, by = "DATE")

combined_df <- full_join(combined_df, RDU_df_rename, by = "DATE")

combined_df$DATE <- ym(combined_df$DATE)

combined_df <- full_join(combined_df, NC_gas_df, by = "DATE")

combined_df <- combined_df %>%
  filter(DATE >= as.Date("1989-01-01"))

airport_df <- combined_df %>%
  rowwise() %>%
  mutate(mean_CLDD = mean(c(CLDD_RDU, CLDD_CLT, CLDD_GSO), na.rm = TRUE),
         mean_HTDD = mean(c(HTDD_RDU, HTDD_CLT, HTDD_GSO), na.rm = TRUE),
         mean_TAVG = mean(c(TAVG_RDU, TAVG_CLT, TAVG_GSO), na.rm = TRUE),
         mean_TMAX = mean(c(TMAX_RDU, TMAX_CLT, TMAX_GSO), na.rm = TRUE),
         mean_TMIN = mean(c(TMIN_RDU, TMIN_CLT, TMIN_GSO), na.rm = TRUE)) %>%
  ungroup() %>%
  select(DATE, mean_CLDD, mean_HTDD, mean_TAVG, mean_TMAX, mean_TMIN, residential, commercial)

annual_df <- airport_df %>%
  mutate(
    year = year(DATE),
    month = month(DATE)) %>%
  group_by(year) %>%
  summarise(
    mean_CLDD = mean(mean_CLDD, na.rm = TRUE),
    mean_HTDD = mean(mean_HTDD, na.rm = TRUE),
    mean_TAVG = mean(mean_TAVG, na.rm = TRUE),
    mean_TMAX = mean(mean_TMAX, na.rm = TRUE),
    mean_TMIN = mean(mean_TMIN, na.rm = TRUE),
    residential = mean(residential, na.rm = TRUE),
    commercial  = mean(commercial,  na.rm = TRUE)
  )

cold_df <- airport_df %>%
  mutate(
    year = year(DATE),
    month = month(DATE),
    cold_year = if_else(month >= 11, year, year - 1)
  ) %>%
  filter(month %in% c(11,12,1,2,3,4)) %>%
  group_by(cold_year) %>%
  summarise(
    mean_CLDD = mean(mean_CLDD, na.rm = TRUE),
    mean_HTDD = mean(mean_HTDD, na.rm = TRUE),
    mean_TAVG = mean(mean_TAVG, na.rm = TRUE),
    mean_TMAX = mean(mean_TMAX, na.rm = TRUE),
    mean_TMIN = mean(mean_TMIN, na.rm = TRUE),
    residential = mean(residential, na.rm = TRUE),
    commercial  = mean(commercial,  na.rm = TRUE)
  ) %>%
  rename(year = cold_year)

warm_df <- airport_df %>%
  mutate(
    year = year(DATE),
    month = month()) %>%
  filter(month %in% 5:10) %>%
  group_by(year) %>%
  summarise(
    mean_CLDD = mean(mean_CLDD, na.rm = TRUE),
    mean_HTDD = mean(mean_HTDD, na.rm = TRUE),
    mean_TAVG = mean(mean_TAVG, na.rm = TRUE),
    mean_TMAX = mean(mean_TMAX, na.rm = TRUE),
    mean_TMIN = mean(mean_TMIN, na.rm = TRUE),
    residential = mean(residential, na.rm = TRUE),
    commercial  = mean(commercial,  na.rm = TRUE)
  )

#residential total correlations
vars <- c("mean_CLDD", "mean_HTDD", "mean_TAVG", "mean_TMAX", "mean_TMIN")

residential_cor_df <- map_df(vars, function(v) {
  test <- cor.test(annual_df$residential, annual_df[[v]], use = "complete.obs")
  
  tibble(
    variable = v,
    r = test$estimate,
    p_value = test$p.value
  )
})

residential_cor_df

#commercial total correlations
commercial_cor_df <- map_df(vars, function(v) {
  test <- cor.test(annual_df$commercial, annual_df[[v]], use = "complete.obs")
  
  tibble(
    variable = v,
    r = test$estimate,
    p_value = test$p.value
  )
})

commercial_cor_df

#warm correlations
residential_warm_cor_df <- map_df(vars, function(v) {
  test <- cor.test(warm_df$residential, warm_df[[v]], use = "complete.obs")
  
  tibble(
    variable = v,
    r = test$estimate,
    p_value = test$p.value
  )
})

residential_warm_cor_df

commercial_warm_cor_df <- map_df(vars, function(v) {
  test <- cor.test(warm_df$commercial, warm_df[[v]], use = "complete.obs")
  
  tibble(
    variable = v,
    r = test$estimate,
    p_value = test$p.value
  )
})

commercial_warm_cor_df

#cold correlations
residential_cold_cor_df <- map_df(vars, function(v) {
  test <- cor.test(cold_df$residential, cold_df[[v]], use = "complete.obs")
  
  tibble(
    variable = v,
    r = test$estimate,
    p_value = test$p.value
  )
})

residential_warm_cor_df

commercial_cold_cor_df <- map_df(vars, function(v) {
  test <- cor.test(cold_df$commercial, cold_df[[v]], use = "complete.obs")
  
  tibble(
    variable = v,
    r = test$estimate,
    p_value = test$p.value
  )
})

commercial_cold_cor_df

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

#residential total correlations
vars <- c("mean_CLDD", "mean_HTDD", "mean_TAVG", "mean_TMAX", "mean_TMIN")

residential_cor_df <- map_df(vars, function(v) {
  test <- cor.test(airport_df$residential, airport_df[[v]], use = "complete.obs")
  
  tibble(
    variable = v,
    r = test$estimate,
    p_value = test$p.value
  )
})

residential_cor_df

#commercial total correlations

vars <- c("mean_CLDD", "mean_HTDD", "mean_TAVG", "mean_TMAX", "mean_TMIN")

commercial_cor_df <- map_df(vars, function(v) {
  test <- cor.test(airport_df$commercial, airport_df[[v]], use = "complete.obs")
  
  tibble(
    variable = v,
    r = test$estimate,
    p_value = test$p.value
  )
})

commercial_cor_df

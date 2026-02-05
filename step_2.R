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

combined_df <- full_join(airport_df, RDU_df_rename, by = "DATE")




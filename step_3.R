#workspace for the last step of the analysis

#cleaning and prepping pred_temp_df

pred_annual_df <- pred_temp_df %>%
  group_by(Year) %>%
  summarize(
    CanESM2 = mean(CanESM2),
    CSIRO.Mk3.6.0 = mean(CSIRO.Mk3.6.0),
    GFDL.CM3 = mean(GFDL.CM3),
    MIROC.ESM = mean(MIROC.ESM),
    MPI.ESM.LR = mean(MPI.ESM.LR)
  )

pred_warm_df <- pred_temp_df %>%
  filter(Month %in% c(5:10)) %>%
  group_by(Year) %>%
  summarize(
    CanESM2 = mean(CanESM2),
    CSIRO.Mk3.6.0 = mean(CSIRO.Mk3.6.0),
    GFDL.CM3 = mean(GFDL.CM3),
    MIROC.ESM = mean(MIROC.ESM),
    MPI.ESM.LR = mean(MPI.ESM.LR)
  )

pred_cold_df <- pred_temp_df %>%
  mutate(cold_year = if_else(Month >= 11, Year, Year - 1)) %>%
  filter(Month %in% c(11,12,1,2,3,4)) %>%
  group_by(Year) %>%
  summarize(
    CanESM2 = mean(CanESM2),
    CSIRO.Mk3.6.0 = mean(CSIRO.Mk3.6.0),
    GFDL.CM3 = mean(GFDL.CM3),
    MIROC.ESM = mean(MIROC.ESM),
    MPI.ESM.LR = mean(MPI.ESM.LR)
  )

#predicting models
TAVG_model <- lm(residential ~ mean_TAVG, data = airport_df)

TAVG_model















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
  group_by(cold_year) %>%
  summarize(
    CanESM2 = mean(CanESM2),
    CSIRO.Mk3.6.0 = mean(CSIRO.Mk3.6.0),
    GFDL.CM3 = mean(GFDL.CM3),
    MIROC.ESM = mean(MIROC.ESM),
    MPI.ESM.LR = mean(MPI.ESM.LR)
  ) %>%
  rename(Year = cold_year)


#calculating temperature trends for each GCM:

gcms <- c("CanESM2", "CSIRO.Mk3.6.0", "GFDL.CM3", "MIROC.ESM", "MPI.ESM.LR")

GCM_temp_trends <- map_df(gcms, function(x) {
  
  f <- as.formula(paste(x, "~ Year"))
  
  test <- tidy(lm(f, data = pred_annual_df))
  
  tibble(
    GCM = x,
    slope = test$estimate[test$term == "Year"],
    p_value = test$p.value[test$term == "Year"]
    )
})

GCM_temp_trends

GCM_cold_trends <- map_df(gcms, function(x) {
  
  f <- as.formula(paste(x, "~ Year"))
  
  test <- tidy(lm(f, data = pred_cold_df))
  
  tibble(
    GCM = x,
    slope = test$estimate[test$term == "Year"],
    p_value = test$p.value[test$term == "Year"]
  )
})

GCM_cold_trends

GCM_warm_trends <- map_df(gcms, function(x) {
  
  f <- as.formula(paste(x, "~ Year"))
  
  test <- tidy(lm(f, data = pred_warm_df))
  
  tibble(
    GCM = x,
    slope = test$estimate[test$term == "Year"],
    p_value = test$p.value[test$term == "Year"]
  )
})

GCM_warm_trends

# RESIDENTIAL MODEL
TAVG_model_res <- lm(residential ~ mean_TAVG, data = airport_df)

# ---- CanESM2 ----
CanESM2_cold_res <- pred_cold_df %>%
  mutate(mean_TAVG = CanESM2) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_res, newdata = .),
         GCM = "CanESM2",
         type = "residential")

CanESM2_warm_res <- pred_warm_df %>%
  mutate(mean_TAVG = CanESM2) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_res, newdata = .),
         GCM = "CanESM2",
         type = "residential")

# ---- CSIRO ----
CSIRO_cold_res <- pred_cold_df %>%
  mutate(mean_TAVG = CSIRO.Mk3.6.0) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_res, newdata = .),
         GCM = "CSIRO.Mk3.6.0",
         type = "residential")

CSIRO_warm_res <- pred_warm_df %>%
  mutate(mean_TAVG = CSIRO.Mk3.6.0) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_res, newdata = .),
         GCM = "CSIRO.Mk3.6.0",
         type = "residential")

# ---- GFDL ----
GFDL_cold_res <- pred_cold_df %>%
  mutate(mean_TAVG = GFDL.CM3) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_res, newdata = .),
         GCM = "GFDL.CM3",
         type = "residential")

GFDL_warm_res <- pred_warm_df %>%
  mutate(mean_TAVG = GFDL.CM3) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_res, newdata = .),
         GCM = "GFDL.CM3",
         type = "residential")

# ---- MIROC ----
MIROC_cold_res <- pred_cold_df %>%
  mutate(mean_TAVG = MIROC.ESM) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_res, newdata = .),
         GCM = "MIROC.ESM",
         type = "residential")

MIROC_warm_res <- pred_warm_df %>%
  mutate(mean_TAVG = MIROC.ESM) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_res, newdata = .),
         GCM = "MIROC.ESM",
         type = "residential")

GCM_warm_trends <- map_df(gcms, function(x) {
  
  f <- as.formula(paste(x, "~ Year"))
  
  test <- tidy(lm(f, data = pred_warm_df))
  
  tibble(
    GCM = x,
    slope = test$estimate[test$term == "Year"],
    p_value = test$p.value[test$term == "Year"]
  )
})

GCM_warm_trends

# RESIDENTIAL MODEL
TAVG_model_res <- lm(residential ~ mean_TAVG, data = airport_df)

# ---- CanESM2 ----
CanESM2_cold_res <- pred_cold_df %>%
  mutate(mean_TAVG = CanESM2) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_res, newdata = .),
         GCM = "CanESM2",
         type = "residential")

CanESM2_warm_res <- pred_warm_df %>%
  mutate(mean_TAVG = CanESM2) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_res, newdata = .),
         GCM = "CanESM2",
         type = "residential")

# ---- CSIRO ----
CSIRO_cold_res <- pred_cold_df %>%
  mutate(mean_TAVG = CSIRO.Mk3.6.0) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_res, newdata = .),
         GCM = "CSIRO.Mk3.6.0",
         type = "residential")

CSIRO_warm_res <- pred_warm_df %>%
  mutate(mean_TAVG = CSIRO.Mk3.6.0) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_res, newdata = .),
         GCM = "CSIRO.Mk3.6.0",
         type = "residential")

# ---- GFDL ----
GFDL_cold_res <- pred_cold_df %>%
  mutate(mean_TAVG = GFDL.CM3) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_res, newdata = .),
         GCM = "GFDL.CM3",
         type = "residential")

GFDL_warm_res <- pred_warm_df %>%
  mutate(mean_TAVG = GFDL.CM3) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_res, newdata = .),
         GCM = "GFDL.CM3",
         type = "residential")

# ---- MIROC ----
MIROC_cold_res <- pred_cold_df %>%
  mutate(mean_TAVG = MIROC.ESM) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_res, newdata = .),
         GCM = "MIROC.ESM",
         type = "residential")

MIROC_warm_res <- pred_warm_df %>%
  mutate(mean_TAVG = MIROC.ESM) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_res, newdata = .),
         GCM = "MIROC.ESM",
         type = "residential")

# ---- MPI ----
MPI_cold_res <- pred_cold_df %>%
  mutate(mean_TAVG = MPI.ESM.LR) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_res, newdata = .),
         GCM = "MPI.ESM.LR",
         type = "residential")

MPI_warm_res <- pred_warm_df %>%
  mutate(mean_TAVG = MPI.ESM.LR) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_res, newdata = .),
         GCM = "MPI.ESM.LR",
         type = "residential")

# COMMERCIAL MODEL
TAVG_model_com <- lm(commercial ~ mean_TAVG, data = airport_df)

# ---- CanESM2 ----
CanESM2_cold_com <- pred_cold_df %>%
  mutate(mean_TAVG = CanESM2) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_com, newdata = .),
         GCM = "CanESM2",
         type = "commercial")

CanESM2_warm_com <- pred_warm_df %>%
  mutate(mean_TAVG = CanESM2) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_com, newdata = .),
         GCM = "CanESM2",
         type = "commercial")

# ---- CSIRO ----
CSIRO_cold_com <- pred_cold_df %>%
  mutate(mean_TAVG = CSIRO.Mk3.6.0) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_com, newdata = .),
         GCM = "CSIRO.Mk3.6.0",
         type = "commercial")

CSIRO_warm_com <- pred_warm_df %>%
  mutate(mean_TAVG = CSIRO.Mk3.6.0) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_com, newdata = .),
         GCM = "CSIRO.Mk3.6.0",
         type = "commercial")

# ---- GFDL ----
GFDL_cold_com <- pred_cold_df %>%
  mutate(mean_TAVG = GFDL.CM3) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_com, newdata = .),
         GCM = "GFDL.CM3",
         type = "commercial")

GFDL_warm_com <- pred_warm_df %>%
  mutate(mean_TAVG = GFDL.CM3) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_com, newdata = .),
         GCM = "GFDL.CM3",
         type = "commercial")

# ---- MIROC ----
MIROC_cold_com <- pred_cold_df %>%
  mutate(mean_TAVG = MIROC.ESM) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_com, newdata = .),
         GCM = "MIROC.ESM",
         type = "commercial")

MIROC_warm_com <- pred_warm_df %>%
  mutate(mean_TAVG = MIROC.ESM) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_com, newdata = .),
         GCM = "MIROC.ESM",
         type = "commercial")

# ---- MPI ----
MPI_cold_com <- pred_cold_df %>%
  mutate(mean_TAVG = MPI.ESM.LR) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_com, newdata = .),
         GCM = "MPI.ESM.LR",
         type = "commercial")

MPI_warm_com <- pred_warm_df %>%
  mutate(mean_TAVG = MPI.ESM.LR) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_com, newdata = .),
         GCM = "MPI.ESM.LR",
         type = "commercial")

MPI_warm_res <- pred_warm_df %>%
  mutate(mean_TAVG = MPI.ESM.LR) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_res, newdata = .),
         GCM = "MPI.ESM.LR",
         type = "residential")

# COMMERCIAL MODEL
TAVG_model_com <- lm(commercial ~ mean_TAVG, data = airport_df)

# ---- CanESM2 ----
CanESM2_cold_com <- pred_cold_df %>%
  mutate(mean_TAVG = CanESM2) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_com, newdata = .),
         GCM = "CanESM2",
         type = "commercial")

CanESM2_warm_com <- pred_warm_df %>%
  mutate(mean_TAVG = CanESM2) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_com, newdata = .),
         GCM = "CanESM2",
         type = "commercial")

# ---- CSIRO ----
CSIRO_cold_com <- pred_cold_df %>%
  mutate(mean_TAVG = CSIRO.Mk3.6.0) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_com, newdata = .),
         GCM = "CSIRO.Mk3.6.0",
         type = "commercial")

CSIRO_warm_com <- pred_warm_df %>%
  mutate(mean_TAVG = CSIRO.Mk3.6.0) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_com, newdata = .),
         GCM = "CSIRO.Mk3.6.0",
         type = "commercial")

# ---- GFDL ----
GFDL_cold_com <- pred_cold_df %>%
  mutate(mean_TAVG = GFDL.CM3) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_com, newdata = .),
         GCM = "GFDL.CM3",
         type = "commercial")

GFDL_warm_com <- pred_warm_df %>%
  mutate(mean_TAVG = GFDL.CM3) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_com, newdata = .),
         GCM = "GFDL.CM3",
         type = "commercial")

# ---- MIROC ----
MIROC_cold_com <- pred_cold_df %>%
  mutate(mean_TAVG = MIROC.ESM) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_com, newdata = .),
         GCM = "MIROC.ESM",
         type = "commercial")

MIROC_warm_com <- pred_warm_df %>%
  mutate(mean_TAVG = MIROC.ESM) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_com, newdata = .),
         GCM = "MIROC.ESM",
         type = "commercial")

# ---- MPI ----
MPI_cold_com <- pred_cold_df %>%
  mutate(mean_TAVG = MPI.ESM.LR) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_com, newdata = .),
         GCM = "MPI.ESM.LR",
         type = "commercial")

MPI_warm_com <- pred_warm_df %>%
  mutate(mean_TAVG = MPI.ESM.LR) %>%
  select(Year, mean_TAVG) %>%
  mutate(pred = predict(TAVG_model_com, newdata = .),
         GCM = "MPI.ESM.LR",
         type = "commercial")
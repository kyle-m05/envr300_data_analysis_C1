
#overall residential consumption plot

residential_plot <- NC_gas_df %>%
  ggplot() +
  geom_point(aes(x = date, y = residential)) +
  labs(
    x = "Date",
    y = "Natural Gas Consumption (MMcf)",
    title = "Residential Natural Gas Consumption 1983-2023"
  )
    

residential_plot

#linear model for residential consumption

residential_lm <- lm(residential ~ date, data = NC_gas_df)

summary(residential_lm)


#overall commercial consumption plot

commercial_plot <- NC_gas_df %>%
  ggplot() +
  geom_point(aes(x = date, y = commercial)) +
  labs(
    x = "Date",
    y = "Natural Gas Consumption (MMcf)",
    title = "Commercial Natural Gas Consumption 1983-2023"
  )

commercial_plot
    

#linear model for commercial consumption

commercial_lm <- lm(commercial ~ date, data = NC_gas_df)

summary(commercial_lm)














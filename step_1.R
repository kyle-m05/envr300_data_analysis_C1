
#overall residential yearly mean consumption plot

residential_plot <- NC_gas_avg_df %>%
  ggplot() +
  geom_point(aes(x = year, y = mean_residential)) +
  geom_smooth(aes(x = year, y = mean_residential), 
              method = "lm", formula = y ~ x, se = FALSE) +
  labs(
    x = "Year",
    y = "Natural Gas Consumption (MMcf)",
    title = "Annual Mean Residential Natural Gas Consumption 1989-2023"
  )
    

residential_plot

#linear model for residential consumption

residential_lm <- lm(mean_residential ~ year, data = NC_gas_avg_df)

tidy(residential_lm) %>%
  mutate_if(is.numeric, round, 4)


#overall commercial consumption plot

commercial_plot <- NC_gas_avg_df %>%
  ggplot() +
  geom_point(aes(x = year, y = mean_commercial)) +
  geom_smooth(aes(x = year, y = mean_commercial), method = "lm", formula = y ~ x, se = FALSE) +
  labs(
    x = "Date",
    y = "Natural Gas Consumption (MMcf)",
    title = "Mean Annual Commercial Natural Gas Consumption 1989-2023"
  )

commercial_plot
    

#linear model for commercial consumption

commercial_lm <- lm(mean_commercial ~ year, data = NC_gas_avg_df)

tidy(commercial_lm) %>%
  mutate_if(is.numeric, round, 4)


#creating plots for warm months

residential_warm_plot <- NC_gas_warm_df %>%
  ggplot() +
  geom_point(aes(x = year, y = residential_warm)) +
  geom_smooth(aes(x = year, y = residential_warm),
              method = "lm", formula = y ~ x, se = FALSE) +
  labs(
    x = "Date",
    y = "Natural Gas Consumption (MMcf)",
    title = "Residential Natural Gas Consumption of 'Warm' Months"
  )

residential_warm_plot

commercial_warm_plot <- NC_gas_warm_df %>%
  ggplot() +
  geom_point(aes(x = year, y = commercial_warm)) +
  geom_smooth(aes(x = year, y = commercial_warm), 
              method = "lm", formula = y ~ x, se = FALSE) +
  labs(
    x = "Date",
    y = "Natural Gas Consumption (MMcf)",
    title = "Commercial Natural Gas Consumption of 'Warm' Months"
  )

commercial_warm_plot

#creating plots for cold months

residential_cold_plot <- NC_gas_cold_df %>%
  ggplot() +
  geom_point(aes(x = year, y = residential_cold)) +
  geom_smooth(aes(x = year, y = residential_cold), 
              method = "lm", formula = y ~ x, se = FALSE) +
  labs(
    x = "Date",
    y = "Natural Gas Consumption (MMcf)",
    title = "Residential Natural Gas Consumption of 'Cold' Months"
  )

residential_cold_plot

commercial_cold_plot <- NC_gas_cold_df %>%
  ggplot() +
  geom_point(aes(x = year, y = commercial_cold)) +
  geom_smooth(aes(x = year, y = commercial_cold), 
              method = "lm", formula = y ~ x, se = FALSE) +
  labs(
    x = "Date",
    y = "Natural Gas Consumption (MMcf)",
    title = "Commercial Natural Gas Consumption of 'Cold' Months"
  )

commercial_cold_plot

#getting slope for cold and warm months

commercial_cold_lm <- tidy(lm(commercial_cold ~ year, data = NC_gas_cold_df))
commercial_cold_lm

commercial_warm_lm <- tidy(lm(commercial_warm ~ year, data = NC_gas_warm_df))
commercial_warm_lm

residential_cold_lm <- tidy(lm(residential_cold ~ year, data = NC_gas_cold_df))
residential_cold_lm

residential_warm_lm <- tidy(lm(residential_warm ~ year, data = NC_gas_warm_df))
residential_warm_lm
  


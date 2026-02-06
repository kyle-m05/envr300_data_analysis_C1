
#overall residential consumption plot

residential_plot <- NC_gas_df %>%
  ggplot() +
  geom_point(aes(x = date, y = residential)) +
  geom_smooth(aes(x = date, y = residential), 
              method = "lm", formula = y ~ x, se = FALSE) +
  labs(
    x = "Date",
    y = "Natural Gas Consumption (MMcf)",
    title = "Residential Natural Gas Consumption 1983-2023"
  )
    

residential_plot

#linear model for residential consumption

residential_lm <- lm(residential ~ date, data = NC_gas_df)

tidy(residential_lm)


#overall commercial consumption plot

commercial_plot <- NC_gas_df %>%
  ggplot() +
  geom_point(aes(x = date, y = commercial)) +
  geom_smooth(aes(x = date, y = commercial), method = "lm", formula = y ~ x, se = FALSE) +
  labs(
    x = "Date",
    y = "Natural Gas Consumption (MMcf)",
    title = "Commercial Natural Gas Consumption 1983-2023"
  )

commercial_plot
    

#linear model for commercial consumption

commercial_lm <- lm(commercial ~ date, data = NC_gas_df)

tidy(commercial_lm)

#splitting the months into 'warm' and 'cold' months

NC_gas_cold_df <- NC_gas_df %>%
  filter(month(date) %in% c(1,2,3,4,11,12))

NC_gas_warm_df <- NC_gas_df %>%
  filter(month(date) %in% c(5:10))

#creating plots for warm months

residential_warm_plot <- NC_gas_warm_df %>%
  ggplot() +
  geom_point(aes(x = date, y = residential)) +
  geom_smooth(aes(x = date, y = residential), 
              method = "lm", formula = y ~ x, se = FALSE) +
  labs(
    x = "Date",
    y = "Natural Gas Consumption (MMcf)",
    title = "Residential Natural Gas Consumption of 'Warm' Months"
  )

residential_warm_plot

commercial_warm_plot <- NC_gas_warm_df %>%
  ggplot() +
  geom_point(aes(x = date, y = commercial)) +
  geom_smooth(aes(x = date, y = commercial), 
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
  geom_point(aes(x = date, y = residential)) +
  geom_smooth(aes(x = date, y = residential), 
              method = "lm", formula = y ~ x, se = FALSE) +
  labs(
    x = "Date",
    y = "Natural Gas Consumption (MMcf)",
    title = "Residential Natural Gas Consumption of 'Cold' Months"
  )

residential_cold_plot

commercial_cold_plot <- NC_gas_cold_df %>%
  ggplot() +
  geom_point(aes(x = date, y = commercial)) +
  geom_smooth(aes(x = date, y = commercial), 
              method = "lm", formula = y ~ x, se = FALSE) +
  labs(
    x = "Date",
    y = "Natural Gas Consumption (MMcf)",
    title = "Commercial Natural Gas Consumption of 'Cold' Months"
  )

commercial_cold_plot







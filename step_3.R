#workspace for the last step of the analysis

#cleaning and prepping pred_temp_df

pred_temp_avg_df <- pred_temp_df %>%
  group_by(Year) 
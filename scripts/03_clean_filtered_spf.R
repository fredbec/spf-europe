rm(list = ls())
cat("\014")

library(dplyr)
library(here)

source(here("scripts", "clean_filtered_spf", "clean_quarterly_spf_functions.R"))


### Median consensus
SPF_median <- data_function_spf(ConsensusMedian = TRUE)

# Prepare cleaned data set
SPF_median_clean <- SPF_median$SPF_consensus$spf_forecasts_ny %>%
  left_join(
    SPF_median$SPF_consensus$evaluation_data_ny %>%
      select(target_year, target_quarter, gdp_growth) %>%
      distinct(target_year, target_quarter, .keep_all = TRUE),
    by = c("target_year", "target_quarter")
  ) %>%
  rename(rgdp = gdp_growth,
         survey_period = ref_period) %>%
  select(survey_period, everything(),
         -target_year, -target_quarter)

# Save data sets
saveRDS(SPF_median, file = here("output","filter_spf","spf_consensus_and_panel_clean_version","SPF_median_full.rds"))

write.csv(
  SPF_median_clean,
  file = here("output","filter_spf","spf_consensus_and_panel_clean_version","SPF_median_clean.csv"),
  row.names = FALSE
)



### Mean consensus
SPF_mean <- data_function_spf(ConsensusMedian = FALSE)

# Prepare cleaned data set
SPF_mean_clean <- SPF_mean$SPF_consensus$spf_forecasts_ny %>%
  left_join(
    SPF_mean$SPF_consensus$evaluation_data_ny %>%
      select(target_year, target_quarter, gdp_growth) %>%
      distinct(target_year, target_quarter, .keep_all = TRUE),
    by = c("target_year", "target_quarter")
  ) %>%
  rename(rgdp = gdp_growth,
         survey_period = ref_period) %>%
  select(survey_period, everything(),
         -target_year, -target_quarter)

# Save data sets
saveRDS(SPF_mean, file = here("output","filter_spf","spf_consensus_and_panel_clean_version","SPF_mean_full.rds"))

write.csv(
  SPF_mean_clean,
  file = here("output","filter_spf","spf_consensus_and_panel_clean_version","SPF_mean_clean.csv"),
  row.names = FALSE
)



### Forecaster panel
SPF_panel <- data_function_spf(SPFPanel = TRUE)

# Save data sets
saveRDS(SPF_panel, file = here("output","filter_spf","spf_consensus_and_panel_clean_version","SPF_panel_full.rds"))

SPF_panel_clean <- SPF_panel$SPF_panel %>%
  left_join(
    SPF_panel$SPF_panel_eval %>%
      select(target_year, target_quarter, gdp_growth) %>%
      distinct(target_year, target_quarter, .keep_all = TRUE),
    by = c("target_year", "target_quarter")
  ) %>%
  rename(rgdp = gdp_growth,
         survey_period = ref_period) %>%
  select(forecaster_id, survey_period, everything(),
         -target_year, -target_quarter)

write.csv(
  SPF_panel_clean,
  file = here("output","filter_spf","spf_consensus_and_panel_clean_version","SPF_panel_clean.csv"),
  row.names = FALSE
)


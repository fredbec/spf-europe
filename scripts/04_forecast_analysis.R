rm(list = ls())
cat("\014")

library(dplyr)      # For data manipulation: filtering, mutating, grouping, etc.
library(zoo)        # Provides 'yearqtr' and 'yearmon' classes
library(tidyr)      # For reshaping data
library(ggplot2)    # For plotting

library(here)

SPF <- readRDS(
  here("output","filter_spf","spf_consensus_and_panel_clean_version","SPF_median_full.rds")
)
SPF <- SPF$SPF_consensus


### Quick plots

## Plot filtered values using current versus current and next year SPF forecasts

# Merge CY and NY forecasts into one table with gdp_growth
plot_data <- SPF$evaluation_data_cy %>%
  filter(ref_period >= as.yearqtr("2000 Q1", format = "%Y Q%q"),
         ref_period <= as.yearqtr("2019 Q4", format = "%Y Q%q")) %>%
  select(ref_period, gdp_growth, spf_cy = spf_h0) %>%
  left_join(
    SPF$evaluation_data_ny %>% select(ref_period, spf_ny = spf_h0),
    by = "ref_period"
  ) %>%
  pivot_longer(cols = c(gdp_growth, spf_cy, spf_ny),
               names_to = "series",
               values_to = "value")

# Plot
ggplot(plot_data, aes(x = ref_period, y = value, color = series)) +
  geom_line(size = 1) +
  labs(
    title = "GDP Growth vs SPF Forecasts (CY & NY)",
    x = "Reference Quarter",
    y = "Value",
    color = "Series"
  ) +
  theme_minimal()


## Plots for forecast horizons h = 1,2,...,4 and store as .png
for(i in 0:4){
  spf_col <- paste0("spf_h", i)   # spf_h0, spf_h1, ...
  file_name <- paste0("plot_results/SPF_h", i, ".png")

  # Open PDF device
  png(file_name, width = 6.5*400, height = 5*400, res = 400)

  plot_data <- SPF$evaluation_data_cy %>%
    filter(ref_period >= as.yearqtr("2000 Q1", format = "%Y Q%q"),
           ref_period <= as.yearqtr("2019 Q4", format = "%Y Q%q")) %>%
    select(ref_period, gdp_growth, spf_cy = all_of(spf_col)) %>%
    left_join(
      SPF$evaluation_data_ny %>% select(ref_period, spf_ny = all_of(spf_col)),
      by = "ref_period"
    )

  ref_dates <- as.Date(as.yearqtr(plot_data$ref_period, format = "%Y Q%q"))

  # Example recession periods (replace with your actual data if you have it)
  recessions <- data.frame(
    start = as.Date(c("2001-03-01", "2007-12-01", "2020-02-01")),
    end   = as.Date(c("2001-11-01", "2009-06-01", "2020-04-01"))
  )

  # Global plot options
  par(mar = c(3, 3, 1, 1), xaxs = "i")
  par(cex.axis = 1.4,   # axis tick labels (numbers)
      cex.lab  = 1.6,   # axis titles (xlab, ylab)
      cex.main = 1.6)   # main title (if used)


  # Draw empty frame first
  plot(ref_dates, plot_data$gdp_growth,
       type = "n",                 # <-- important: no lines yet
       xlab = " ", ylab = " ",
       bty = "n", xaxs = "i")

  # Add recession shading
  for(i in 1:nrow(recessions)){
    rect(recessions$start[i], par("usr")[3],
         recessions$end[i],   par("usr")[4],
         col = gray(0.8, alpha = 0.6),   # semi-transparent
         border = NA)
  }

  # Add time series
  lines(ref_dates, plot_data$gdp_growth, col = "black", lwd = 3)
  lines(ref_dates, plot_data$spf_ny, col = "#D95319",  lwd = 3)

  legend("bottomleft",
         legend = c("GDP Growth", "quarterly SPF"),
         col = c("black", "#D95319"),
         lwd = 3,
         bty = "o",
         inset = 0.02,
         cex = 1.2)

  # Now add the full box
  box(bty = "o", lwd = 1.5)

  # Close the PDF device
  dev.off()

}




#### Forecasts from AR Benchmark models
source(here("scripts", "in_and_out_of_sample_analysis", "ar_benchmark_quarterly.R"))
source(here("scripts", "in_and_out_of_sample_analysis", "ar_benchmark_yearly.R"))
source(here("scripts", "in_and_out_of_sample_analysis", "bias_test.R"))

# Quarterly GDP growth forecasts
AR_bench_quarterly <- ar_benchmark_quarterly(SPF$rgdp_all, ar_length = 30,
                                             rw_length = 4,
                                             max_lag = 4,
                                             SampleEnd = 2026,
                                             endMonth = 2)

# Yearly GDP growth forecasts
AR_bench_yearly <- ar_benchmark_yearly(SPF$rgdp_all, ar_length = 30,
                                       rw_length = 4,
                                       max_lag = 1,
                                       SampleEnd = 2026,
                                       endMonth = 2)



#### Forecast analysis

### Calculate bias and RMSE of SPF forecasts
dropYears  <- NA # cbind(2009, 2009)
evalPeriod <- cbind(2002,2019)


# Bias (Mean Error) of filtered SPF using current and next year projections
SPF_ME <- SPF_bias(SPF$evaluation_data_ny, DropPeriod = dropYears, EvalPeriod = evalPeriod)

# Evaluation of quarterly filtered SPF against benchmark models
RMSE_quarterly <- SPF_RMSE_DM_Test_quarterly(SPF$evaluation_data_ny, AR_bench_quarterly,
                                             DropPeriod = dropYears,
                                             EvalPeriod = evalPeriod)

# Evaluation of yearly SPF against benchmark models
RMSE_yearly <- SPF_RMSE_DM_Test_yearly(SPF$spf_annual, AR_bench_yearly,
                                       DropPeriod = dropYears,
                                       EvalPeriod = evalPeriod)




#### Print the results

# Bias of quarterly SPF
round(SPF_ME,2)

# Root Mean Squared Errors of quarterly SPF
RMSE_quarterly$RMSE

# DM test statistics of quarterly SPF
round(RMSE_quarterly$DM_Test,2)
RMSE_quarterly$DM_stars

# RMSE and DM test of yearly SPF forecasts
round(RMSE_yearly$RMSE_yearly,2)
RMSE_yearly$DM_stars_yearly





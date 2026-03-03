rm(list = ls())
cat("\014")

library(dplyr)      # For data manipulation: filtering, mutating, grouping, etc.
library(zoo)        # Provides 'yearqtr' and 'yearmon' classes
library(tidyr)      # For reshaping data
library(ggplot2)    # For plotting
library(readxl)     # For reading in .xlsx files

library(here)


### Read in filtered SPF forecasts

# Consensus
SPF <- readRDS(
  here("output","filter_spf","spf_consensus_and_panel_clean_version","SPF_median_full.rds")
)
SPF <- SPF$SPF_consensus

# Panel
SPF_panel <- readRDS(
  here("output","filter_spf","spf_consensus_and_panel_clean_version","SPF_panel_full.rds")
)
SPF_panel <- SPF_panel$SPF_panel_eval



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



### Some information on the panel of forecasters

# How many panelists per quarter
panel_size_ts <- SPF_panel %>%
  group_by(ref_period) %>%
  summarise(
    n_panelists = sum(!is.na(spf_h0)),
    .groups = "drop"
  ) %>%
  arrange(ref_period)

panel_size_ts_trimmed <- panel_size_ts[1:(nrow(panel_size_ts) - 4), ]

# Summary
summary(panel_size_ts_trimmed$n_panelists)




#### Forecasts from AR Benchmark models
source(here("scripts", "in_and_out_of_sample_analysis", "ar_benchmark_quarterly.R"))
source(here("scripts", "in_and_out_of_sample_analysis", "ar_benchmark_yearly.R"))

# Quarterly GDP growth forecasts
AR_bench_quarterly <- ar_benchmark_quarterly(SPF$rgdp_all, ar_length = 30,
                                             rw_length = 1000, # real-time historical mean
                                             max_lag = 4,
                                             SampleEnd = 2026,
                                             endMonth = 2)

# Yearly GDP growth forecasts
AR_bench_yearly <- ar_benchmark_yearly(SPF$rgdp_all, ar_length = 30,
                                       rw_length = 4,
                                       max_lag = 1,
                                       SampleEnd = 2026,
                                       endMonth = 2)



###### Forecast analysis
source(here("scripts", "in_and_out_of_sample_analysis", "rmse_bias.R"))
source(here("scripts", "in_and_out_of_sample_analysis", "mz_reg.R"))
source(here("scripts", "in_and_out_of_sample_analysis", "forecast_revision.R"))


dropYears  <- cbind(2020, 2021) # Drop pandemic and 2022 because of large forecast errors
evalPeriod <- cbind(2002, 2019)
decimals <- 2

##### Consensus forecasts
SPF_cons <- SPF$evaluation_data_ny
SPF_annual <- SPF$spf_annual


### Bias, MZ-regression, forecast efficiency

# Summary stats for main evaluation period
SummaryStatsMain <- Summary_Stats(SPF_cons, DropPeriod = dropYears, EvalPeriod = evalPeriod)

# Bias (Mean Error) of filtered quarterly SPF using current and next year projections
SPF_ME_median_cons <- SPF_bias(SPF_cons, DropPeriod = dropYears, EvalPeriod = evalPeriod)

# Mincer-Zarnowitz regressions
MZReg_median_cons <- MZReg(SPF_cons, EvalPeriod = evalPeriod, digits = decimals)

# Forecast errors on forecast revisions
ErrorOnRev_median_cons <- ErrorsOnRevisionCons(SPF_cons, EvalPeriod = evalPeriod, digits = decimals)

# Forecast revisions on revisions
RevOnRev_median_cons <- RevisionsOnRevisionCons(SPF_cons, EvalPeriod = evalPeriod, digits = decimals)



### Out-of-sample forecast performance

# Evaluation of quarterly filtered SPF against benchmark models
RMSE_quarterly_median_cons <- SPF_RMSE_DM_Test_quarterly(SPF_cons, AR_bench_quarterly,
                                                         DropPeriod = dropYears,
                                                         EvalPeriod = evalPeriod)

RMSE_quarterly_median_cons_no_GFC <- SPF_RMSE_DM_Test_quarterly(SPF_cons, AR_bench_quarterly,
                                                                DropPeriod = cbind(2009, 2009),
                                                                EvalPeriod = evalPeriod)

# Evaluation of yearly SPF against benchmark models
RMSE_yearly_median_cons <- SPF_RMSE_DM_Test_yearly(SPF_annual, AR_bench_yearly,
                                                   DropPeriod = dropYears,
                                                   EvalPeriod = evalPeriod)



##### Panel of SPF forecasts

# Bias
Bias_panel <- BiasSPFPanel(SPF_panel, EvalPeriod = evalPeriod, digits = decimals)

# Mincer-Zarnowitz regression
MZReg_panel <- MZRegPanel(SPF_panel, EvalPeriod = evalPeriod, digits = decimals)

# Forecast errors on forecast revisions (FixedEffects = TRUE yields same results)
ErrorOnRev_panel <- ErrorsOnRevisionPanel(SPF_panel, EvalPeriod = evalPeriod, digits = decimals)

# Forecast revisions on revisions
RevOnRev_panel <- RevisionsOnRevisionPanel(SPF_panel, EvalPeriod = evalPeriod, digits = decimals)


### Disagreement among panelists
source(here("scripts", "in_and_out_of_sample_analysis", "disagreement.R"))

# Sample from 2002 to 2019
disagreement_pre_covid <- SPF_IQR(SPF_panel, EvalPeriod = cbind(2002, 2019))
disagreement_summary_pre_covid <- round(t(disagreement_pre_covid$summary_stats), decimals)

disagreement <- SPF_IQR(SPF_panel, EvalPeriod = cbind(2002, 2024))
disagreement_summary <- round(t(disagreement$summary_stats), decimals)




####### Robustness

###  Consensus using both fixed-event and fixed-horizon forecasts
SPF_FH <- readRDS(
  here("output","filter_spf","spf_consensus_and_panel_clean_version","SPF_FH_median_full.rds")
)
SPF_FH <- SPF_FH$SPF_consensus

# Out-of-sample RMSE of fixed-event versus fixed-event and fixed-horizon
RMSE_quarterly_median_fh <- SPF_RMSE_DM_Test_quarterly(SPF_cons, AR_bench_quarterly,
                                                       DropPeriod = dropYears,
                                                       EvalPeriod = evalPeriod,
                                                       SPFalternative = SPF_FH$evaluation_data_ny)

### Mean as consensus forecast
SPF_mean <- readRDS(
  here("output","filter_spf","spf_consensus_and_panel_clean_version","SPF_mean_full.rds")
)
SPF_mean <- SPF_mean$SPF_consensus

# Out-of-sample RMSE of median versus mean
RMSE_quarterly_mean_cons <- SPF_RMSE_DM_Test_quarterly(SPF_cons, AR_bench_quarterly,
                                                       DropPeriod = dropYears,
                                                       EvalPeriod = evalPeriod,
                                                       SPFalternative = SPF_mean$evaluation_data_ny)


### Alternative consensus as median over individual filtered forecasts
new_h0 <- SPF_panel %>%
  group_by(ref_period) %>%
  summarise(
    spf_h0 = median(spf_h0, na.rm = TRUE),
    .groups = "drop"
  )

SPF_alt <- SPF_cons %>%
  select(-starts_with("spf_h")) %>%
  left_join(
    SPF_panel %>%
      group_by(ref_period) %>%
      summarise(
        spf_h0 = median(spf_h0, na.rm = TRUE),
        spf_h1 = median(spf_h1, na.rm = TRUE),
        spf_h2 = median(spf_h2, na.rm = TRUE),
        spf_h3 = median(spf_h3, na.rm = TRUE),
        spf_h4 = median(spf_h4, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "ref_period"
  )

# Out-of-sample RMSE (virtually identical results, also for mean)
RMSE_quarterly_panel_median_cons <- SPF_RMSE_DM_Test_quarterly(SPF_cons, AR_bench_quarterly,
                                                               DropPeriod = dropYears,
                                                               EvalPeriod = evalPeriod,
                                                               SPFalternative = SPF_alt)


### Forecasting performance over extended time period
evalPeriod <- cbind(2002,2024)

# Summary stats for whole evaluation period
SummaryStatsFull <- Summary_Stats(SPF_cons, DropPeriod = NA, EvalPeriod = evalPeriod)

# Bias
SPF_ME_median_cons_full_period <- SPF_bias(SPF_cons, DropPeriod = dropYears, EvalPeriod = evalPeriod)

# Mincer-Zarnowitz regressions
MZReg_median_cons_full_period <- MZReg(SPF_cons, DropPeriod = dropYears, EvalPeriod = evalPeriod, digits = decimals)

# RMSE
RMSE_quarterly_median_cons_full_period <- SPF_RMSE_DM_Test_quarterly(SPF_cons,
                                                                     AR_bench_quarterly,
                                                                     DropPeriod = dropYears,
                                                                     EvalPeriod = evalPeriod)

# Out-of-sample RMSE of current and next year versus current year forecasts
RMSE_quarterly_median_ny_cy <- SPF_RMSE_DM_Test_quarterly(SPF_cons, AR_bench_quarterly,
                                                          DropPeriod = dropYears,
                                                          EvalPeriod = evalPeriod,
                                                          SPFalternative = SPF$evaluation_data_cy)





####### Data to produce Matlab plots


# Economic Policy Uncertainty (EPU) Index (not used so far)
europe_pu <- read_excel("data/Europe_Policy_Uncertainty_Data.xlsx") %>%
  select(Year, Month, European_News_Index) %>%
  rename(EUP = European_News_Index) %>%
  filter(!is.na(EUP))


# Average monthly observations to quarterly frequency
europe_pu <- europe_pu %>%
  mutate(
    # Convert Year + Month to a date and then to yearqtr
    ref_period = as.yearqtr(paste(Year, Month, "1", sep = "-"), format = "%Y-%m-%d")
  ) %>%
  group_by(ref_period) %>%
  summarise(
    EUP = mean(EUP, na.rm = TRUE),  # average over months in the quarter
    .groups = "drop"
  ) %>%
  arrange(ref_period)


# Merge EPU with disagreement
disagreement <- disagreement$disagreement %>%
  left_join(
    europe_pu %>% select(ref_period, EUP),
    by = "ref_period"
  )

# Reshape to long format
disagreement_long <- disagreement %>%
  pivot_longer(
    cols = starts_with("IQR"),
    names_to = "horizon",
    values_to = "IQR"
  )


# Time series plot
ggplot(disagreement_long, aes(x = ref_period, y = IQR, color = horizon)) +
  geom_line(size = 1) +
  labs(
    x = "Quarter",
    y = "Interquartile Range of SPF forecasts",
    color = "Horizon"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



#### Store data to produce Matlab plots
MatlabPlots <- SPF$evaluation_data_ny %>%
  left_join(disagreement, by = "ref_period") %>%
  arrange(ref_period)

# Merge with filtered SPF using both fixed-event and fixed-horizon forecasts
SPF_FH <- readRDS(
  here("output","filter_spf","spf_consensus_and_panel_clean_version","SPF_FH_median_full.rds")
)
SPF_FH <- SPF_FH$SPF_consensus$evaluation_data_ny

SPF_FH <- SPF_FH %>%
  select(ref_period, spf_h0_fh = spf_h0, spf_h1_fh = spf_h1,
         spf_h2_fh = spf_h2, spf_h3_fh = spf_h3, spf_h4_fh = spf_h4)

MatlabPlots <- MatlabPlots %>%
  left_join(SPF_FH, by = "ref_period") %>%
  arrange(ref_period)

write.csv(
  MatlabPlots,
  file = here("matlab_plots", "SPF_plot_data.csv"),
  row.names = FALSE
)




######## Print the results

##### Median consensus

### Summary statistics
# 2002 - 2019
round(SummaryStatsMain,decimals)

# 2002 - 2024
round(SummaryStatsFull,decimals)

### Root Mean Squared Errors of quarterly SPF and DM-test results
# Main results
RMSE_quarterly_median_cons$RMSE
RMSE_quarterly_median_cons$DM_stars # significance levels

# Excluding GFC
RMSE_quarterly_median_cons_no_GFC$RMSE
RMSE_quarterly_median_cons_no_GFC$DM_stars

# Full sample, excluding COVID
RMSE_quarterly_median_cons_full_period$RMSE
RMSE_quarterly_median_cons_full_period$DM_stars

### Bias (Mean forecast error)
round(SPF_ME_median_cons,decimals)
round(SPF_ME_median_cons_full_period,decimals)

### Mincer-Zarnowitz regressions
print(MZReg_median_cons)
print(MZReg_median_cons_full_period)

### Errors-on-Revision regressions
print(ErrorOnRev_median_cons)

### Revisions-on-Revision regressions
print(RevOnRev_median_cons)


##### Panel

# Survey respondents over time
summary(panel_size_ts_trimmed$n_panelists)

### Bias (Mean forecast error)
#print(Bias_panel)

### Mincer-Zarnowitz regressions
#print(MZReg_panel)

### Errors-on-Revision regressions
print(ErrorOnRev_panel)

### Revisions-on-Revision regressions
print(RevOnRev_panel)

# summary Disagreement
disagreement_summary_pre_covid
disagreement_summary


##### Robustness (not reported becasue numerically virtually identical)

# Root Mean Squared Errors of median without and with fixed-horizon forecasts
RMSE_quarterly_median_fh$RMSE # spf_alt_rmse is with fixed-horizon

# Root Mean Squared Errors of median versus mean consensus
RMSE_quarterly_mean_cons$RMSE # spf_alt_rmse is mean consensus

# Root Mean Squared Errors of median over individual filtered forecasts (spf_alt_rmse)
RMSE_quarterly_panel_median_cons$RMSE

# Root Mean Squared Errors of current and next year versus current year forecasts
RMSE_quarterly_median_ny_cy  # h = 4 always needs next year forecasts



### RMSE and DM test of yearly SPF forecasts (additional results probably not reported)
# round(RMSE_yearly_median_cons$RMSE_yearly,decimals)
# RMSE_yearly_median_cons$DM_stars_yearly

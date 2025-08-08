### Kalman Main - to test Kalman filter and smoother


# This code is a mess and just to play around!!!!!!!!!!!!!!
library(here)
library(dplyr)      # For data manipulation: filtering, mutating, grouping, etc.
library(lubridate)  # For working with dates: constructing and extracting year, month, etc.
library(zoo)        # Provides 'yearqtr' and 'yearmon' classes
library(tidyr)      # For reshaping data
library(ggplot2)    # For plotting
library(sandwich)   # For Newey-West standard errors
library(lmtest)     # For coeftest()
library(tibble)     # For tibble-based output



rm(list = ls())
cat("\014")


# Read in filtered quarterly SPF forecasts
spf_data <- read.csv("data/filter_spf_data_medianfc.csv")

# Read in real-time GDP
rgdp_pre <- read.csv("data/revdatpre14.csv")
rgdp_post <- read.csv("data/revdatpost14.csv")

# Merge GDP
rgdp_pre <- rgdp_pre[ - which(rgdp_pre$origin_year == 2014 & rgdp_pre$origin_month > 9), ]
rgdp_pre$origin_day <- NA
rgdp_all <- rbind(rgdp_pre,rgdp_post)


### Filtered SPF h = 0, 1, ..., 4 step ahead forecasts

# Create 'target_date' and 'origin_date' as yearqtr objects
spf_data <- spf_data %>%
  mutate(
    target_date = as.yearqtr(paste(target_year, target_quarter), format = "%Y %q"),
    origin_date = as.yearqtr(paste(origin_year, origin_quarter), format = "%Y %q")
  )

# Read out forecast horizon h = difference in quarters
spf_data <- spf_data %>%
  mutate(
    h = as.integer(4 * (target_date - origin_date))  # 4 quarters per year
  ) %>%
  filter(h %in% 0:4)  # Keep only horizons of interest

# Replace Na with forecasts exploiting the two-year-ahead SPF
spf_data <- spf_data %>%
  mutate(
    spf_filter_cy = coalesce(spf_filter_cy, spf_filter_ny)
  )


#reshape forecasts like US SPF format
spf_forecasts_cy <- spf_data %>%
  select(target_year, target_quarter, h, spf_filter_cy) %>%
  pivot_wider(
    names_from = h,
    names_prefix = "spf_h",
    values_from = spf_filter_cy
  ) %>%
  arrange(target_year, target_quarter)

spf_forecasts_ny <- spf_data %>%
  select(target_year, target_quarter, h, spf_filter_ny) %>%
  pivot_wider(
    names_from = h,
    names_prefix = "spf_h",
    values_from = spf_filter_ny
  ) %>%
  arrange(target_year, target_quarter)


### Actuals of RGDP

# First or second release of RGDP as actuals
release <- 2

# Compute quarterly GDP growth rates
rgdp_all <- rgdp_all %>%
  arrange(origin_year, origin_month, target_year, target_quarter) %>%  # ensure correct order
  group_by(origin_year, origin_month) %>%
  mutate(
    gdp_growth = ( (rgdp / lag(rgdp) ) ^ 4 - 1) * 100,
  ) %>%
  ungroup()

# Keep second releases
rgdp <- rgdp_all %>%
  mutate(
    # Convert to yearqtr (e.g., 2018 Q2 → 2018.25)
    ref_period = as.yearqtr(paste(target_year, target_quarter), format = "%Y %q"),

    # Create origin_date from origin_year and origin_month
    origin_date = make_date(origin_year, origin_month, 1)
  )

rgdp <- rgdp %>%
  arrange(ref_period, origin_date) %>%
  group_by(ref_period) %>%
  slice(release) %>%
  ungroup()



# Merge SPF and actuals of RGDP
target_aux <- cbind(spf_forecasts_cy$target_quarter, spf_forecasts_cy$target_year)

spf_forecasts_cy <- spf_forecasts_cy %>%
  mutate(ref_period = as.yearqtr(paste(target_year, target_quarter), format = "%Y %q")) %>%
  select(-target_year, -target_quarter)

spf_forecasts_ny <- spf_forecasts_ny %>%
  mutate(ref_period = as.yearqtr(paste(target_year, target_quarter), format = "%Y %q")) %>%
  select(-target_year, -target_quarter)

evaluation_data_cy <- rgdp %>%
  left_join(spf_forecasts_cy, by = "ref_period") %>%
  filter(ref_period >= as.yearqtr("2002 Q1", format = "%Y Q%q"))

evaluation_data_ny <- rgdp %>%
  left_join(spf_forecasts_ny, by = "ref_period")

spf_forecasts_ny$target_year <- target_aux[,2]
spf_forecasts_ny$target_quarter <- target_aux[,1]
spf_forecasts_cy$target_year <- target_aux[,2]
spf_forecasts_cy$target_quarter <- target_aux[,1]


# Quick plots

# Merge CY and NY forecasts into one table with gdp_growth
plot_data <- evaluation_data_cy %>%
  filter(ref_period >= as.yearqtr("2000 Q1", format = "%Y Q%q"),
         ref_period <= as.yearqtr("2019 Q4", format = "%Y Q%q")) %>%
  select(ref_period, gdp_growth, spf_cy = spf_h0) %>%
  left_join(
    evaluation_data_ny %>% select(ref_period, spf_ny = spf_h0),
    by = "ref_period"
  )

# Reshape to long format for plotting
plot_data_long <- plot_data %>%
  pivot_longer(cols = c(gdp_growth, spf_cy, spf_ny),
               names_to = "series",
               values_to = "value")

# Plot
ggplot(plot_data_long, aes(x = ref_period, y = value, color = series)) +
  geom_line(size = 1) +
  labs(
    title = "GDP Growth vs SPF Forecasts (CY & NY)",
    x = "Reference Quarter",
    y = "Value",
    color = "Series"
  ) +
  theme_minimal()



### Fit AR(1) in spirit of direct forecasting

ref_qtrs <- seq(as.yearqtr("2004 Q1", format = "%Y Q%q"), # Has to be correctly chosen!
                as.yearqtr("2024 Q4", format = "%Y Q%q"), # Has to be correctly chosen!
                by = 0.25)

# Convert to data frame with formatted ref_period
forecasts_AR1 <- tibble(
  ref_period = format(ref_qtrs, "%Y Q%q"),
  AR1_0 = NA_real_,
  AR1_1 = NA_real_,
  AR1_2 = NA_real_,
  AR1_3 = NA_real_,
  AR1_4 = NA_real_
)

forecasts_SPF_cy <- tibble(
  ref_period = format(ref_qtrs, "%Y Q%q"),
  SPF_cy_0 = NA_real_,
  SPF_cy_1 = NA_real_,
  SPF_cy_2 = NA_real_,
  SPF_cy_3 = NA_real_,
  SPF_cy_4 = NA_real_
)

forecasts_SPF_ny <- tibble(
  ref_period = format(ref_qtrs, "%Y Q%q"),
  SPF_ny_0 = NA_real_,
  SPF_ny_1 = NA_real_,
  SPF_ny_2 = NA_real_,
  SPF_ny_3 = NA_real_,
  SPF_ny_4 = NA_real_
)


# Filter the relevant vintages
vintages <- rgdp_all %>%
  filter(origin_year >= 2004 & origin_year <= 2024, # Has to be correctly chosen!
         origin_month %in% c(2, 5, 8, 11)) %>%
  distinct(origin_year, origin_month) %>%
  arrange(origin_year, origin_month)


# Loop over each vintage
for (i in seq_len(nrow(vintages) - 4)) {
  this_year <- vintages$origin_year[i]
  this_month <- vintages$origin_month[i]
  this_quarter <- floor(this_month / 3) + 1

  # Read out vintage
  vintage_data <- rgdp_all %>% filter(origin_year == this_year, origin_month == this_month)
  vintage_data <- vintage_data[-1, ]

  # Merge SPF to vintages
  vintage_data <- vintage_data %>%
    mutate(merge_date = as.yearqtr(paste(target_year, target_quarter), format = "%Y %q") )

  spf_cy <- spf_forecasts_cy %>%
    mutate(merge_date = as.yearqtr(paste(target_year, target_quarter), format = "%Y %q") )
  spf_cy$spf_h1 <- lead(spf_cy$spf_h1)
  spf_cy$spf_h2 <- lead(spf_cy$spf_h2,2)
  spf_cy$spf_h3 <- lead(spf_cy$spf_h3,3)
  spf_cy$spf_h4 <- lead(spf_cy$spf_h4,4)

  spf_ny <- spf_forecasts_ny %>%
    mutate(merge_date = as.yearqtr(paste(target_year, target_quarter), format = "%Y %q") )
  spf_ny$spf_h1 <- lead(spf_ny$spf_h1)
  spf_ny$spf_h2 <- lead(spf_ny$spf_h2,2)
  spf_ny$spf_h3 <- lead(spf_ny$spf_h3,3)
  spf_ny$spf_h4 <- lead(spf_ny$spf_h4,4)

  vintage_data <- vintage_data %>%
    left_join(spf_cy, by = "merge_date")

  vintage_data <- vintage_data %>%
    left_join(spf_ny, by = "merge_date")

  # First observation of SPF
  drop_ind <- which(!is.na(vintage_data$spf_h0.x))[1] - 5
  vintage_data <- vintage_data[-(1:drop_ind), ]

  # Lag order
  T <- dim(vintage_data)[1]
  lag_quarters <- (this_year - vintage_data$target_year.x[T]) * 4 + (this_quarter - vintage_data$target_quarter.x[T])

  gdp_latest <- vintage_data$gdp_growth[T]

  # Direct forecasts AR(1)
  ar_coeff <- lm(gdp_growth ~ lag(gdp_growth, lag_quarters), data = vintage_data)
  forecasts_AR1$AR1_0[i] <- coefficients(ar_coeff) %*% rbind(1, gdp_latest)

  ar_coeff <- lm(gdp_growth ~ lag(gdp_growth, lag_quarters + 1), data = vintage_data)
  forecasts_AR1$AR1_1[i+1] <- coefficients(ar_coeff) %*% rbind(1, gdp_latest)

  ar_coeff <- lm(gdp_growth ~ lag(gdp_growth, lag_quarters + 2), data = vintage_data)
  forecasts_AR1$AR1_2[i+2] <- coefficients(ar_coeff) %*% rbind(1, gdp_latest)

  ar_coeff <- lm(gdp_growth ~ lag(gdp_growth, lag_quarters + 3), data = vintage_data)
  forecasts_AR1$AR1_3[i+3] <- coefficients(ar_coeff) %*% rbind(1, gdp_latest)

  ar_coeff <- lm(gdp_growth ~ lag(gdp_growth, lag_quarters + 4), data = vintage_data)
  forecasts_AR1$AR1_4[i+4] <- coefficients(ar_coeff) %*% rbind(1, gdp_latest)


  # Direct forecasts SPF (cy)
  ar_coeff <- lm(gdp_growth ~ lag(gdp_growth, lag_quarters) + spf_h0.x, data = vintage_data)
  forecasts_SPF_cy$SPF_cy_0[i] <- coefficients(ar_coeff) %*% rbind(1, gdp_latest, vintage_data$spf_h0.x[T])

  ar_coeff <- lm(gdp_growth ~ lag(gdp_growth, lag_quarters + 1) + lag(spf_h1.x,1), data = vintage_data)
  forecasts_SPF_cy$SPF_cy_1[i+1] <- coefficients(ar_coeff) %*% rbind(1, gdp_latest, vintage_data$spf_h1.x[T])

  ar_coeff <- lm(gdp_growth ~ lag(gdp_growth, lag_quarters + 2) + lag(spf_h2.x,2), data = vintage_data)
  forecasts_SPF_cy$SPF_cy_2[i+2] <- coefficients(ar_coeff) %*% rbind(1, gdp_latest, vintage_data$spf_h2.x[T])

  ar_coeff <- lm(gdp_growth ~ lag(gdp_growth, lag_quarters + 3) + lag(spf_h3.x,3), data = vintage_data)
  forecasts_SPF_cy$SPF_cy_3[i+3] <- coefficients(ar_coeff) %*% rbind(1, gdp_latest, vintage_data$spf_h3.x[T])

  ar_coeff <- lm(gdp_growth ~ lag(gdp_growth, lag_quarters + 4) + lag(spf_h4.x,4), data = vintage_data)
  forecasts_SPF_cy$SPF_cy_4[i+4] <- coefficients(ar_coeff) %*% rbind(1, gdp_latest, vintage_data$spf_h4.x[T])


  # Direct forecasts SPF (ny)
  ar_coeff <- lm(gdp_growth ~ lag(gdp_growth, lag_quarters) + spf_h0.y, data = vintage_data)
  forecasts_SPF_ny$SPF_ny_0[i] <- coefficients(ar_coeff) %*% rbind(1, gdp_latest, vintage_data$spf_h0.y[T])

  ar_coeff <- lm(gdp_growth ~ lag(gdp_growth, lag_quarters + 1) + lag(spf_h1.y,1), data = vintage_data)
  forecasts_SPF_ny$SPF_ny_1[i+1] <- coefficients(ar_coeff) %*% rbind(1, gdp_latest, vintage_data$spf_h1.y[T])

  ar_coeff <- lm(gdp_growth ~ lag(gdp_growth, lag_quarters + 2) + lag(spf_h2.y,2), data = vintage_data)
  forecasts_SPF_ny$SPF_ny_2[i+2] <- coefficients(ar_coeff) %*% rbind(1, gdp_latest, vintage_data$spf_h2.y[T])

  ar_coeff <- lm(gdp_growth ~ lag(gdp_growth, lag_quarters + 3) + lag(spf_h3.y,3), data = vintage_data)
  forecasts_SPF_ny$SPF_ny_3[i+3] <- coefficients(ar_coeff) %*% rbind(1, gdp_latest, vintage_data$spf_h3.y[T])

  ar_coeff <- lm(gdp_growth ~ lag(gdp_growth, lag_quarters + 4) + lag(spf_h4.y,4), data = vintage_data)
  forecasts_SPF_ny$SPF_ny_4[i+4] <- coefficients(ar_coeff) %*% rbind(1, gdp_latest, vintage_data$spf_h4.y[T])

}



# Merge AR1 forecasts
forecasts_AR1 <- forecasts_AR1 %>%
  mutate(ref_period = as.yearqtr(ref_period, format = "%Y Q%q"))

forecasts_SPF_cy <- forecasts_SPF_cy %>%
  mutate(ref_period = as.yearqtr(ref_period, format = "%Y Q%q"))

forecasts_SPF_ny <- forecasts_SPF_ny %>%
  mutate(ref_period = as.yearqtr(ref_period, format = "%Y Q%q"))


rgdp <- rgdp %>%
  left_join(forecasts_AR1, by = "ref_period")

rgdp <- rgdp %>%
  left_join(forecasts_SPF_cy, by = "ref_period")

rgdp <- rgdp %>%
  left_join(forecasts_SPF_ny, by = "ref_period")


# Merge filtered SPF
spf_forecasts_cy <- spf_forecasts_cy %>%
  mutate(ref_period = as.yearqtr(paste(target_year, target_quarter), format = "%Y %q")) %>%
  select(-target_year, -target_quarter)

spf_forecasts_ny <- spf_forecasts_ny %>%
  mutate(ref_period = as.yearqtr(paste(target_year, target_quarter), format = "%Y %q")) %>%
  select(-target_year, -target_quarter)

rgdp <- rgdp %>%
  left_join(
    spf_forecasts_cy %>%
      rename_with(~ paste0("filter_cy_", .), starts_with("spf_h")),
    by = "ref_period"
  )

rgdp <- rgdp %>%
  left_join(
    spf_forecasts_ny %>%
      rename_with(~ paste0("filter_ny_", .), starts_with("spf_h")),
    by = "ref_period"
  )





### Forecast evaluation
evaluation_data <- rgdp %>%
  filter(!(is.na(filter_cy_spf_h0) | is.na(filter_cy_spf_h4)))

evaluation_data <- evaluation_data %>%
  mutate(fc_error_cy_0 = gdp_growth - filter_cy_spf_h0,
         fc_error_cy_1 = gdp_growth - filter_cy_spf_h1,
         fc_error_cy_2 = gdp_growth - filter_cy_spf_h2,
         fc_error_cy_3 = gdp_growth - filter_cy_spf_h3,
         fc_error_cy_4 = gdp_growth - filter_cy_spf_h4,
         fc_error_ny_0 = gdp_growth - filter_ny_spf_h0,
         fc_error_ny_1 = gdp_growth - filter_ny_spf_h1,
         fc_error_ny_2 = gdp_growth - filter_ny_spf_h2,
         fc_error_ny_3 = gdp_growth - filter_ny_spf_h3,
         fc_error_ny_4 = gdp_growth - filter_ny_spf_h4)


# Exclude 2009 and 2010?
evaluation_data <- evaluation_data %>%
  filter(!(target_year %in% c(2009, 2010)))

evaluation_data <- evaluation_data %>%
    filter(target_year < 2019 & target_year > 2004)

# Unbiased (CY)
results <- lapply(0:4, function(h) {
  # Get formula as string and evaluate
  formula <- as.formula(paste0("fc_error_cy_", h, " ~ 1"))

  # Fit regression
  model <- lm(formula, data = evaluation_data)

  # Newey-West SE (you can choose lag = h or any rule-of-thumb)
  nw <- coeftest(model, vcov = NeweyWest(model, lag = h, prewhite = FALSE))

  # Return horizon, estimate, SE, and p-value
  tibble(
    horizon = h,
    intercept = nw[1, 1],
    std_error = nw[1, 2],
    p_value = nw[1, 4]
  )
})

# Combine all into one table
results_table <- bind_rows(results)
print(results_table)


# Unbiased (NY)
results <- lapply(0:4, function(h) {
  # Get formula as string and evaluate
  formula <- as.formula(paste0("fc_error_ny_", h, " ~ 1"))

  # Fit regression
  model <- lm(formula, data = evaluation_data)

  # Newey-West SE (you can choose lag = h or any rule-of-thumb)
  nw <- coeftest(model, vcov = NeweyWest(model, lag = h, prewhite = FALSE))

  # Return horizon, estimate, SE, and p-value
  tibble(
    horizon = h,
    intercept = nw[1, 1],
    std_error = nw[1, 2],
    p_value = nw[1, 4]
  )
})

# Combine all into one table
results_table <- bind_rows(results)
print(results_table)



# MSE

# Compute historical mean of GDP growth (the naive forecast)
gdp_mean <- mean(evaluation_data$gdp_growth, na.rm = TRUE)

# Loop over h = 1 to 4
error_stats <- lapply(0:4, function(h) {
  actual <- evaluation_data$gdp_growth
  spf_forecast_error_cy <- evaluation_data[[paste0("fc_error_cy_", h)]]
  spf_forecast_error_ny <- evaluation_data[[paste0("fc_error_ny_", h)]]

  # Compute SPF errors
  spf_cy_mse <- mean((spf_forecast_error_cy)^2, na.rm = TRUE)
  spf_cy_mae <- mean(abs(spf_forecast_error_cy), na.rm = TRUE)

  spf_ny_mse <- mean((spf_forecast_error_ny)^2, na.rm = TRUE)
  spf_ny_mae <- mean(abs(spf_forecast_error_ny), na.rm = TRUE)

  # Compute benchmark errors: actual - historical mean
  benchmark_error <- actual - gdp_mean
  hist_mean_mse <- mean((benchmark_error)^2, na.rm = TRUE)
  hist_mean_mae <- mean(abs(benchmark_error), na.rm = TRUE)

  # Compute benchmark errors of AR1:
  forecast_ar1 <- evaluation_data[[paste0("AR1_", h)]]
  benchmark_ar1_error <- actual - forecast_ar1
  ar1_mse <- mean((benchmark_ar1_error)^2, na.rm = TRUE)
  ar1_mae <- mean(abs(benchmark_ar1_error), na.rm = TRUE)

  # Compute errors of AR1-SPF_cy:
  forecast_ar1 <- evaluation_data[[paste0("SPF_cy_", h)]]
  benchmark_ar1_error <- actual - forecast_ar1
  ar_spf_cy_mse <- mean((benchmark_ar1_error)^2, na.rm = TRUE)
  ar_spf_cy_mae <- mean(abs(benchmark_ar1_error), na.rm = TRUE)

  # Compute errors of AR1-SPF_ny:
  forecast_ar1 <- evaluation_data[[paste0("SPF_ny_", h)]]
  benchmark_ar1_error <- actual - forecast_ar1
  ar_spf_ny_mse <- mean((benchmark_ar1_error)^2, na.rm = TRUE)
  ar_spf_ny_mae <- mean(abs(benchmark_ar1_error), na.rm = TRUE)

  tibble(
    horizon = h,
    spf_cy_mse = spf_cy_mse,
    #spf_cy_mae = spf_cy_mae,
    spf_ny_mse = spf_ny_mse,
    #spf_ny_mae = spf_ny_mae,
    hist_mean_mse = hist_mean_mse,
    #hist_mean_mae = hist_mean_mae,
    ar1_mse = ar1_mse,
    #ar1_mae = ar1_mae,
    ar_spf_cy_mse = ar_spf_cy_mse,
    #ar_spf_cy_mae = ar_spf_cy_mae,
    ar_spf_ny_mse = ar_spf_ny_mse,
    #ar_spf_ny_mae = ar_spf_ny_mae
  )
})

comparison_table <- bind_rows(error_stats)
print(comparison_table)








#### Trash - Code

source(here("scripts", "kalman_filter.R"))
source(here("scripts", "kalman_smoother.R"))

q <- rbind(-1.975905496,-0.5638928,2.660615959,2.566018083,2.244165169,2.060216621,
           4.861686218,3.396030031,1.409498959,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,
           NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN)
a <- rbind(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,2.338002864,NaN,NaN,NaN,1.65436809,
           NaN,NaN,NaN,2.055519891,NaN,NaN,NaN,1.915861112,NaN,NaN,NaN,1.912181401,NaN,NaN,
           NaN,1.885626914,NaN,NaN,NaN,1.833479425)

# Test the SPF-filter function
test <- SPF_filter(q,a)

y <- cbind(a,q)

# start will be estimated using built-in optimizer
matlab_est <- 1.33424292247901 # was estimated using Matlab

# Estimate the random walk error standard deviation
start <- 0.5
est_sd <- optim(par = start,
                fn = log_likelihood_function,
                y = y,
                approx_err = 0.01,
                method = "L-BFGS-B",
                lower = 0.0001,
                upper = Inf)

# Check difference to Matlab implementation
matlab_est - est_sd$par

# Given the estimate 'est_sd', we can filter and smooth states, i.e., implied SPF
filtered_states <- kalman_filter(y, rw_sd = est_sd$par, approx_err = 0.01, smooth = TRUE)
SPF <- as.matrix(kalman_smoother(filtered_states)[,1])



# Real-time filter conditional on latest vintage available

rm(list = ls())
cat("\014")


source(here("scripts", "kalman_filter.R"))
source(here("scripts", "kalman_smoother.R"))


# Set up hypothetical data set
rgdp <- rbind(1.1,0.7,0.2,-0.5,1.1,0.7,0.2,-0.5,
           -1.975905496,-0.5638928,2.660615959,2.566018083,2.244165169,2.060216621,
           4.861686218,3.396030031,1.409498959,1.343469,2.1361234,0.5125,-0.22352,1.121177,0.659696,1.7478568,1.12568865,2.0120120,
           1.23234,0.8124124,1.123452345,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN)



spf1 <- rbind(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,
              NaN,NaN,NaN,2.0119931,NaN,NaN,NaN,1.3453435,NaN,NaN,NaN,2.338002864,NaN,NaN,NaN,1.65436809,
             NaN,NaN,NaN,2.055519891,NaN,NaN,NaN,1.915861112,NaN,NaN,NaN,1.912181401,NaN,NaN,
             NaN,1.885626914,NaN,NaN,NaN,1.833479425)
spf2 <- rbind(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,
              NaN,NaN,NaN,2.0119931,NaN,NaN,NaN,1.3453435,NaN,NaN,NaN,2.338002864,NaN,NaN,NaN,1.65436809,
              NaN,NaN,NaN,2.055519891,NaN,NaN,NaN,1.915861112,NaN,NaN,NaN,1.912181401,NaN,NaN,
              NaN,1.885626914,NaN,NaN,NaN,1.833479425)*0.9
spf3 <- rbind(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,
              NaN,NaN,NaN,2.0119931,NaN,NaN,NaN,1.3453435,NaN,NaN,NaN,2.338002864,NaN,NaN,NaN,1.65436809,
              NaN,NaN,NaN,2.055519891,NaN,NaN,NaN,1.915861112,NaN,NaN,NaN,1.912181401,NaN,NaN,
              NaN,1.885626914,NaN,NaN,NaN,1.833479425)*1.1
spf4 <- rbind(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,
              NaN,NaN,NaN,2.0119931,NaN,NaN,NaN,1.3453435,NaN,NaN,NaN,2.338002864,NaN,NaN,NaN,1.65436809,
              NaN,NaN,NaN,2.055519891,NaN,NaN,NaN,1.915861112,NaN,NaN,NaN,1.912181401,NaN,NaN,
              NaN,1.885626914,NaN,NaN,NaN,1.833479425)*1.05



years <- 1998:2008
quarters <- paste0("Q", 1:4)
qdates <- unlist(lapply(years, function(y) paste0(y, ":", quarters)))



### Example being at 2005:Q3
# Input to function
fc_origin <- "2005:Q3"
data_all <- data.frame(qdates, rgdp, stringsAsFactors = FALSE)
colnames(data_all) <- c("date", "rgdp")
# data: Quarterly date, annualized quarterly GDP growth rates, SPF year over year
#       growth taken from the latest available survey. NOTE: The annual growth rates
#       of the SPF for a respective year need to be in YYYY:Q4.
data <- data_all[1:36, ]
SPF_all <- data.frame(qdates, cbind(spf1,spf2,spf3,spf4))
SPF_quarterly <- SPF_all[1:36, ]
colnames(SPF_quarterly) <- c("qdates","SPFQ1","SPFQ2","SPFQ3","SPFQ4")
SPF_quarterly[32:36,4:5] <- NaN  # Q3 not released yet, thus





# First survey round
SPF_first_survey <- SPF_quarterly[min(which(!is.nan(SPF_quarterly[ ,2]))) - 3, 1]

## Function (Improve speed by using matrices and vectors instead of indexing in data frames and year string variable?)
# Input: data, SPF_first_survey, fc_origin, info comes from real-time data function (not coded yet)
SPF_filter_realtime_option_1 = function(RGDP,SPF_first_survey,fc_origin) {

  data <- RGDP

  ##### Inside the function
  cols <- paste0("SPF_Q")
  data[cols] <- NaN
  cols <- paste0("SPF", 0:3, "_rt")
  data[cols] <- NaN

  Obs <- dim(SPF_quarterly)[1]

  # First survey of SPF
  begin_spf <- which(SPF_quarterly[ ,1] == SPF_first_survey)

  # Current quarter (forecast origin)
  TT <- which(data$date == fc_origin)

  # Which is the latest SPF survey available? (Rikes function?????)
  SPF_Q_ind <- 2 # min(which(is.nan(as.numeric(SPF_quarterly[Obs,2:5])))) - 1

  # Latest GDP observation
  nan_idx <- which(is.nan(data$rgdp))
  T_latest <- if (length(nan_idx) > 0) min(nan_idx) else NA
  T_latest <- T_latest

  # Current quarter
  Q_id <- as.numeric(sub(".*Q", "", data$date[TT]))

  for (t in TT:begin_spf) {

    T_latest <- T_latest - 1

    # h-step ahead SPF-forecasts
    if (Q_id == 3) {

      # Read out relevant SPF data in Q3, check with Rike's function!!!
      # Survey from SPF_Q_ind and year <- as.numeric(sub(":Q[1-4]", "", data$date[t]))
      SPF_Q <- c(1.9,0.8)

      # h = 0 and 1
      data_filter <- data[1:(t+5),1:3]
      data_filter$SPF_Q[c(t+1,t+5)] <- SPF_Q
      data_filter <- data_filter[1:(t+1),]
      SPF_rt <- SPF_filter(data_filter$rgdp,data_filter$SPF_Q)
      data_filter <- cbind(data_filter,SPF_rt)
      data$SPF0_rt[t] <- data_filter$SPF_implied[t]
      data$SPF1_rt[t] <- data_filter$SPF_implied[t+1]

      # h = 2 and 3
      data_filter <- data[1:(t+5),1:3]
      data_filter$SPF_Q[c(t+1,t+5)] <- SPF_Q
      data_filter <- data_filter[1:(t+5),] # ?
      SPF_rt <- SPF_filter(data_filter$rgdp,data_filter$SPF_Q)
      data_filter <- cbind(data_filter,SPF_rt)
      data$SPF2_rt[t] <- data_filter$SPF_implied[t+2]
      data$SPF3_rt[t] <- data_filter$SPF_implied[t+3]


    } else if (Q_id == 2) {

      # Read out relevant SPF data in Q2, check with Rike's function!!!
      # Survey from SPF_Q_ind and year <- as.numeric(sub(":Q[1-4]", "", data$date[t]))
      SPF_Q <- c(1.9,0.8)

      # h = 0, 1, and 2
      data_filter <- data[1:(t+6),1:3]
      data_filter$SPF_Q[c(t+2,t+6)] <- SPF_Q
      data_filter <- data_filter[1:(t+2),]
      SPF_rt <- SPF_filter(data_filter$rgdp,data_filter$SPF_Q)
      data_filter <- cbind(data_filter,SPF_rt)
      data$SPF0_rt[t] <- data_filter$SPF_implied[t]
      data$SPF1_rt[t] <- data_filter$SPF_implied[t+1]
      data$SPF2_rt[t] <- data_filter$SPF_implied[t+2]

      # h = 3
      data_filter <- data[1:(t+6),1:3]
      data_filter$SPF_Q[c(t+2,t+6)] <- SPF_Q
      data_filter <- data_filter[1:(t+6),] # ?
      SPF_rt <- SPF_filter(data_filter$rgdp,data_filter$SPF_Q)
      data_filter <- cbind(data_filter,SPF_rt)
      data$SPF3_rt[t] <- data_filter$SPF_implied[t+3]


    } else if (Q_id == 1) {

      # Read out relevant SPF data in Q1, check with Rike's function!!!
      # Survey from SPF_Q_ind and year <- as.numeric(sub(":Q[1-4]", "", data$date[t])) (- 1)  ?? IF Q_if == 4 ??
      SPF_Q <- c(1.1,1.85) # Think about this again!!!!!!!!!!!!!

      # h = 0, 1, 2, and 3
      data_filter <- data[1:(t+3),1:3]
      data_filter$SPF_Q[c(t-1,t+3)] <- SPF_Q
      data_filter <- data_filter[1:(t+3),] # ?
      SPF_rt <- SPF_filter(data_filter$rgdp,data_filter$SPF_Q)
      data_filter <- cbind(data_filter,SPF_rt)
      data$SPF0_rt[t] <- data_filter$SPF_implied[t]
      data$SPF1_rt[t] <- data_filter$SPF_implied[t+1]
      data$SPF2_rt[t] <- data_filter$SPF_implied[t+2]
      data$SPF3_rt[t] <- data_filter$SPF_implied[t+3]

    } else if (Q_id == 4) {

      # Read out relevant SPF data in Q3, check with Rike's function!!!
      # Survey from SPF_Q_ind and year <- as.numeric(sub(":Q[1-4]", "", data$date[t]))
      SPF_Q <- c(1.1,1.85)

      # h = 0
      data_filter <- data[1:(t+4),1:3]
      data_filter$SPF_Q[c(t,t+4)] <- SPF_Q
      data_filter <- data_filter[1:t,] # ?
      SPF_rt <- SPF_filter(data_filter$rgdp,data_filter$SPF_Q)
      data_filter <- cbind(data_filter,SPF_rt)
      data$SPF0_rt[t] <- data_filter$SPF_implied[t]

      # h = 1, 2, and 3
      data_filter <- data[1:(t+4),1:3]
      data_filter$SPF_Q[c(t,t+4)] <- SPF_Q
      data_filter <- data_filter[1:(t+4),] # ?
      SPF_rt <- SPF_filter(data_filter$rgdp,data_filter$SPF_Q)
      data_filter <- cbind(data_filter,SPF_rt)
      data$SPF1_rt[t] <- data_filter$SPF_implied[t+1]
      data$SPF2_rt[t] <- data_filter$SPF_implied[t+2]
      data$SPF3_rt[t] <- data_filter$SPF_implied[t+3]

    }

    # Recursively drop latest GDP growth
    data$rgdp[T_latest] <- NaN

    # Update quarter from which in-sample SPF is taken
    if (SPF_Q_ind > 1) {
      SPF_Q_ind <- SPF_Q_ind - 1
    } else {
      SPF_Q_ind <- 4
    }

    # Update quarter of forecast origin
    if (Q_id > 1) {
      Q_id <- Q_id - 1
    } else {
      Q_id <- 4
    }


  }

  data <- data[, c(1,4:7)]
  return(data)

}


test1 <- SPF_filter_realtime_option_1(data,SPF_first_survey,fc_origin)












gr <- test$SPF_implied
quarterly_growth <- (1 + gr / 100)^0.25
cumprod_test = cumprod(quarterly_growth)

gr1 = mean(cumprod_test[17:20])
gr2 = mean(cumprod_test[21:24])

gr2/gr1*100 - 100

T_all <- dim(data)[1]

# Find latest RGDP
nan_idx <- which(is.nan(data$rgdp))
first_nan_pos <- if (length(nan_idx) > 0) min(nan_idx) else NA

# Lag order with respect to the SPF
lag <- TT - first_nan_pos + 1


# Loop
t <- TT
#t <- t - 1

# Filter SPF
Q_id <- as.numeric(sub(".*Q", "", data$date[t]))
data_aux <- data[1:T_all,c(1,2,2+Q_id)]
#data_aux$rgdp[2:8] <- NaN
filter_data <- cbind(data_aux$rgdp,data_aux$spf_q3)
filter_data[1:20,2] <- NaN
filter_data <- filter_data[21:28,]
spf_test <- SPF_filter(filter_data[,1],filter_data[,2])
data_aux <- cbind(data_aux,spf_test)

# Read out h-step ahead forecasts
data$SPF0_rt[t] <- data_aux$SPF_implied[lag+1]
data$SPF1_rt[t] <- data_aux$SPF_implied[lag+2]
data$SPF2_rt[t] <- data_aux$SPF_implied[lag+3]
data$SPF3_rt[t] <- data_aux$SPF_implied[lag+4]

# Update index
T_all <- T_all - 1












































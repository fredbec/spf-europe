




AR_benchmark = function(rgdp_all) {

  # Input to function
  rw_length <- 8
  ar_length <- 30
  SampleEnd <- 2026
  max_lag   <- 4

  # Specify evaluation sample
  ref_qtrs <- seq(as.yearqtr("2001 Q1", format = "%Y Q%q"), # Has to be correctly chosen!
                  as.yearqtr(SampleEnd + 0.75, format = "%Y Q%q"),
                  by = 0.25)

  # Convert to data frame with formatted ref_period
  fc_AR1 <- tibble(
    ref_period = as.yearqtr(ref_qtrs),
    AR1_0 = NA_real_,
    AR1_1 = NA_real_,
    AR1_2 = NA_real_,
    AR1_3 = NA_real_,
    AR1_4 = NA_real_
  )

  fc_RWmean <- tibble(
    ref_period = as.yearqtr(ref_qtrs),
    RWmean_0 = NA_real_,
    RWmean_1 = NA_real_,
    RWmean_2 = NA_real_,
    RWmean_3 = NA_real_,
    RWmean_4 = NA_real_,
  )


  # Filter the relevant vintages
  vintages <- rgdp_all %>%
    filter(origin_year >= 2001 & origin_year <= SampleEnd, # Has to be correctly chosen!
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

    # Lag order
    T <- dim(vintage_data)[1]
    lag_quarters <- (this_year - vintage_data$target_year[T]) * 4 + (this_quarter - vintage_data$target_quarter[T])

    # Real-time GDP (latest vintage) and lags for direct forecasting
    gdp_rt <- .make_lagged_dataset(vintage_data, lag_quarters, max_lag)
    T_max <- dim(gdp_rt)[1]


    # Latest observation(s) to forecast GDP_(t+h)
    max_lag <- min(max_lag, T_max)
    gdp_latest <- vintage_data$gdp_growth[(T-max_lag+1):T]

    # Adjust estimation sample size
    rw_length <- min(rw_length, T_max)
    ar_length <- min(ar_length + max_lag, T_max)
    gdp_rt <- gdp_rt[(T_max-ar_length+1):T_max, ]


    # Chose optimal lag length according to BIC for IAR and DAR

    # function
    #BIC_select = function(gdp_rt,max_lag) {

      # Input to function
    #  gdp = gdp_rt
    #  maxLag = max_lag

      ## Indirect Autoregressive Model
      # Lag length for nowcast h+0
    #  lm(gdp ~ gdp_dlag0, data = gdp)
    #}




    # Direct forecasts AR(1)
    ar_coeff <- lm(gdp ~ gdp_dlag0, data = gdp_rt)
    fc_AR1$AR1_0[i] <- coefficients(ar_coeff) %*% rbind(1, gdp_latest)

    ar_coeff <- lm(gdp ~ gdp_dlag1, data = gdp_rt)
    fc_AR1$AR1_1[i+1] <- coefficients(ar_coeff) %*% rbind(1, gdp_latest)

    ar_coeff <- lm(gdp ~ gdp_dlag2, data = gdp_rt)
    fc_AR1$AR1_2[i+2] <- coefficients(ar_coeff) %*% rbind(1, gdp_latest)

    ar_coeff <- lm(gdp ~ gdp_dlag3, data = gdp_rt)
    fc_AR1$AR1_3[i+3] <- coefficients(ar_coeff) %*% rbind(1, gdp_latest)

    ar_coeff <- lm(gdp ~ gdp_dlag4, data = gdp_rt)
    fc_AR1$AR1_4[i+4] <- coefficients(ar_coeff) %*% rbind(1, gdp_latest)

    # Rolling window mean
    fc_RWmean$RWmean_0[i] <- mean(gdp_rt$gdp[(T_max-rw_length+1):T_max])
    fc_RWmean$RWmean_1[i+1] <- fc_RWmean$RWmean_0[i]
    fc_RWmean$RWmean_2[i+2] <- fc_RWmean$RWmean_0[i]
    fc_RWmean$RWmean_3[i+3] <- fc_RWmean$RWmean_0[i]
    fc_RWmean$RWmean_4[i+4] <- fc_RWmean$RWmean_0[i]

  }

  Output <- list(
    AR_fc     = fc_AR1,
    RWmean_fc = fc_RWmean
  )
  return(Output)

}









.make_lagged_dataset <- function(vintage_data, lag_quarters, max_lag) {

  # base variable
  df <- vintage_data %>%
    mutate(gdp = gdp_growth)

  # Direct lags: from h = 0 up to h = 4, plus up to max_lag extra for horizon h=4
  direct_lags <- map_dfc(
    set_names(0:(4 + max_lag-1), paste0("gdp_dlag", 0:(4 + max_lag-1))),
    ~ lag(df$gdp_growth, lag_quarters + .x)
  )

  # IAR lags: from 1 to max_lag
  iar_lags <- map_dfc(
    set_names(1:max_lag, paste0("gdp_lag", 1:max_lag)),
    ~ lag(df$gdp_growth, .x)
  )

  # Combine everything
  out <- bind_cols(df, direct_lags, iar_lags) %>%
    filter(!is.na(.data[[paste0("gdp_dlag", 4 + max_lag-1)]])) # trim sample

  return(out)
}

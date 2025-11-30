#' AR Benchmark Forecasts for Yearly Real-Time GDP
#'
#' Computes real-time forecasts of yearly GDP using:
#'   - Direct AR (DAR) models with horizon-specific lag selection,
#'   - Indirect AR (IAR) models (iterated AR forecasts),
#'   - Rolling-window mean (RWmean) benchmarks.
#'
#' Forecasts are computed for current and next calendar year and stored in separate tibbles.
#'
#' @param rgdp A data.frame or tibble containing real-time GDP vintages with at least:
#'   \itemize{
#'     \item{origin_year, origin_month}{vintage date of release}
#'     \item{target_year, target_quarter}{observation period}
#'     \item{gdp_growth}{GDP growth series}
#'   }
#' @param ar_length Integer. Length of the rolling window for AR estimation.
#' @param rw_length Integer. Length of the rolling window for the rolling mean benchmark.
#' @param max_lag Integer. Maximum lag length considered for AR models.
#' @param SampleEnd Numeric or integer. Last year of the evaluation sample.
#' @param endMonth Integer. End of month within the quarter
#'
#' @return A list with three elements:
#'   \item{DAR_fc}{Tibble of direct AR forecasts for current and next year}
#'   \item{IAR_fc}{Tibble of iterated AR forecasts for current and next year}
#'   \item{RWmean_fc}{Tibble of rolling-window mean forecasts for current and next year}
#'   \item{NoChange_fc}{Tibble of NoChange forecasts for current and next year}
#'
#' @examples
#' \dontrun{
#' # Suppose rgdp_data is a tibble with the required columns
#' forecasts <- AR_benchmark(
#'   rgdp = rgdp_data,
#'   ar_length = 30,
#'   rw_length = 10,
#'   max_lag = 1,
#'   SampleEnd = 2019
#' )
#' head(forecasts$DAR_fc)
#' head(forecasts$IAR_fc)
#' head(forecasts$RWmean_fc)
#' head(forecasts$NoChange_fc)
#' }
#' #'
#' @export
AR_benchmark_yearly = function(rgdp, ar_length, rw_length, max_lag, SampleEnd, endMonth = 2) {

  # Specify evaluation sample
  ref_qtrs <- seq(as.yearqtr("2001 Q1", format = "%Y Q%q"), # Has to be correctly chosen!
                  as.yearqtr(SampleEnd + 0.75, format = "%Y Q%q"),
                  by = 0.25)

  # Maximum of three months per quarter
  if (endMonth > 3) {
    endMonth = 3
    cat('Warning: Maximum of three months per quarter. Thus, endMonth = 3 chosen.')
  }

  # Set up matrices to store real-time forecasts
  fc_DAR <- tibble(
    ref_period = as.yearqtr(ref_qtrs),
    DAR_fc1 = NA_real_,
    DAR_fc2 = NA_real_
  )

  fc_IAR <- tibble(
    ref_period = as.yearqtr(ref_qtrs),
    IAR_fc1 = NA_real_,
    IAR_fc2 = NA_real_
  )

  fc_RWmean <- tibble(
    ref_period = as.yearqtr(ref_qtrs),
    RWmean_fc1 = NA_real_
  )

  fc_NoChange <- tibble(
    ref_period = as.yearqtr(ref_qtrs),
    NoChange_fc1 = NA_real_
  )

  # Filter the relevant vintages
  vintages <- rgdp %>%
    filter(origin_year >= 2001 & origin_year <= SampleEnd,    # starts in 2001
           origin_month %in% (c(0, 3, 6, 9) + endMonth) ) %>% # c(0, 3, 6, 9) + 2 is end of middle month of a quarter
    distinct(origin_year, origin_month) %>%
    arrange(origin_year, origin_month)


  # Loop over each vintage
  for (i in seq_len(nrow(vintages) - 4)) {
    this_year <- vintages$origin_year[i]
    this_month <- vintages$origin_month[i]
    this_quarter <- floor(this_month / 3) + 1

    # Read out vintage
    vintage_data <- rgdp %>% filter(origin_year == this_year, origin_month == this_month)

    # Compute annual real-time GDP growth rates
    vintage_data <- vintage_data %>%
      arrange(origin_year, origin_month, target_year, target_quarter) %>%  # ensure correct order
      group_by(origin_year, origin_month) %>%
      mutate(
        sum4 = zoo::rollsumr(rgdp, 4, fill = NA),        # sum of t, t-1, t-2, t-3
        sum4_prev = dplyr::lag(sum4, 4),                 # sum of t-4, t-5, t-6, t-7
        gdp_yearly = (sum4 / sum4_prev - 1) * 100        # percent change between the two 4-quarter sums
      ) %>%
      filter(target_quarter == 4) %>%
      filter(!is.na(gdp_yearly))

    # Lag order
    T <- dim(vintage_data)[1]
    lag_years <- this_year - vintage_data$target_year[T]

    # Real-time GDP (latest vintage) and lags for direct forecasting
    max_lag <- max(1, min(max_lag, T-lag_years) )

    # Latest observation(s) to forecast GDP_(t+h)
    gdp_latest <- rev(vintage_data$gdp_yearly[(T-max_lag+1):T])

    # Adjust estimation sample size
    rw_length <- min(rw_length, T)
    ar_length <- min(ar_length + max_lag, T)
    gdp_rt <- as.matrix(vintage_data$gdp_yearly)
    T_max <- dim(gdp_rt)[1]


    ### Direct forecasts DAR(p)

    # Generate lags for current and next year forecasts
    for (j in 0:(max_lag)) {
      lag_k <- lag_years + j
      vintage_data[[paste0("lag_", lag_k)]] <- dplyr::lag(vintage_data$gdp_yearly, n = lag_k)
    }

    # Current year forecasts
    predictors_fc1 <- paste0("lag_", lag_years:(lag_years + max_lag - 1))
    formula_fc1 <- as.formula(paste("gdp_yearly ~", paste(predictors_fc1, collapse = " + ")))

    # Next year forecasts
    predictors_fc2 <- paste0("lag_", (lag_years+1):(lag_years + max_lag))
    formula_fc2 <- as.formula(paste("gdp_yearly ~", paste(predictors_fc2, collapse = " + ")))

    # Run regressions
    dar_fc1 <- lm(formula_fc1, data = vintage_data)
    dar_fc2 <- lm(formula_fc2, data = vintage_data)

    # Current and next year forecasts
    fc_DAR$DAR_fc1[i] <- coefficients(dar_fc1) %*% t(cbind(1, t(gdp_latest)))
    fc_DAR$DAR_fc2[i] <- coefficients(dar_fc2) %*% t(cbind(1, t(gdp_latest)))


    ### Indirect forecasts IAR(p)

    # Generate lags for indirect current and next year forecasts
    for (j in 1:(max_lag)) {
      vintage_data[[paste0("lag_iar", j)]] <- dplyr::lag(vintage_data$gdp_yearly, n = j)
    }
    predictors_iar <- paste0("lag_iar", 1:max_lag)
    formula_iar <- as.formula(paste("gdp_yearly ~", paste(predictors_iar, collapse = " + ")))

    # Estimate OLS
    ar_coeff <- lm(formula_iar, data = vintage_data)

    # Initialize latest observed values for IAR
    gdp_latest_iar <- gdp_latest[1:max_lag]

    # Pre-iterate lag_quarters - 1 times
    if (lag_years > 1) {
      for (pre_h in 1:(lag_years - 1)) {
        y_hat_pre <- sum(coefficients(ar_coeff) * c(1, gdp_latest_iar))
        gdp_latest_iar <- c(y_hat_pre, head(gdp_latest_iar, max_lag - 1))
      }
    }

    # Forecast for current and next year
    for (h in 1:2) {

      # Prepare vector of predictors
      x_vec <- c(1, gdp_latest_iar)

      # Compute forecast
      y_hat <- sum(coefficients(ar_coeff) * x_vec)

      # Store forecast
      col_name <- paste0("IAR_fc", h)
      fc_IAR[[col_name]][i] <- y_hat

      # Update gdp_latest to include this forecast for next iteration
      gdp_latest_iar <- c(y_hat, head(gdp_latest_iar, max_lag - 1))
    }

    ### Rolling window mean and NoChange forecast
    fc_RWmean$RWmean_fc1[i]     <- mean(gdp_rt[(T_max-rw_length+1):T_max])
    fc_NoChange$NoChange_fc1[i] <- gdp_rt[T_max]

  }

  # Lag next year forecasts by four quarters
  fc_DAR$DAR_fc2           <- lag(fc_DAR$DAR_fc2,4)
  fc_IAR$IAR_fc2           <- lag(fc_IAR$IAR_fc2,4)
  fc_RWmean$RWmean_fc2     <- lag(fc_RWmean$RWmean_fc1,4)
  fc_NoChange$NoChange_fc2 <- lag(fc_NoChange$NoChange_fc1,4)

  # Define output of this function
  Output <- list(
    DAR_fc      = fc_DAR,
    IAR_fc      = fc_IAR,
    RWmean_fc   = fc_RWmean,
    NoChange_fc = fc_NoChange
  )
  return(Output)

}






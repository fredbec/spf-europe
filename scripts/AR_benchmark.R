#' AR Benchmark Forecasts for Real-Time GDP
#'
#' Computes real-time forecasts of GDP using:
#'   - Direct AR (DAR) models with horizon-specific lag selection,
#'   - Indirect AR (IAR) models (iterated AR forecasts),
#'   - Rolling-window mean (RWmean) benchmarks.
#'
#' Forecasts are computed for horizons h = 0,...,4 and stored in separate tibbles.
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
#'   \item{DAR_fc}{Tibble of direct AR forecasts for h = 0,...,4}
#'   \item{IAR_fc}{Tibble of iterated AR forecasts for h = 0,...,4}
#'   \item{RWmean_fc}{Tibble of rolling-window mean forecasts for h = 0,...,4}
#'
#' @examples
#' \dontrun{
#' # Suppose rgdp_data is a tibble with the required columns
#' forecasts <- AR_benchmark(
#'   rgdp = rgdp_data,
#'   ar_length = 30,
#'   rw_length = 10,
#'   max_lag = 4,
#'   SampleEnd = 2019
#' )
#' head(forecasts$DAR_fc)
#' head(forecasts$IAR_fc)
#' head(forecasts$RWmean_fc)
#' }
#' #'
#' @export
AR_benchmark = function(rgdp, ar_length, rw_length, max_lag, SampleEnd, endMonth = 2) {

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
    DAR_h0 = NA_real_,
    DAR_h1 = NA_real_,
    DAR_h2 = NA_real_,
    DAR_h3 = NA_real_,
    DAR_h4 = NA_real_
  )

  fc_IAR <- tibble(
    ref_period = as.yearqtr(ref_qtrs),
    IAR_h0 = NA_real_,
    IAR_h1 = NA_real_,
    IAR_h2 = NA_real_,
    IAR_h3 = NA_real_,
    IAR_h4 = NA_real_
  )

  fc_RWmean <- tibble(
    ref_period = as.yearqtr(ref_qtrs),
    RWmean_h0 = NA_real_,
    RWmean_h1 = NA_real_,
    RWmean_h2 = NA_real_,
    RWmean_h3 = NA_real_,
    RWmean_h4 = NA_real_,
  )

  fc_NoChange <- tibble(
    ref_period = as.yearqtr(ref_qtrs),
    NoChange_h0 = NA_real_,
    NoChange_h1 = NA_real_,
    NoChange_h2 = NA_real_,
    NoChange_h3 = NA_real_,
    NoChange_h4 = NA_real_,
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
    vintage_data <- vintage_data[-1, ]

    # Merge SPF to vintages
    vintage_data <- vintage_data %>%
      mutate(merge_date = as.yearqtr(paste(target_year, target_quarter), format = "%Y %q") )

    # Lag order
    T <- dim(vintage_data)[1]
    lag_quarters <- (this_year - vintage_data$target_year[T]) * 4 + (this_quarter - vintage_data$target_quarter[T])

    # Real-time GDP (latest vintage) and lags for direct forecasting
    max_lag <- max(1, min(max_lag, T - 20) )
    gdp_rt <- .make_lagged_dataset(vintage_data, lag_quarters, max_lag)
    T_max <- dim(gdp_rt)[1]

    # Latest observation(s) to forecast GDP_(t+h)
    max_lag <- min(max_lag, T_max)
    gdp_latest <- rev(vintage_data$gdp_growth[(T-max_lag+1):T])

    # Adjust estimation sample size
    rw_length <- min(rw_length, T_max)
    ar_length <- min(ar_length + max_lag, T_max)
    gdp_rt <- gdp_rt[(T_max-ar_length+1):T_max, ]
    T_max <- dim(gdp_rt)[1]

    ### Direct forecasts DAR(p)

    # Chose optimal lag length according to BIC for IAR and DAR
    lagLenghts <- .BIC_select(gdp_rt,4)
    lags <- lagLenghts$BIC_direct

    for (h in 0:4) {
      # Set up DAR model
      m <- lags[h+1]
      predictors <- paste0("gdp_dlag", h:(h + (m-1) ))
      formula <- as.formula(paste("gdp ~", paste(predictors, collapse = " + ")))

      # Estimate OLS
      ar_coeff <- lm(formula, data = gdp_rt)

      # Forecast h steps ahead
      col_name <- paste0("DAR_h", h)
      fc_DAR[[col_name]][i+h] <- coefficients(ar_coeff) %*% t(cbind(1, t(gdp_latest[1:m])))

    }


    ### Indirect forecasts IAR(p)

    # Set up IAR model
    lags <- lagLenghts$BIC_indirect
    predictors <- paste0("gdp_lag", 1:lags )
    formula <- as.formula(paste("gdp ~", paste(predictors, collapse = " + ")))

    # Estimate OLS
    ar_coeff <- lm(formula, data = gdp_rt)

    # Initialize latest observed values for IAR
    gdp_latest_iar <- gdp_latest[1:lags]

    # Pre-iterate lag_quarters - 1 times
    if (lag_quarters > 1) {
      for (pre_h in 1:(lag_quarters - 1)) {
        y_hat_pre <- sum(coefficients(ar_coeff) * c(1, gdp_latest_iar))
        gdp_latest_iar <- c(y_hat_pre, head(gdp_latest_iar, lags - 1))
      }
    }

    # Forecast h steps ahead
    for (h in 0:4) {

      # Prepare vector of predictors
      x_vec <- c(1, gdp_latest_iar)

      # Compute forecast
      y_hat <- sum(coefficients(ar_coeff) * x_vec)

      # Store forecast
      col_name <- paste0("IAR_h", h)
      fc_IAR[[col_name]][i+h] <- y_hat

      # Update gdp_latest to include this forecast for next iteration
      gdp_latest_iar <- c(y_hat, head(gdp_latest_iar, lags - 1))
    }


    ### Rolling window mean and NoChange forecast
    fc_RWmean$RWmean_h0[i]   <- mean(gdp_rt$gdp[(T_max-rw_length+1):T_max])
    fc_RWmean$RWmean_h1[i+1] <- fc_RWmean$RWmean_h0[i]
    fc_RWmean$RWmean_h2[i+2] <- fc_RWmean$RWmean_h0[i]
    fc_RWmean$RWmean_h3[i+3] <- fc_RWmean$RWmean_h0[i]
    fc_RWmean$RWmean_h4[i+4] <- fc_RWmean$RWmean_h0[i]

    fc_NoChange$NoChange_h0[i]   <- mean(gdp_rt$gdp[T_max])
    fc_NoChange$NoChange_h1[i+1] <- fc_NoChange$NoChange_h0[i]
    fc_NoChange$NoChange_h2[i+2] <- fc_NoChange$NoChange_h0[i]
    fc_NoChange$NoChange_h3[i+3] <- fc_NoChange$NoChange_h0[i]
    fc_NoChange$NoChange_h4[i+4] <- fc_NoChange$NoChange_h0[i]

  }

  # Define output of this function
  Output <- list(
    DAR_fc     = fc_DAR,
    IAR_fc     = fc_IAR,
    RWmean_fc = fc_RWmean,
    NoChange_fc = fc_NoChange
  )
  return(Output)

}



#' Create lagged dataset for direct and indirect forecasting
#'
#' Constructs a data frame with all necessary lagged variables for
#' direct (DAR) and indirect (IAR) forecasting. Direct lags are created
#' for horizons h = 0,...,4 plus additional lags up to `max_lag` for the last horizon.
#' Indirect lags are created from 1 to `max_lag` for iterative AR forecasting.
#'
#' @param vintage_data A data.frame or tibble containing at least `gdp_growth`.
#' @param lag_quarters Number of quarters to shift for the first direct lag.
#' @param max_lag Maximum lag length to construct.
#' @return A data.frame with:
#'   \item{gdp}{Original series from `gdp_growth`}
#'   \item{gdp_dlag0,...,gdp_dlagN}{Lagged variables for DAR}
#'   \item{gdp_lag1,...,gdp_lagM}{Lagged variables for IAR}

.make_lagged_dataset <- function(vintage_data, lag_quarters, max_lag) {

  # Base variable
  df <- vintage_data %>%
    mutate(gdp = gdp_growth)

  # Direct lags: from h = 0 up to h = 4, plus up to max_lag extra for horizon h = 4
  direct_lags <- purrr::map_dfc(
    purrr::set_names(0:(4 + max_lag-1), paste0("gdp_dlag", 0:(4 + max_lag-1))),
    ~ lag(df$gdp_growth, lag_quarters + .x)
  )

  # IAR lags: from 1 to max_lag
  iar_lags <- purrr::map_dfc(
    purrr::set_names(1:max_lag, paste0("gdp_lag", 1:max_lag)),
    ~ lag(df$gdp_growth, .x)
  )

  # Combine everything
  out <- bind_cols(df, direct_lags, iar_lags) %>%
    filter(!is.na(.data[[paste0("gdp_dlag", 4 + max_lag-1)]])) # trim sample

  return(out)
}



#' Compute optimal lag lengths for DAR and IAR models
#'
#' Computes BIC and AIC to select the best lag length for:
#'  - Direct AR models (DAR) across horizons h = 0,...,4
#'  - Indirect AR models (IAR)
#'
#' @param gdp A data.frame containing `gdp` and lagged columns `gdp_dlag0`, `gdp_dlag1`, ...
#' @param maxLag Maximum number of lags to consider
#' @return A list with BIC and AIC optimal lags for direct and indirect models

.BIC_select = function(gdp,maxLag) {

  # Sample size
  T <- dim(gdp)[1]

  ### Direct forecasting (DAR)
  BIC_dir <- matrix(NA_real_, nrow = 5, ncol = maxLag,
                    dimnames = list(paste0("h", 0:4), paste0("lag", 1:maxLag)))
  AIC_dir <- matrix(NA_real_, nrow = 5, ncol = maxLag,
                    dimnames = list(paste0("h", 0:4), paste0("lag", 1:maxLag)))

  # Loop over h = 0, 1,..., 4
  for (kk in 0:4) {

    # Loop over lag lengths
    for (k in 0:(maxLag-1)) {
      predictors <- paste0("gdp_dlag", kk:(kk + k))
      formula <- as.formula(paste("gdp ~", paste(predictors, collapse = " + ")))

      OLS_res <- lm(formula, data = gdp)
      residuals <- OLS_res$residuals
      SSR <- sum(residuals^2)

      # Compute BIC and AIC for h = kk and laglength = k+1
      BIC_dir[kk+1, k+1] <- log(SSR/T) + (k+1)  * log(T) / T
      AIC_dir[kk+1, k+1] <- log(SSR/T) + (k+1) * 2/T
    }
  }

  # best by BIC and AIC
  best_BIC_dir <- apply(BIC_dir, 1, which.min)
  best_AIC_dir <- apply(AIC_dir, 1, which.min)


  ### Indirect forecasting (IAR)
  BIC_ind <- matrix(1,maxLag)
  AIC_ind <- matrix(1,maxLag)

  # Loop over lag lengths
  for (k in 1:maxLag) {
    predictors <- paste0("gdp_lag", 1:k)
    formula <- as.formula(paste("gdp ~", paste(predictors, collapse = " + ")))

    OLS_res <- lm(formula, data = gdp)
    residuals <- OLS_res$residuals
    SSR <- sum(residuals^2)

    # Compute BIC and AIC for laglength = k
    BIC_ind[k] <- log(SSR/T) + k  * log(T) / T
    AIC_ind[k] <- log(SSR/T) + k * 2/T
  }

  # best by BIC and AIC
  best_BIC_ind <- which.min(BIC_ind)
  best_AIC_ind <- which.min(AIC_ind)


  Output <- list(
    BIC_direct = best_BIC_dir,
    AIC_direct = best_AIC_dir,
    BIC_indirect = best_BIC_ind,
    AIC_indirect = best_AIC_ind
  )
  return(Output)

}




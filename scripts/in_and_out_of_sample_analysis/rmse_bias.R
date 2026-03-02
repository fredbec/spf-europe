library(sandwich)   # For Newey-West standard errors
library(lmtest)     # For coeftest()


#' Consensus SPF Forecast Bias Test
#'
#' Computes bias in SPF GDP forecasts across horizons (h = 0,...,4)
#' using OLS with Newey–West standard errors.
#'
#' @param spf_data Data frame with SPF forecasts and realized GDP growth (from `data_function_spf.R`).
#' @param EvalPeriod Numeric 2×1 matrix giving start and end years for evaluation (default: 2002–2019).
#' @param DropPeriod Optional matrix of periods (start–end years) to exclude from evaluation.
#'
#' @return Tibble with intercept estimates, standard errors, and p-values for each forecast horizon.
#' @export
SPF_bias <- function(spf_data, EvalPeriod = cbind(2002, 2019), DropPeriod = NA) {

  # Drop missings
  evaluation_data <- spf_data %>%
    filter(!(is.na(spf_h0) | is.na(spf_h4)))

  # Adjust sample start and end points
  evaluation_data <- evaluation_data %>%
    filter(target_year < (EvalPeriod[2]+1) & target_year > (EvalPeriod[1]-1))

  # Drop periods if specified
  if (any(!is.na(DropPeriod))) {
    for (i in 1:dim(DropPeriod)[1]) {
      evaluation_data <- evaluation_data %>%
        filter(!(target_year %in% c(DropPeriod[i,1]:DropPeriod[i,2]) ))
    }
  }

  # Forecast errors
  evaluation_data <- evaluation_data %>%
    mutate(spf_fc_error_0 = gdp_growth - spf_h0,
           spf_fc_error_1 = gdp_growth - spf_h1,
           spf_fc_error_2 = gdp_growth - spf_h2,
           spf_fc_error_3 = gdp_growth - spf_h3,
           spf_fc_error_4 = gdp_growth - spf_h4)

  ### Unbiased
  results <- lapply(0:4, function(h) {
    # Get formula as string and evaluate
    formula <- as.formula(paste0("spf_fc_error_", h, " ~ 1"))

    # Fit regression
    model <- lm(formula, data = evaluation_data)

    # Newey-West SE
    lag_ruleOfThumb <- max(4, as.integer( dim(evaluation_data)[1]^0.25 ) ) # floor(4*(dim(evaluation_data)[1]/100)^(2/9))
    nw <- coeftest(model, vcov = NeweyWest(model, lag = lag_ruleOfThumb, prewhite = FALSE))

    # Return horizon, estimate, SE, and p-value
    tibble(
      horizon = h,
      intercept = nw[1, 1],
      std_error = nw[1, 2],
      p_value = nw[1, 4]
    )
  })

  # Combine all into one table
  results_bias_spf <- bind_rows(results)

  return(results_bias_spf)
}




#' Consensus SPF RMSE and Diebold–Mariano Test (Quarterly)
#'
#' Computes RMSEs and Diebold–Mariano statistics comparing SPF GDP
#' forecasts to benchmark models across horizons h = 0,...,4.
#'
#' @param spf_data Data frame with SPF forecasts and realized GDP growth (from `data_function_spf.R`).
#' @param ar_benchmark_data List containing benchmark forecast series (from `ar_benchmark_quarterly.R`).
#' @param EvalPeriod 2×1 numeric matrix with evaluation start/end years (default: 2002–2019).
#' @param DropPeriod Optional matrix of year ranges to exclude.
#' @param lagLength Optional lag length for Newey–West SEs (default: n^0.25).
#' #' @param SPFalternative Optional data frame with alternative SPF forecasts (from `data_function_spf.R`).
#'
#' @return List with RMSE table, DM statistics, and DM significance stars.
SPF_RMSE_DM_Test_quarterly <- function(spf_data, ar_benchmark_data,
                                       EvalPeriod = cbind(2002, 2019),
                                       DropPeriod = NA, lagLength = NA,
                                       SPFalternative = NULL) {

  # Merge SPF and AR-benchmark models
  evaluation_data <- spf_data %>%
    left_join(ar_benchmark_data$DAR_fc, by = "ref_period")

  evaluation_data <- evaluation_data %>%
    left_join(ar_benchmark_data$IAR_fc, by = "ref_period")

  evaluation_data <- evaluation_data %>%
    left_join(ar_benchmark_data$RWmean_fc, by = "ref_period")

  evaluation_data <- evaluation_data %>%
    left_join(ar_benchmark_data$NoChange_fc, by = "ref_period")

  # If alternative SPF forecasts for comparison
  if (!is.null(SPFalternative)) {
    # Select only the SPF columns to merge and rename them
    alt_cols <- SPFalternative %>%
      select(ref_period, spf_h0:spf_h4) %>%
      rename_with(~paste0("alt_", .), spf_h0:spf_h4)

    # Join the renamed columns
    evaluation_data <- evaluation_data %>%
      left_join(alt_cols, by = "ref_period")
  }

  # Drop missings
  evaluation_data <- evaluation_data %>%
    filter(!(is.na(spf_h0) | is.na(spf_h4)))

  # Adjust sample start and end points
  evaluation_data <- evaluation_data %>%
    filter(target_year < (EvalPeriod[2]+1) & target_year > (EvalPeriod[1]-1))

  # Drop periods if specified
  #if (any(!is.na(DropPeriod))) {
  #  for (i in 1:dim(DropPeriod)[1]) {
  #    evaluation_data <- evaluation_data %>%
  #      filter(!(target_year %in% c(DropPeriod[i,1]:DropPeriod[i,2]) ))
  #  }
  #}
  if (any(!is.na(DropPeriod))) {
    for (i in 1:nrow(DropPeriod)) {

      start_year <- floor(DropPeriod[i,1])
      end_year   <- floor(DropPeriod[i,2])

      start_frac <- DropPeriod[i,1] %% 1
      end_frac   <- DropPeriod[i,2] %% 1

      # Map decimals to quarters
      start_q <- ifelse(start_frac == 0, 0, ceiling(start_frac * 4))
      end_q   <- ifelse(end_frac == 0, 0, ceiling(end_frac * 4))

      evaluation_data <- evaluation_data %>%
        filter(!(
          # middle years
          (target_year > start_year & target_year < end_year) |

            # start year
            (target_year == start_year &
               (start_q == 0 | target_quarter > start_q)) |

            # end year
            (target_year == end_year &
               (end_q == 0 | target_quarter <= end_q))
        ))
    }
  }


  # Compute historical mean of GDP growth as simple benchmark forecast
  gdp_mean <- mean(evaluation_data$gdp_growth, na.rm = TRUE)
  actual <- evaluation_data$gdp_growth


  ### Root mean squared forecast errors

  # Loop over h = 0 to 4
  sq_error_loss <- lapply(0:4, function(h) {

    # Compute squared SPF and benchmark errors
    spf_sq_error       <- (actual - evaluation_data[[paste0("spf_h", h)]])^2
    benchmark_sq_error <- (actual - gdp_mean)^2
    dar_sq_error       <- (actual - evaluation_data[[paste0("DAR_h", h)]])^2
    iar_sq_error       <- (actual - evaluation_data[[paste0("IAR_h", h)]])^2
    RWmean_sq_error    <- (actual - evaluation_data[[paste0("RWmean_h", h)]])^2
    NoChange_sq_error  <- (actual - evaluation_data[[paste0("NoChange_h", h)]])^2
    if (!is.null(SPFalternative)) {
      spf_alt_sq_error <- (actual - evaluation_data[[paste0("alt_spf_h", h)]])^2
    }

    # Compute RMSE
    spf_mse       <- mean(spf_sq_error)
    hist_mean_mse <- mean(benchmark_sq_error)
    DAR_mse       <- mean(dar_sq_error)
    IAR_mse       <- mean(iar_sq_error)
    RW_mean_mse   <- mean(RWmean_sq_error)
    NoChange_mse  <- mean(NoChange_sq_error)
    if (!is.null(SPFalternative)) {
      spf_alt_mse <- mean(spf_alt_sq_error)
    }

    if (is.null(SPFalternative)) {
      tibble(
        horizon = h,
        spf_rmse       = sqrt(spf_mse),
        DAR_rmse       = sqrt(DAR_mse),
        IAR_rmse       = sqrt(IAR_mse),
        RW_rmse        = sqrt(RW_mean_mse),
        NoChange_rmse  = sqrt(NoChange_mse),
        hist_mean_rmse = sqrt(hist_mean_mse)
      )

    } else {
      tibble(
        horizon = h,
        spf_rmse       = sqrt(spf_mse),
        DAR_rmse       = sqrt(DAR_mse),
        IAR_rmse       = sqrt(IAR_mse),
        RW_rmse        = sqrt(RW_mean_mse),
        NoChange_rmse  = sqrt(NoChange_mse),
        hist_mean_rmse = sqrt(hist_mean_mse),
        spf_alt_rmse   = sqrt(spf_alt_mse)
      )
    }


  })


  ### Diebold-Mariano test
  n <- length(actual)

  # Loop over h = 0 to 4
  DM_Test <- lapply(0:4, function(h) {

    # Compute squared SPF and benchmark errors
    spf_sq_error       <- (actual - evaluation_data[[paste0("spf_h", h)]])^2
    benchmark_sq_error <- (actual - gdp_mean)^2
    dar_sq_error       <- (actual - evaluation_data[[paste0("DAR_h", h)]])^2
    iar_sq_error       <- (actual - evaluation_data[[paste0("IAR_h", h)]])^2
    RWmean_sq_error    <- (actual - evaluation_data[[paste0("RWmean_h", h)]])^2
    NoChange_sq_error  <- (actual - evaluation_data[[paste0("NoChange_h", h)]])^2
    if (!is.null(SPFalternative)) {
      spf_alt_sq_error <- (actual - evaluation_data[[paste0("alt_spf_h", h)]])^2
    }

    # Run DM Tests of competitor models versus SPF
    Loss_spf_hist_mean <- benchmark_sq_error - spf_sq_error
    Loss_spf_dar       <- dar_sq_error - spf_sq_error
    Loss_spf_iar       <- iar_sq_error - spf_sq_error
    Loss_spf_RWmean    <- RWmean_sq_error - spf_sq_error
    Loss_spf_NoChange  <- NoChange_sq_error - spf_sq_error
    if (!is.null(SPFalternative)) {
      Loss_spf_spf_alt <- spf_alt_sq_error - spf_sq_error
    }

    # Lag length for HAC standard errors
    if (is.na(lagLength)) {
      lags <- max(4, as.integer( n^0.25 ) ) # floor(4*( n /100)^(2/9))
    } else {
      lags <- lagLength
    }

    dm_test <- lm(Loss_spf_hist_mean ~ 1)
    nw_var  <- NeweyWest(dm_test, lag = lags, prewhite = FALSE)
    dm_test_hist_mean <- coef(dm_test) / sqrt(nw_var)

    dm_test <- lm(Loss_spf_dar ~ 1)
    nw_var  <- NeweyWest(dm_test, lag = lags, prewhite = FALSE)
    dm_test_dar <- coef(dm_test) / sqrt(nw_var)

    dm_test <- lm(Loss_spf_iar ~ 1)
    nw_var  <- NeweyWest(dm_test, lag = lags, prewhite = FALSE)
    dm_test_iar <- coef(dm_test) / sqrt(nw_var)

    dm_test <- lm(Loss_spf_RWmean ~ 1)
    nw_var  <- NeweyWest(dm_test, lag = lags, prewhite = FALSE)
    dm_test_rwmean <- coef(dm_test) / sqrt(nw_var)

    dm_test <- lm(Loss_spf_NoChange ~ 1)
    nw_var  <- NeweyWest(dm_test, lag = lags, prewhite = FALSE)
    dm_test_nochange <- coef(dm_test) / sqrt(nw_var)

    if (!is.null(SPFalternative)) {
      dm_test <- lm(Loss_spf_spf_alt ~ 1)
      nw_var  <- NeweyWest(dm_test, lag = lags, prewhite = FALSE)
      dm_test_spf_alt <- coef(dm_test) / sqrt(nw_var)
    }

    if (is.null(SPFalternative)) {
      tibble(
        horizon = h,
        spf_DAR = dm_test_dar,
        spf_IAR = dm_test_iar,
        SPF_RW_mean = dm_test_rwmean,
        spf_NoChange = dm_test_nochange,
        spf_hist_mean = dm_test_hist_mean
      )

    } else {
      tibble(
        horizon = h,
        spf_DAR = dm_test_dar,
        spf_IAR = dm_test_iar,
        SPF_RW_mean = dm_test_rwmean,
        spf_NoChange = dm_test_nochange,
        spf_hist_mean = dm_test_hist_mean,
        spf_spf_alt = dm_test_spf_alt,
      )
    }


  })

  RMSE <- bind_rows(sq_error_loss)
  DM_Test <- bind_rows(DM_Test)


  # Function to convert DM statistic into star code
  dm_to_stars <- function(x) {

    x <- as.numeric(x)

    sx <- sign(x)
    ax <- abs(x)

    d <- case_when(
      # One-sided 10% significance: DM > 1.282
      x > 1.282 & x < 1.645 ~ 9,

      # Two-sided thresholds
      ax < 1.645 ~ 0,
      ax < 1.96  ~ 1,
      ax < 2.576 ~ 2,
      TRUE       ~ 3
    )

    return(sx * d)
  }

  # Apply to all DM columns except 'horizon'
  DM_stars <- DM_Test %>%
    mutate(across(
      .cols = -horizon,
      .fns  = dm_to_stars,
      .names = "{.col}"
    ))


  output <- list(RMSE = RMSE, DM_Test = DM_Test, DM_stars = DM_stars)
  return(output)
}




#' Consensus SPF RMSE and Diebold–Mariano Test (Yearly)
#'
#' Computes RMSEs and Diebold–Mariano statistics comparing annual SPF GDP
#' forecasts to benchmark models by horizon (y1–y2) and forecast quarter.
#'
#' @param spf_annual Data frame with annual SPF forecasts and realized GDP (from `data_function_spf_yearly.R`).
#' @param ar_benchmark_data List containing annual benchmark forecast series (from `ar_benchmark_yearly.R`).
#' @param EvalPeriod 2×1 numeric matrix with evaluation start/end years (default: 2002–2019).
#' @param DropPeriod Optional matrix of year ranges to exclude.
#' @param lagLength Optional lag length for Newey–West SEs (default: n^0.25).
#'
#' @return List with RMSE table, DM statistics, and DM significance stars.
SPF_RMSE_DM_Test_yearly <- function(spf_annual, ar_benchmark_data,
                                    EvalPeriod = cbind(2002, 2019),
                                    DropPeriod = NA, lagLength = NA) {

  library(purrr)

  # Bring SPF forecasts into wide shape
  spf_wide <- spf_annual %>%
    mutate(
      horizon = case_when(
        target_year == forecast_year     ~ "SPF_y1",
        target_year == forecast_year + 1 ~ "SPF_y2"
      ),
      ref_period = as.yearqtr(paste0(forecast_year, " Q", forecast_quarter))
    ) %>%
    select(forecast_year, forecast_quarter, ref_period, horizon, SPF_yearly) %>%
    tidyr::pivot_wider(
      names_from = horizon,
      values_from = SPF_yearly
    ) %>%
    arrange(forecast_year, forecast_quarter) %>%   # ensure time order
    mutate(
      SPF_y2 = dplyr::lag(SPF_y2, 4)                # overwrite SPF_y2 with its lag
    )

  # Merge GDP realizations to SPF
  gdp_merge <- spf_annual %>%
    select(target_year, gdp_yearly) %>%
    distinct() %>%
    rename(forecast_year = target_year)

  spf_wide <- spf_wide %>%
    left_join(gdp_merge, by = "forecast_year")


  # Merge benchmark models
  model_names <- c("DAR_fc", "DAR_alt_fc", "IAR_fc", "RWmean_fc", "NoChange_fc")

  EvalData <- purrr::reduce(
    model_names,
    .init = spf_wide,
    .f = function(df, model) {
      df %>% left_join(AR_bench_yearly[[model]], by = "ref_period")
    }
  )

  # Delete initial years without next-year forecasts of benchmark models and
  # insert NA for benchmark forecasts where SPF didn't provide next-year
  # forecasts for 2021 in 2020-Q1
  next_year_cols <- paste0(model_names, "2")
  EvalData <- EvalData %>%
    filter(!is.na(.data[[next_year_cols[1]]])) %>%
    mutate(across(all_of(next_year_cols),
                  ~ if_else(forecast_year == 2021 & forecast_quarter == 1, NA_real_, .)))


  # Adjust sample start and end points
  EvalData <- EvalData %>%
    filter(forecast_year < (EvalPeriod[2]+1) & forecast_year > (EvalPeriod[1]-1))

  # Compute historical mean of GDP growth as simple benchmark forecast
  gdp_mean <- mean(EvalData$gdp_yearly[EvalData$forecast_quarter==1])

  # Drop periods if specified
  if (any(!is.na(DropPeriod))) {
    for (i in 1:dim(DropPeriod)[1]) {
      EvalData <- EvalData %>%
        filter(!(forecast_year %in% c(DropPeriod[i,1]:DropPeriod[i,2]) ))
    }
  }


  ### Root mean squared forecast errors

  # Define horizons and quarters
  horizons <- 2:1
  quarters <- 1:4

  # Loop over horizons and quarters to compute RMSE tables
  RMSE_yearly <- map_dfr(horizons, function(h) {
    map_dfr(quarters, function(q) {
      # Filter for forecast quarter q
      df_sub <- EvalData %>% filter(forecast_quarter == q)

      # Select actual values and corresponding forecast for horizon h
      actual      <- df_sub$gdp_yearly
      spf_fc      <- if (h == 1) df_sub$SPF_y1 else df_sub$SPF_y2
      DAR_fc      <- df_sub[[paste0("DAR_fc", h)]]
      DAR_alt_fc  <- df_sub[[paste0("DAR_alt_fc", h)]]
      IAR_fc      <- df_sub[[paste0("IAR_fc", h)]]
      RWmean_fc   <- df_sub[[paste0("RWmean_fc", h)]]
      NoChange_fc <- df_sub[[paste0("NoChange_fc", h)]]

      # Compute squared errors
      spf_sq_error      <- (actual - spf_fc)^2
      DAR_sq_error      <- (actual - DAR_fc)^2
      DAR_alt_sq_error  <- (actual - DAR_alt_fc)^2
      IAR_sq_error      <- (actual - IAR_fc)^2
      RWmean_sq_error   <- (actual - RWmean_fc)^2
      NoChange_sq_error <- (actual - NoChange_fc)^2
      Histmean_sq_error <- (actual - gdp_mean)^2

      # Compute RMSE (handle possible NAs)
      tibble(
        horizon = h,
        forecast_quarter = q,
        spf_rmse       = sqrt(mean(spf_sq_error, na.rm = TRUE)),
        DAR_rmse       = sqrt(mean(DAR_sq_error, na.rm = TRUE)),
        DAR_alt_rmse   = sqrt(mean(DAR_alt_sq_error, na.rm = TRUE)),
        IAR_rmse       = sqrt(mean(IAR_sq_error, na.rm = TRUE)),
        RWmean_rmse    = sqrt(mean(RWmean_sq_error, na.rm = TRUE)),
        NoChange_rmse  = sqrt(mean(NoChange_sq_error, na.rm = TRUE)),
        Histmean_rmse  = sqrt(mean(Histmean_sq_error, na.rm = TRUE))
      )
    })
  })


  DM_Test_yearly <- map_dfr(horizons, function(h) {
    map_dfr(quarters, function(q) {
      df_sub <- EvalData %>% filter(forecast_quarter == q)

      actual <- df_sub$gdp_yearly
      spf_fc <- if (h == 1) df_sub$SPF_y1 else df_sub$SPF_y2

      DAR_fc      <- df_sub[[paste0("DAR_fc", h)]]
      DAR_alt_fc  <- df_sub[[paste0("DAR_alt_fc", h)]]
      IAR_fc      <- df_sub[[paste0("IAR_fc", h)]]
      RWmean_fc   <- df_sub[[paste0("RWmean_fc", h)]]
      NoChange_fc <- df_sub[[paste0("NoChange_fc", h)]]

      spf_sq_error       <- (actual - spf_fc)^2
      dar_sq_error       <- (actual - DAR_fc)^2
      dar_alt_sq_error   <- (actual - DAR_alt_fc)^2
      iar_sq_error       <- (actual - IAR_fc)^2
      RWmean_sq_error    <- (actual - RWmean_fc)^2
      NoChange_sq_error  <- (actual - NoChange_fc)^2
      Histmean_sq_error  <- (actual - gdp_mean)^2

      Loss_spf_dar      <- dar_sq_error - spf_sq_error
      Loss_spf_dar_alt  <- dar_alt_sq_error - spf_sq_error
      Loss_spf_iar      <- iar_sq_error - spf_sq_error
      Loss_spf_RWmean   <- RWmean_sq_error - spf_sq_error
      Loss_spf_NoChange <- NoChange_sq_error - spf_sq_error
      Loss_spf_Histmean <- Histmean_sq_error - spf_sq_error


      #### Optional quick and dirty plots - squared losses
      plot_true = FALSE
      if (plot_true == TRUE) {

        # Collect all series in one list
        all_errors <- list(
          spf_sq_error,
          #dar_sq_error,
          dar_alt_sq_error#,
          #iar_sq_error,
          #RWmean_sq_error,
          #NoChange_sq_error
        )

        # Determine y-axis limits
        ymin <- min(sapply(all_errors, min), na.rm = TRUE)
        ymax <- max(sapply(all_errors, max), na.rm = TRUE)

        # Plot
        plot(spf_sq_error, type = "l", lwd = 2, col = 1,
             ylab = "Squared Error", xlab = "Index",
             ylim = c(ymin, ymax),
             main = "Squared Errors Over Time")

        lines(dar_sq_error,       col = 2, lwd = 2)
        lines(dar_alt_sq_error,   col = 3, lwd = 2)
        lines(iar_sq_error,       col = 4, lwd = 2)
        lines(RWmean_sq_error,    col = 5, lwd = 2)
        lines(NoChange_sq_error,  col = 6, lwd = 2)
        lines(Histmean_sq_error,  col = 7, lwd = 2)

        legend("topright",
               legend = c("SPF", "DAR", "DAR_alt", "IAR", "RWmean", "NoChange", "HistMean"),
               col    = 1:7,
               lwd    = 2)



        #### Optional quick and dirty plots - loss differences

        # Collect all series in one list
        all_errors <- list(
          #Loss_spf_hist_mean,
          #Loss_spf_dar,
          Loss_spf_dar_alt#,
          #Loss_spf_iar,
          #Loss_spf_RWmean,
          #Loss_spf_NoChange
        )

        # Determine y-axis limits
        ymin <- min(sapply(all_errors, min), na.rm = TRUE)
        ymax <- max(sapply(all_errors, max), na.rm = TRUE)

        # Plot
        plot(Loss_spf_dar, type = "l", lwd = 2, col = 1,
             ylab = "Squared Error", xlab = "Index",
             ylim = c(ymin, ymax),
             main = "Loss Differences Over Time")

        lines(Loss_spf_dar_alt,   col = 2, lwd = 2)
        lines(Loss_spf_iar,       col = 3, lwd = 2)
        lines(Loss_spf_RWmean,    col = 4, lwd = 2)
        lines(Loss_spf_NoChange,  col = 5, lwd = 2)
        lines(Loss_spf_Histmean,  col = 6, lwd = 2)

        legend("topright",
               legend = c("DAR", "DAR_alt", "IAR", "RWmean", "NoChange", "Histmean"),
               col    = 1:6,
               lwd    = 2)
      }



      n <- length(Loss_spf_dar_alt)

      lags <- if (is.na(lagLength)) as.integer(n^0.25) else lagLength
      # lags <- if (is.na(lagLength)) floor(4*( n /100)^(2/9)) else lagLength

      # Define a helper function to run DM test and get statistic
      run_dm_test <- function(loss_diff) {
        dm_test <- lm(loss_diff ~ 1)
        nw_var <- NeweyWest(dm_test, lag = lags, prewhite = FALSE)
        coef(dm_test) / sqrt(nw_var)
      }

      tibble(
        horizon = h,
        forecast_quarter = q,
        spf_DAR       = run_dm_test(Loss_spf_dar),
        spf_DAR_alt   = run_dm_test(Loss_spf_dar_alt),
        spf_IAR       = run_dm_test(Loss_spf_iar),
        spf_RWmean    = run_dm_test(Loss_spf_RWmean),
        spf_NoChange  = run_dm_test(Loss_spf_NoChange),
        spf_HistMean  = run_dm_test(Loss_spf_Histmean)
      )
    })
  })


  # Function to convert DM statistic into star code
  dm_to_stars <- function(x) {

    x <- as.numeric(x)

    sx <- sign(x)
    ax <- abs(x)

    d <- case_when(
      ax < 1.645 ~ 0,
      ax < 1.96  ~ 1,
      ax < 2.576 ~ 2,
      TRUE       ~ 3
    )

    return(sx * d)
  }

  # Apply to all DM columns except 'horizon'
  DM_stars <- DM_Test_yearly %>%
    mutate(across(
      .cols = -c(horizon, forecast_quarter),
      .fns  = dm_to_stars,
      .names = "{.col}"
    ))

  output <- list(RMSE_yearly = RMSE_yearly, DM_Test_yearly = DM_Test_yearly, DM_stars_yearly = DM_stars)
  return(output)
}




#' Panel SPF Forecast Bias Test
#'
#' Tests bias in SPF forecasts for each horizon h = 0,...,4 across forecasters
#' by regressing forecast errors on a constant and computing
#' cluster-robust standard errors (clustered by forecaster_id).
#'
#' @param SPF_panel Data frame with individual SPF forecasts (`spf_h0`–`spf_h4`),
#'        realized GDP growth (`gdp_growth`), and `forecaster_id`.
#' @param EvalPeriod Numeric vector of length 2 giving start and end years for evaluation (default: c(2002, 2019)).
#' @param digits Number of digits to round output (default: 3).
#' @param DropPeriod Optional matrix of periods (start–end years) to exclude from evaluation.
#'
#' @return Data frame with bias estimates (alpha), cluster-robust SEs,
#'         N, and horizon as row names.
#' @export
BiasSPFPanel <- function(SPF_panel, EvalPeriod = c(2002, 2019), digits = 3, DropPeriod = NA) {

  # Filter evaluation period
  SPF_panel <- SPF_panel %>%
    filter(target_year >= EvalPeriod[1],
           target_year <= EvalPeriod[2])

  # Drop periods if specified
  if (any(!is.na(DropPeriod))) {
    for (i in 1:nrow(DropPeriod)) {
      SPF_panel <- SPF_panel %>%
        filter(!(target_year %in% (DropPeriod[i,1]:DropPeriod[i,2])))
    }
  }

  # Horizons
  horizons <- 0:4

  # Run bias regressions
  bias_results <- lapply(horizons, function(h) {

    df <- SPF_panel %>%
      mutate(f = .data[[paste0("spf_h", h)]],
             y = gdp_growth) %>%
      filter(!is.na(f), !is.na(forecaster_id))

    model <- lm(y - f ~ 1, data = df)   # bias = forecast error regressed on constant

    # Cluster-robust covariance by forecaster
    vcov_cluster <- vcovCL(model, cluster = df$forecaster_id)

    list(model = model,
         vcov  = vcov_cluster,
         df    = df)
  })


  # Build table
  fmt <- function(coef, se) {
    paste0("$\\underset{(", se, ")}{", coef, "}$")
  }

  bias_table <- data.frame(
    horizon = horizons,

    alpha = mapply(fmt,
                   sapply(bias_results, function(x) round(coef(x$model)[1], digits)),
                   sapply(bias_results, function(x) round(sqrt(diag(x$vcov))[1], digits))
    ),

    N = sapply(bias_results, function(x) nobs(x$model))
  )

  # Set horizon as row names
  row.names(bias_table) <- bias_table$horizon
  bias_table$horizon <- NULL

  return(bias_table)

}




#' Summary statistics for consensus SPF and real GDP growth
#'
#' @param spf_data Data frame with SPF forecasts and realized GDP growth (from `data_function_spf.R`).
#' @param EvalPeriod Numeric 2×1 matrix giving start and end years for evaluation (default: 2002–2019).
#' @param DropPeriod Optional matrix of periods (start–end years) to exclude from evaluation.
#'
#' @return Tibble with intercept estimates, standard errors, and p-values for each forecast horizon.
#' @export
Summary_Stats <- function(spf_data, EvalPeriod = cbind(2002, 2019), DropPeriod = NA) {

  # Drop missings
  evaluation_data <- spf_data %>%
    filter(!(is.na(spf_h0) | is.na(spf_h4)))

  # Adjust sample start and end points
  evaluation_data <- evaluation_data %>%
    filter(target_year < (EvalPeriod[2]+1) & target_year > (EvalPeriod[1]-1))

  # Drop periods if specified
  if (any(!is.na(DropPeriod))) {
    for (i in 1:dim(DropPeriod)[1]) {
      evaluation_data <- evaluation_data %>%
        filter(!(target_year %in% c(DropPeriod[i,1]:DropPeriod[i,2]) ))
    }
  }

  # Summary statistics
  summary_stats <- evaluation_data %>%
    select(gdp_growth, spf_h0, spf_h1, spf_h2, spf_h3, spf_h4) %>%
    pivot_longer(cols = everything(),
                 names_to = "variable",
                 values_to = "value") %>%
    group_by(variable) %>%
    summarise(
      mean = mean(value, na.rm = TRUE),
      sd   = sd(value, na.rm = TRUE),
      min  = min(value, na.rm = TRUE),
      max  = max(value, na.rm = TRUE),
      N    = sum(!is.na(value)),
      .groups = "drop"
    ) %>%
    as.data.frame()

  # Move variable column to row names
  rownames(summary_stats) <- summary_stats$variable
  summary_stats$variable <- NULL

  return(summary_stats)
}


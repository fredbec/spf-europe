### Read in and merge SPF and RGDP data
.prep_spf_data <- function(FilterOpt = NA, gamma_est = FALSE, spf_h = NA, Month = 1) {

  ### Read in filtered quarterly SPF forecasts
  if (is.na(FilterOpt)) {
    # SPF without US
    spf_data <- read.csv(sprintf("data/filter_spf_data_medianfc_month%d.csv", Month))

    # SPF filter augmented by US-SPF
  } else if (FilterOpt == 'US_SPF') {

    if (gamma_est) {

      # SPF filter augmented by US-SPF with estimation of gamma
      if (is.na(spf_h)) {
        spf_data <- read.csv(sprintf("data/filter_spf_data_medianfc_withus_withgammaest_month%d.csv", Month))
      } else if (spf_h == 0) {
        spf_data <- read.csv(sprintf("data/filter_spf_data_medianfc_withus_withgammaest_stepahead0_month%d.csv", Month))
      } else if (spf_h == 1) {
        spf_data <- read.csv(sprintf("data/filter_spf_data_medianfc_withus_withgammaest_stepahead1_month%d.csv", Month))
      } else if (spf_h == 2) {
        spf_data <- read.csv(sprintf("data/filter_spf_data_medianfc_withus_withgammaest_stepahead2_month%d.csv", Month))
      } else if (spf_h == 3) {
        spf_data <- read.csv(sprintf("data/filter_spf_data_medianfc_withus_withgammaest_stepahead3_month%d.csv", Month))
      }
    } else {

      # SPF filter augmented by US-SPF without estimation of gamma
      if (is.na(spf_h)) {
        spf_data <- read.csv(sprintf("data/filter_spf_data_medianfc_withus_month%d.csv", Month))
      } else if (spf_h == 0) {
        spf_data <- read.csv(sprintf("data/filter_spf_data_medianfc_withus_stepahead0_month%d.csv", Month))
      } else if (spf_h == 1) {
        spf_data <- read.csv(sprintf("data/filter_spf_data_medianfc_withus_stepahead1_month%d.csv", Month))
      } else if (spf_h == 2) {
        spf_data <- read.csv(sprintf("data/filter_spf_data_medianfc_withus_stepahead2_month%d.csv", Month))
      } else if (spf_h == 3) {
        spf_data <- read.csv(sprintf("data/filter_spf_data_medianfc_withus_stepahead3_month%d.csv", Month))
      }
    }

  } else if (FilterOpt == 'IndProd') {
    spf_data <- read.csv(sprintf("data/filter_spf_data_medianfc_withip_withgammaest_month%d.csv", Month))
  }

  # Read in real-time GDP
  rgdp_pre  <- read.csv("data/revdatpre14.csv")
  rgdp_post <- read.csv("data/revdatpost14.csv")

  # Merge GDP
  rgdp_pre <- rgdp_pre[ - which(rgdp_pre$origin_year == 2014 & rgdp_pre$origin_month > 9), ]
  rgdp_pre$origin_day <- NA
  rgdp <- rbind(rgdp_pre,rgdp_post)

  # Compute quarterly GDP growth rates
  rgdp_all <- rgdp %>%
    arrange(origin_year, origin_month, target_year, target_quarter) %>%  # ensure correct order
    group_by(origin_year, origin_month) %>%
    mutate(
      gdp_growth = ( (rgdp / lag(rgdp) ) ^ 4 - 1) * 100,
    ) %>%
    ungroup()

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
  release <- 3

  # Keep second releases
  rgdp_rel <- rgdp_all %>%
    mutate(
      # Convert to yearqtr (e.g., 2018 Q2 → 2018.25)
      ref_period = as.yearqtr(paste(target_year, target_quarter), format = "%Y %q"),

      # Create origin_date from origin_year and origin_month
      origin_date = make_date(origin_year, origin_month, 1)
    )

  rgdp_rel <- rgdp_rel %>%
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

  evaluation_data_cy <- rgdp_rel %>%
    left_join(spf_forecasts_cy, by = "ref_period") # %>%
  # filter(ref_period >= as.yearqtr("2002 Q1", format = "%Y Q%q"))

  evaluation_data_ny <- rgdp_rel %>%
    left_join(spf_forecasts_ny, by = "ref_period")

  spf_forecasts_ny$target_year <- target_aux[,2]
  spf_forecasts_ny$target_quarter <- target_aux[,1]
  spf_forecasts_cy$target_year <- target_aux[,2]
  spf_forecasts_cy$target_quarter <- target_aux[,1]



  ### Yearly data for yearly evaluation

  # Yearly SPF forecasts
  spf_yearly <- read.csv("data/spf_median_forecast.csv")

  # Compute yearly GDP growth rates
  rgdp_yearly <- rgdp %>%
    arrange(origin_year, origin_month, target_year, target_quarter) %>%  # ensure correct order
    group_by(origin_year, origin_month) %>%
    mutate(
      sum4 = zoo::rollsumr(rgdp, 4, fill = NA),        # sum of t, t-1, t-2, t-3
      sum4_prev = dplyr::lag(sum4, 4),                 # sum of t-4, t-5, t-6, t-7
      gdp_yoy_sum = (sum4 / sum4_prev - 1) * 100       # percent change between the two 4-quarter sums
    ) %>%
    ungroup()

  ## Fixed month, e.g., origin_month = 3
  #rgdp_yearly_eval <- rgdp_yearly %>%
  #  filter(target_quarter == 4,
  #         origin_month == 3,
  #         origin_year == target_year + 1)

  # k-th release
  k_rel <- 3   # possible future input

  rgdp_yearly_eval <- rgdp_yearly %>%
    filter(
      target_quarter == 4,
      origin_year == target_year + 1
    ) %>%
    group_by(target_year) %>%
    arrange(origin_year, origin_month) %>%  # sort by actual release date
    slice(k_rel) %>%                        # choose k-th release
    ungroup()

  #plot(rgdp_yearly_eval$gdp_yoy_sum,type="l")

  # Merge annual SPF and annual GDP growth rates
  spf_annual <- spf_yearly %>%
    left_join(
      rgdp_yearly_eval %>% select(target_year, gdp_yearly = gdp_yoy_sum),
      by = "target_year"
    ) %>%
    rename(SPF_yearly = ens_fc)

  # Find all forecast_years with any missing gdp_yearly
  years_with_na <- spf_annual %>%
    filter(is.na(gdp_yearly)) %>%
    pull(forecast_year) %>%
    unique()

  # Remove all rows where forecast_year is in years_with_na
  spf_annual <- spf_annual %>%
    filter(!forecast_year %in% years_with_na)


  # Output of this function
  output <- list(
    rgdp_all           = rgdp_all,
    spf_forecasts_cy   = spf_forecasts_cy,
    spf_forecasts_ny   = spf_forecasts_ny,
    evaluation_data_cy = evaluation_data_cy,
    evaluation_data_ny = evaluation_data_ny,
    spf_annual         = spf_annual
  )

  return(output)
}


### Wrapper function calling prep_spf_data.R, filtered ECB-SPF consensus forecasts
#   either with or without US-SPF/Industrial Production
data_function_spf <- function(FilterOpt = NA, gamma_estimation = FALSE, endMonth = 1) {

  # Possible future input
  spf_h = NA

  if (is.na(FilterOpt)) {

    ### ECB-SPF filtered without US-SPF
    SPF <- .prep_spf_data(Month = endMonth)

  } else if (FilterOpt == 'US_SPF') {
    ### ECB-SPF augmented by US-SPF

    if (is.na(spf_h)) {
      ### Prepare SPF data augmented by US-SPF calibrated on the respective forecast horizon h

      # Initialize using h = 0
      SPF <- .prep_spf_data(FilterOpt = FilterOpt, gamma_est = gamma_estimation, spf_h = 0, endMonth)
      rgdp_all           <- SPF$rgdp_all
      spf_forecasts_cy   <- SPF$spf_forecasts_cy
      spf_forecasts_ny   <- SPF$spf_forecasts_ny
      evaluation_data_cy <- SPF$evaluation_data_cy
      evaluation_data_ny <- SPF$evaluation_data_ny

      # Loop over horizons h = 1:3 (or more)
      for (h in 1:3) {
        SPF <- .prep_spf_data(FilterOpt = 'US_SPF', gamma_est = gamma_estimation, spf_h = h, endMonth)

        # Update the corresponding columns in the forecasts and evaluation data
        spf_forecasts_cy[[paste0("spf_h", h)]]   <- SPF$spf_forecasts_cy[[paste0("spf_h", h)]]
        spf_forecasts_ny[[paste0("spf_h", h)]]   <- SPF$spf_forecasts_ny[[paste0("spf_h", h)]]
        evaluation_data_cy[[paste0("spf_h", h)]] <- SPF$evaluation_data_cy[[paste0("spf_h", h)]]
        evaluation_data_ny[[paste0("spf_h", h)]] <- SPF$evaluation_data_ny[[paste0("spf_h", h)]]
      }
    } #else if spf_h = h in case we want to read out filtered SPF data calibrated for a specific h only

  } else if (FilterOpt == 'IndProd') {

    ### ECB-SPF augmented Industrial Production
    SPF <- .prep_spf_data(FilterOpt, endMonth)

  }

  return(SPF)
}




#' SPF Forecast Bias Test
#'
#' Computes bias in SPF GDP forecasts across horizons (h = 0,...,4)
#' using OLS with Newey–West standard errors.
#'
#' @param spf_data Data frame realized and forecast GDP growth (from `data_function_spf.R`).
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
  results_bias_spf <- bind_rows(results)

  return(results_bias_spf)
}




####### HEADER
SPF_RMSE_DM_Test_yearly <- function(spf_data, ar_benchmark_data,
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




####### HEADER
SPF_RMSE_DM_Test <- function(spf_data, ar_benchmark_data,
                             EvalPeriod = cbind(2002, 2019),
                             DropPeriod = NA, lagLength = NA) {

  # Merge SPF and AR-banchmark models
  evaluation_data <- spf_data %>%
    left_join(ar_benchmark_data$DAR_fc, by = "ref_period")

  evaluation_data <- evaluation_data %>%
    left_join(ar_benchmark_data$IAR_fc, by = "ref_period")

  evaluation_data <- evaluation_data %>%
    left_join(ar_benchmark_data$RWmean_fc, by = "ref_period")

  evaluation_data <- evaluation_data %>%
    left_join(ar_benchmark_data$NoChange_fc, by = "ref_period")

  # Drop missings
  evaluation_data <- evaluation_data %>%
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

    # Compute RMSE
    spf_mse       <- mean(spf_sq_error)
    hist_mean_mse <- mean(benchmark_sq_error)
    DAR_mse       <- mean(dar_sq_error)
    IAR_mse       <- mean(iar_sq_error)
    RW_mean_mse   <- mean(RWmean_sq_error)
    NoChange_mse  <- mean(NoChange_sq_error)

    tibble(
      horizon = h,
      spf_rmse       = sqrt(spf_mse),
      DAR_rmse       = sqrt(DAR_mse),
      IAR_rmse       = sqrt(IAR_mse),
      RW_rmse        = sqrt(RW_mean_mse),
      NoChange_rmse  = sqrt(NoChange_mse),
      hist_mean_rmse = sqrt(hist_mean_mse)
    )

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

    # Run DM Tests of competitor models versus SPF
    Loss_spf_hist_mean <- benchmark_sq_error - spf_sq_error
    Loss_spf_dar       <- dar_sq_error - spf_sq_error
    Loss_spf_iar       <- iar_sq_error - spf_sq_error
    Loss_spf_RWmean    <- RWmean_sq_error - spf_sq_error
    Loss_spf_NoChange  <- NoChange_sq_error - spf_sq_error

    # Lag length for HAC standard errors
    if (is.na(lagLength)) {
      lags <- as.integer( n^0.25 )
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

    tibble(
      horizon = h,
      spf_DAR = dm_test_dar,
      spf_IAR = dm_test_iar,
      SPF_RW_mean = dm_test_rwmean,
      spf_NoChange = dm_test_nochange,
      spf_hist_mean = dm_test_hist_mean
    )

  })

  RMSE <- bind_rows(sq_error_loss)
  DM_Test <- bind_rows(DM_Test)


  # Function to convert DM statistic into star code
  dm_to_stars <- function(x) {
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



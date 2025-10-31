### Read in and merge SPF and RGDP data
.prep_spf_data <- function(FilterOpt = NA, gamma_est = FALSE, spf_h = NA, Month = 2) {

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
  rgdp_all <- rbind(rgdp_pre,rgdp_post)

  # Compute quarterly GDP growth rates
  rgdp_all <- rgdp_all %>%
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
  release <- 2

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
    left_join(spf_forecasts_cy, by = "ref_period") # %>%
  # filter(ref_period >= as.yearqtr("2002 Q1", format = "%Y Q%q"))

  evaluation_data_ny <- rgdp %>%
    left_join(spf_forecasts_ny, by = "ref_period")

  spf_forecasts_ny$target_year <- target_aux[,2]
  spf_forecasts_ny$target_quarter <- target_aux[,1]
  spf_forecasts_cy$target_year <- target_aux[,2]
  spf_forecasts_cy$target_quarter <- target_aux[,1]

  output <- list(
    rgdp_all           = rgdp_all,
    spf_forecasts_cy   = spf_forecasts_cy,
    spf_forecasts_ny   = spf_forecasts_ny,
    evaluation_data_cy = evaluation_data_cy,
    evaluation_data_ny = evaluation_data_ny
  )

  return(output)
}


### Wrapper function calling prep_spf_data.R, filtered ECB-SPF consensus forecasts
#   either with or without US-SPF/Industrial Production
data_function_spf <- function(FilterOpt = NA, gamma_estimation = FALSE, endMonth = 2) {

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
SPF_RMSE_DM_Test <- function(spf_data, ar_benchmak_data,
                             EvalPeriod = cbind(2002, 2019),
                             DropPeriod = NA, lagLength = NA) {

  # Merge SPF and AR-banchmark models
  evaluation_data <- spf_data %>%
    left_join(ar_benchmak_data$DAR_fc, by = "ref_period")

  evaluation_data <- evaluation_data %>%
    left_join(ar_benchmak_data$IAR_fc, by = "ref_period")

  evaluation_data <- evaluation_data %>%
    left_join(ar_benchmak_data$RWmean_fc, by = "ref_period")

  evaluation_data <- evaluation_data %>%
    left_join(ar_benchmak_data$NoChange_fc, by = "ref_period")

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
      hist_mean_rmse = sqrt(hist_mean_mse),
      RW_rmse        = sqrt(RW_mean_mse),
      DAR_rmse       = sqrt(DAR_mse),
      IAR_rmse       = sqrt(IAR_mse),
      NoChange_rmse  = sqrt(NoChange_mse)
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
      spf_hist_mean = dm_test_hist_mean,
      SPF_RW_mean = dm_test_rwmean,
      spf_DAR = dm_test_dar,
      spf_IAR = dm_test_iar,
      spf_NoChange = dm_test_nochange
    )

  })

  RMSE <- bind_rows(sq_error_loss)
  DM_Test <- bind_rows(DM_Test)

  output <- list(RMSE = RMSE, DM_Test = DM_Test)
  return(output)
}



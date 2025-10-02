### Read in and merge SPF and RGDP data
.prep_spf_data <- function(FilterOpt = NA, gamma_est = FALSE, spf_h = NA) {

  ### Read in filtered quarterly SPF forecasts
  if (is.na(FilterOpt)) {
    # SPF without US
    spf_data <- read.csv("data/filter_spf_data_medianfc.csv")

    # SPF filter augmented by US-SPF
  } else if (FilterOpt == 'US_SPF') {

    if (gamma_est) {

      # SPF filter augmented by US-SPF with estimation of gamma
      if (is.na(spf_h)) {
        spf_data <- read.csv("data/filter_spf_data_medianfc_withus_withgammaest.csv")
      } else if (spf_h == 0) {
        spf_data <- read.csv("data/filter_spf_data_medianfc_withus_withgammaest_stepahead0.csv")
      } else if (spf_h == 1) {
        spf_data <- read.csv("data/filter_spf_data_medianfc_withus_withgammaest_stepahead1.csv")
      } else if (spf_h == 2) {
        spf_data <- read.csv("data/filter_spf_data_medianfc_withus_withgammaest_stepahead2.csv")
      } else if (spf_h == 3) {
        spf_data <- read.csv("data/filter_spf_data_medianfc_withus_withgammaest_stepahead3.csv")
      }
    } else {

      # SPF filter augmented by US-SPF without estimation of gamma
      if (is.na(spf_h)) {
        spf_data <- read.csv("data/filter_spf_data_medianfc_withus.csv")
      } else if (spf_h == 0) {
        spf_data <- read.csv("data/filter_spf_data_medianfc_withus_stepahead0.csv")
      } else if (spf_h == 1) {
        spf_data <- read.csv("data/filter_spf_data_medianfc_withus_stepahead1.csv")
      } else if (spf_h == 2) {
        spf_data <- read.csv("data/filter_spf_data_medianfc_withus_stepahead2.csv")
      } else if (spf_h == 3) {
        spf_data <- read.csv("data/filter_spf_data_medianfc_withus_stepahead3.csv")
      }
    }

  } #else if (FilterOpt == 'IndProd') {
  #spf_data <- read.csv("data/filter_spf_data_medianfc_???.csv")
  #}

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
data_function_spf <- function(FilterOpt = NA, gamma_estimation = FALSE) {

  #Possible future input
  spf_h = NA

  if (is.na(FilterOpt)) {

    ### ECB-SPF filtered without US-SPF
    SPF <- .prep_spf_data()

  } else {
    ### ECB-SPF augmented by US-SPF or Industrial Production

    if (is.na(spf_h)) {
      ### Prepare SPF data augmented by US-SPF calibrated on the respective forecast horizon h

      # Initialize using h = 0
      SPF <- .prep_spf_data(FilterOpt = FilterOpt, gamma_est = gamma_estimation, spf_h = spf_h)
      rgdp_all           <- SPF$rgdp_all
      spf_forecasts_cy   <- SPF$spf_forecasts_cy
      spf_forecasts_ny   <- SPF$spf_forecasts_ny
      evaluation_data_cy <- SPF$evaluation_data_cy
      evaluation_data_ny <- SPF$evaluation_data_ny

      # Loop over horizons h = 1:3 (or more)
      for (h in 1:3) {
        SPF <- .prep_spf_data(FilterOpt = 'US_SPF', gamma_est = gamma_estimation, spf_h = h)

        # Update the corresponding columns in the forecasts and evaluation data
        spf_forecasts_cy[[paste0("spf_h", h)]]   <- SPF$spf_forecasts_cy[[paste0("spf_h", h)]]
        spf_forecasts_ny[[paste0("spf_h", h)]]   <- SPF$spf_forecasts_ny[[paste0("spf_h", h)]]
        evaluation_data_cy[[paste0("spf_h", h)]] <- SPF$evaluation_data_cy[[paste0("spf_h", h)]]
        evaluation_data_ny[[paste0("spf_h", h)]] <- SPF$evaluation_data_ny[[paste0("spf_h", h)]]
      }
    } #else if spf_h = h in case we want to read out filtered SPF data calibrated for a specific h only
  }

  return(SPF)
}

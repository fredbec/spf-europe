### Main function to clean consensus and individual forecasts
data_function_spf <- function(ConsensusMedian = TRUE, SPFPanel = FALSE, FixedHorizon = FALSE) {


  ### Read in and clean real GDP
  rgdp_pre  <- read.csv("data/processed/revdatpre14.csv")
  rgdp_post <- read.csv("data/processed/revdatpost14.csv")

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


  ### Read in and clean consensus or panelists forecasts
  if (SPFPanel) {

    # Panel of SPF forecasters
    if (FixedHorizon) {
      # Filter using fixed-event and fixed-horizon forecasts
      IndivSPFall <- read.csv("output/filter_spf/individual/001_run.csv")
    } else {
      # Filter using fixed-event forecasts
      IndivSPFall <- read.csv("output/filter_spf/individual/001_run.csv")
    }

    # Loop over forecast IDs and construct a panel
    ForecastIDs <- unique(IndivSPFall$forecaster_id)
    panel_list1 <- vector("list", length(ForecastIDs))
    panel_list2 <- vector("list", length(ForecastIDs))
    names(panel_list1) <- ForecastIDs
    names(panel_list2) <- ForecastIDs

    for (i in seq_along(ForecastIDs)) {

      fid <- ForecastIDs[i]

      IndivSPF <- IndivSPFall %>%
        filter(forecaster_id == fid) %>%
        #distinct(target_quarter, target_year, # distinct not needed anymore
        #         origin_quarter, origin_year,
        #         .keep_all = TRUE) %>%
        select(-forecaster_id)

      SPF_ind <- .prep_spf_data(IndivSPF, rgdp_all)

      # extract only what is needed
      panel_list1[[i]] <- SPF_ind$evaluation_data_ny %>%
        mutate(forecaster_id = fid)

      panel_list2[[i]] <- SPF_ind$spf_forecasts_ny %>%
        mutate(forecaster_id = fid)
    }

    # Bind into a single panel data set
    SPF_panel_eval <- bind_rows(panel_list1)
    SPF_panel <- bind_rows(panel_list2)

  } else {

    if (ConsensusMedian) {
      # Median forecasts as SPF consensus
      if (FixedHorizon) {
        # Filter using fixed-event and fixed-horizon forecasts
        spf_data <- read.csv("output/filter_spf/inc_fixedhorizon_median/001_run.csv")
      } else {
        # Filter using fixed-event forecasts
        spf_data <- read.csv("output/filter_spf/consensus_median/001_run.csv")
      }


    } else {
      # Mean forecasts as SPF consensus
      if (FixedHorizon) {
        # Filter using fixed-event and fixed-horizon forecasts
        spf_data <- read.csv("output/filter_spf/inc_fixedhorizon_mean/001_run.csv")
      } else {
        # Filter using fixed-event forecasts
        spf_data <- read.csv("output/filter_spf/consensus_mean/001_run.csv")
      }
    }

    # Consensus forecasts
    SPF <- .prep_spf_data(spf_data, rgdp_all)

  }


  # Define output
  if (FixedHorizon) {
    FilterInfo = "FixedEventFixedHorizon"
  } else {
    FilterInfo = "FixedEvent"
  }

  if (SPFPanel) {
    # Panel of SPF forecasters
    SPF <- list(SPF_panel = SPF_panel, SPF_panel_eval = SPF_panel_eval, FilterInfo)
  } else {
    # Consensus forecasts
    if (ConsensusMedian) {
      SPF <- list(SPF_consensus = SPF, Consensus = 'median', FilterInfo = FilterInfo)
    } else {
      SPF <- list(SPF_consensus = SPF, Consensus = 'mean', FilterInfo = FilterInfo)
    }
  }

  return(SPF)

}



# Clean SPF data and construct consensus or panel of SPF forecasts for h=0,1,...,4
.prep_spf_data <- function(spf_data, rgdp_all) {


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

  # Replace NA with forecasts exploiting the two-year-ahead SPF
  spf_data <- spf_data %>%
    mutate(
      spf_filter_cy = coalesce(spf_filter_cy, spf_filter_ny)
    )

  # Reshape forecasts like US SPF format
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
  release <- 1

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

  evaluation_data_cy <- full_join(
    spf_forecasts_cy,
    rgdp_rel %>%
      distinct(ref_period, .keep_all = TRUE),  # keep all GDP vars
    by = "ref_period"
  ) %>%
    arrange(ref_period)


  evaluation_data_ny <- full_join(
    spf_forecasts_ny,
    rgdp_rel %>%
      distinct(ref_period, .keep_all = TRUE),  # keep all GDP vars
    by = "ref_period"
  ) %>%
    arrange(ref_period)



  #evaluation_data_ny <- rgdp_rel %>%
  #  left_join(spf_forecasts_ny, by = "ref_period")

  spf_forecasts_ny$target_year <- target_aux[,2]
  spf_forecasts_ny$target_quarter <- target_aux[,1]
  spf_forecasts_cy$target_year <- target_aux[,2]
  spf_forecasts_cy$target_quarter <- target_aux[,1]



  ### Yearly data for yearly evaluation

  # Yearly SPF forecasts
  spf_yearly <- read.csv("data/processed/spf_median_forecast.csv")

  # Compute yearly GDP growth rates
  rgdp_yearly <- rgdp_all %>%
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


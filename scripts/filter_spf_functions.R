source(here("scripts", "kalman_filter.R"))
source(here("scripts", "kalman_filter_us.R"))
source(here("scripts", "kalman_filter_us_gamma.R"))

get_rtd <- function(real_time_data,
                    current_issue,
                    rtd_issue = c("latest_vintage", "rtd"),
                    vintage_id = NULL){

  #chain operator for data.table
  DT <- `[`


  if(rtd_issue == "latest_vintage"){
    truth_dat <- real_time_data |>
      copy() |>
      DT(origin_year == current_issue$year &
           origin_month == current_issue$month) |>
      #delete redundant info
      DT(, c("origin_year", "origin_month") := NULL)

    #explicitly append NA values for current and next year
    #first generate all NA data.table for current and next year
    na_obs <- CJ(target_year = c(current_issue$year - 1,
                                 current_issue$year,
                                 current_issue$year + 1),
                 target_quarter = 1:4,
                 rgdp_growth = NaN)

    #exclude instances from na_obs that are not NA (as they are in truth_dat)
    na_obs <- na_obs[!truth_dat, on = .(target_year, target_quarter)]

    #append NAs to truth_dat
    truth_dat <- rbind(truth_dat,
                       na_obs)
  } else if (rtd_issue == "rtd") {

    ###Write function for real_time_issues
    truth_dat <- NULL
  } else if (rtd_issue == "issue"){

    ###Write function for particular issue
    truth_dat <- NULL
  }


  return(truth_dat)
}


#shift = 0 will set the month in the mid of the quarter (i.e. May for Q2)
#shift = -1 will get first month in quarter (e.g. April for Q2), shift = 1
#will get last month in quarter (e.g. June for Q2)
get_current_issue <- function(shift,
                              current_quarter,
                              current_year)  {

  qstart_month <- (current_quarter - 1) * 3 + 1
  cmonth <- qstart_month + (shift + 1)

  ciss <- list(month = cmonth, year = current_year)

  return(ciss)

}

filter_dat <- function(current_quarter,
                       current_year,
                       SPF_data,
                       real_time_data,
                       SPF_data_US = NULL,
                       release_US_SPF = "latest",
                       est_gamma = FALSE,
                       rtd_issue = c("latest_vintage", "rtd")){

  if(length(rtd_issue) > 1){
    stop("Choose one of the available options")
  }

  #chain operator for data.table
  DT <- `[`


  #get current issue
  current_issue <- get_current_issue(shift = -1,
                                     current_quarter = current_quarter,
                                     current_year = current_year)

  ###read in real time data
  #currently implemented: expanding window (use all historic data)
  #could also implement a rolling window approach
  truth_dat <- get_rtd(real_time_data = real_time_data,
                       current_issue = current_issue,
                       rtd_issue = rtd_issue) |>
    setorder(target_year, target_quarter)

  ###read in SPF data
  spf_fcs <- SPF_data |>
    DT(forecast_year == current_year &
         forecast_quarter == current_quarter &
         target_year %in% c(current_year, current_year + 1)) |>
    setorder(target_year) |>
    DT(, "ens_fc") |>
    unlist()


  data_filter_cy <- truth_dat |>
    copy() |>
    DT(target_quarter == 4 & target_year == current_year,
       "spf_fc" := spf_fcs[1]) |>
    DT(target_year <= current_year) |>
    DT(is.na(spf_fc), spf_fc := NaN)

  data_filter_cyandny <- truth_dat |>
    copy() |>
    DT(target_quarter == 4 & target_year %in% c(current_year, current_year+1),
       "spf_fc" := spf_fcs) |>
    DT(is.na(spf_fc), spf_fc := NaN)


  if(!is.null(SPF_data_US)){

    if(release_US_SPF == "latest"){
      cpreds_zero <- SPF_data_US |>
        DT(,step_ahead := (target_quarter - origin_quarter) + (target_year - origin_year)*4) |>
        DT(step_ahead == 0 & origin_year <= current_year) |>
        DT(, prev := ifelse(origin_year == current_year & origin_quarter >= current_quarter, 1, 0)) |>
        DT(prev == 0) |>
        DT(, c("prev", "step_ahead", "origin_year", "origin_quarter") := NULL) |>
        setnames("prediction", "rgdp_growth_usspf")

      cpreds_forward <- SPF_data_US |>
        DT(origin_year == current_year & origin_quarter == current_quarter)|>
        DT(, c("step_ahead", "origin_year", "origin_quarter") := NULL) |>
        setnames("prediction", "rgdp_growth_usspf")

      cpreds_current <- rbind(cpreds_zero, cpreds_forward)

      data_filter_cy <- cpreds_current[data_filter_cy, on = c("target_year", "target_quarter")]

      data_filter_cyandny <- cpreds_current[data_filter_cyandny, on = c("target_year", "target_quarter")] |>
        DT(, rgdp_growth_usspf := ifelse(is.na(rgdp_growth_usspf), NaN, rgdp_growth_usspf))
    } else if (is.numeric(release_US_SPF)){

      if(release_US_SPF>3){

        stop("release_US_SPF must be 3 or smaller, longer horizons currently not available")


      }

      #this is for the IP data
      if(release_US_SPF < 0){
        shift_cqu <- release_US_SPF + 1
      } else {
        shift_cqu <- 0
      }
      cpreds_zero <- SPF_data_US |>
        DT(,step_ahead := (target_quarter - origin_quarter) + (target_year - origin_year)*4) |>
        DT(step_ahead == release_US_SPF & origin_year <= current_year) |>
        DT(, prev := ifelse(origin_year == current_year & origin_quarter > (current_quarter + shift_cqu), 1, 0)) |> #important that here we have origin_quarter > current_quarter and not origin_quarter >=  current_quarter (as above)
        DT(prev == 0) |>
        DT(, c("prev", "step_ahead", "origin_year", "origin_quarter") := NULL) |>
        setnames("prediction", "rgdp_growth_usspf")

      data_filter_cy <- cpreds_zero[data_filter_cy, on = c("target_year", "target_quarter")]

      data_filter_cyandny <- cpreds_zero[data_filter_cyandny, on = c("target_year", "target_quarter")] |>
        DT(, rgdp_growth_usspf := ifelse(is.na(rgdp_growth_usspf), NaN, rgdp_growth_usspf))

    } else {
    stop("release_US_SPF must either be 'latest' or numeric")
  }
  }

  if(is.null(SPF_data_US)){
    cyres <- SPF_filter(data_filter_cy$rgdp_growth, data_filter_cy$spf_fc)
    spf_filter_vals_cy <- cyres$SPF_filtered
    cyandnyres <- SPF_filter(data_filter_cyandny$rgdp_growth, data_filter_cyandny$spf_fc)
    spf_filter_vals_cyandny <- cyandnyres$SPF_filtered
  } else {
    #following code is a bit adhoc, since there was an error in the instance 2018Q1
    #with the Cholesky decomposition
    if(!est_gamma){
      cyres <- tryCatch(
        {
          SPF_filter_us(data_filter_cy$rgdp_growth, data_filter_cy$spf_fc, data_filter_cy$rgdp_growth_usspf)
        },
        error = function(e){
          message(paste0(current_year, "Q", current_quarter))

          data_filter_cy[, 3] <- data_filter_cy[, 3] - rnorm(1, sd = 0.0001)
          SPF_filter_us(data_filter_cy$rgdp_growth, data_filter_cy$spf_fc, data_filter_cy$rgdp_growth_usspf)
        }
      )
      spf_filter_vals_cy <- cyres$SPF_filtered
      #spf_filter_vals_cyandny <- spf_filter_vals_cy
      cyandnyres <- SPF_filter_us(data_filter_cyandny$rgdp_growth, data_filter_cyandny$spf_fc, data_filter_cyandny$rgdp_growth_usspf)
      spf_filter_vals_cyandny <- cyandnyres$SPF_filtered
    } else {

      cyres <- tryCatch(
        {
          SPF_filter_us_gamma(data_filter_cy$rgdp_growth, data_filter_cy$spf_fc, data_filter_cy$rgdp_growth_usspf)
        },
        error = function(e){
          message(paste0(current_year, "Q", current_quarter))

          data_filter_cy[, 3] <- data_filter_cy[, 3] - rnorm(1, sd = 0.0001)
          SPF_filter_us_gamma(data_filter_cy$rgdp_growth, data_filter_cy$spf_fc, data_filter_cy$rgdp_growth_usspf)
        }
      )
      spf_filter_vals_cy <- cyres$SPF_filtered
      #spf_filter_vals_cyandny <- spf_filter_vals_cy
      cyandnyres <- SPF_filter_us_gamma(data_filter_cyandny$rgdp_growth, data_filter_cyandny$spf_fc, data_filter_cyandny$rgdp_growth_usspf)
      spf_filter_vals_cyandny <- cyandnyres$SPF_filtered

    }

  }

  #make spf_filter_dat (take target columns from data_filter_cyandny)
  spf_filter_dat <- data_filter_cyandny |>
    DT(, .SD, .SDcols = c("target_quarter", "target_year")) |>
    DT(, spf_filter_cy := c(spf_filter_vals_cy, rep(NA, 4))) |>
    DT(, spf_filter_ny := spf_filter_vals_cyandny) |>
    DT(, origin_quarter := current_quarter) |>
    DT(, origin_year := current_year)

  return(list(spf_filter_dat = spf_filter_dat, ll = c(cy = cyres$logLik, cyandny = cyandnyres$logLik), params = list(cy = cyres$par_est, cyandny=cyandnyres$par_est)))

}


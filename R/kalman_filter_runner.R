source(here("R", "kalman_filter_core", "kalman_filter_us.R"))
source(here("R", "kalman_filter_core", "kalman_filter_us_gamma.R"))


get_rtd <- function(real_time_data,
                    current_issue,
                    current_year,
                    rtd_issue = c("latest_vintage", "rtd"),
                    vintage_id = NULL){

  #chain operator for data.table
  DT <- `[`


  if(rtd_issue == "latest_vintage"){
    truth_dat <- real_time_data |>
      copy() |>
      DT(origin_year == lubridate::year(current_issue) &
           origin_month == lubridate::month(current_issue) &
           origin_day == lubridate::day(current_issue)) |>
      #delete redundant info
      DT(, c("origin_year", "origin_month", "origin_day") := NULL)

    #explicitly append NA values for current and next year
    #first generate all NA data.table for current and next year
    na_obs <- CJ(target_year = c(current_year - 1,
                                 current_year,
                                 current_year + 1),
                 target_quarter = 1:4,
                 rgdp_growth = NaN)

    #exclude instances from na_obs that are not NA (as they are in truth_dat)
    na_obs <- na_obs[!truth_dat, on = .(target_year, target_quarter)]
    na_obs <- cbind(na_obs,NA_real_)
    colnames(na_obs)[4] <- "rgdp_growth_ann"

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
                              rtd_match_data,
                              current_quarter,
                              current_year)  {

  origin_quarter <- current_quarter - shift

  if(origin_quarter == 0){
    filter_quarter <- 4
    filter_year <- current_year - 1
  } else {
    filter_quarter <- current_quarter
    filter_year <- current_year
  }


  rtd_date <- rtd_match_data |>
    DT(origin_year == filter_year & origin_quarter == filter_quarter)

  rtd_date <- rtd_date$closest_rtd_release

  return(rtd_date)

}

filter_dat <- function(current_quarter,
                       current_year,
                       SPF_data,
                       real_time_data,
                       SPF_fixedhorizon,
                       est_gamma = FALSE,
                       rtd_issue = c("latest_vintage", "rtd"),
                       rtd_shift = 0,
                       rtd_match_data,
                       approx_err = 0.01,
                       monthly = FALSE){

  if(length(rtd_issue) > 1){
    stop("Choose one of the available options")
  }

  #chain operator for data.table
  DT <- `[`


  #get current issue
  current_issue <- get_current_issue(shift = rtd_shift,
                                     rtd_match_data = rtd_match_data,
                                     current_quarter = current_quarter,
                                     current_year = current_year)

  approx_errors <- fread(here("data", "processed", "approximation_errors.csv"))
  approx_errors_monthly <- fread(here("data", "processed", "approximation_errors_ip.csv"))
  ###read in real time data
  #currently implemented: expanding window (use all historic data)
  #could also implement a rolling window approach
  truth_dat <- get_rtd(real_time_data = real_time_data,
                       current_issue = current_issue,
                       current_year = current_year,
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



  # approximation error to observed data
  if (is.null(approx_err)) {
      cissue_approx_errors <- approx_errors |>
        DT(origin_year == lubridate::year(current_issue) & origin_month == lubridate::month(current_issue) & lubridate::day(current_issue))
      max_ind <- max(which(!is.na(cissue_approx_errors$sigma_err)))

      approx_err <- cissue_approx_errors$sigma_err[max_ind]

      approx_err_fh <- cissue_approx_errors$sigma_qonq_err[max_ind]

      if(monthly){
        cissue_approx_err_monthly <- approx_errors_monthly |>
          DT(target_year == lubridate::year(current_issue) & target_quarter == lubridate::quarter(current_issue))

        if(nrow(cissue_approx_err_monthly) == 0){
          stop("no approx error available at monthly frequency")
        }

        approx_err_quarterly<- cissue_approx_err_monthly$sigma_err
      }

  } else {
    # Could be an input to the function
    approx_err_fh <- approx_err
  }

  if (!is.null(SPF_fixedhorizon)){
    fixedhorizon_fc <- SPF_fixedhorizon |>
      DT(forecast_year == current_year & forecast_quarter == current_quarter)

    fixedhorizon_fc <- fixedhorizon_fc$ens_fc

    quarterID <- ((current_quarter + 2 - 1) %% 4) + 1
    cyres <- SPF_filter_fe_fh(rgdp = data_filter_cy$rgdp_growth,
                              spf = data_filter_cy$spf_fc,
                              spfFixHor = fixedhorizon_fc,
                              QuarterID = quarterID,
                              approxerr = c(approx_err,approx_err_fh) )
    spf_filter_vals_cy <- cyres$SPF_filtered
    cyandnyres <- SPF_filter_fe_fh(rgdp = data_filter_cyandny$rgdp_growth,
                                   spf = data_filter_cyandny$spf_fc,
                                   spfFixHor = fixedhorizon_fc,
                                   QuarterID = quarterID,
                                   approxerr = c(approx_err,approx_err_fh) )
    spf_filter_vals_cyandny <- cyandnyres$SPF_filtered

  } else {
    if(monthly){
      cyres <- SPF_filter_monthly(data_filter_cy$rgdp_growth, data_filter_cy$spf_fc,
                                  approx_err = approx_err, approx_err_quarterly = approx_err_quarterly)
      spf_filter_vals_cy <- cyres$SPF_filtered
      spf_filter_vars_cy <- cyres$SPF_filtered_var
      cyandnyres <- SPF_filter_monthly(data_filter_cyandny$rgdp_growth, data_filter_cyandny$spf_fc,
                                       approx_err = approx_err, approx_err_quarterly = approx_err_quarterly)
      spf_filter_vals_cyandny <- cyandnyres$SPF_filtered
      spf_filter_vars_cyandny <- cyandnyres$SPF_filtered_var
    } else {

      cyres <- SPF_filter(data_filter_cy$rgdp_growth, data_filter_cy$spf_fc,
                          approx_err = approx_err)
      spf_filter_vals_cy <- cyres$SPF_filtered
      spf_filter_vars_cy <- cyres$SPF_filtered_var
      cyandnyres <- SPF_filter(data_filter_cyandny$rgdp_growth, data_filter_cyandny$spf_fc,
                               approx_err = approx_err)
      spf_filter_vals_cyandny <- cyandnyres$SPF_filtered
      spf_filter_vars_cyandny <- cyandnyres$SPF_filtered_var

    }
  }
  #make spf_filter_dat (take target columns from data_filter_cyandny)
  if(monthly){

    setorder(data_filter_cyandny, target_year, target_quarter)
    data_filter_cyandny <- data_filter_cyandny[, target_month := target_quarter * 3L]
    start <- data_filter_cyandny[1, target_year * 12 + (target_quarter - 1L) * 3L + 1L]
    end   <- data_filter_cyandny[.N, target_year * 12 + target_month]

    grid <- CJ(target_year  = unique(data_filter_cyandny$target_year),
               target_month = 1:12)
    grid <- grid[between(target_year * 12 + target_month, start, end)]

    data_filter_cyandny <- data_filter_cyandny[grid, on = .(target_year, target_month)]
    data_filter_cyandny<- data_filter_cyandny[, target_quarter := (target_month - 1L) %/% 3L + 1L]
    setcolorder(data_filter_cyandny, c("target_year", "target_month", "target_quarter"))

    repc <- 12
    sdcols <- c("target_month", "target_year")
  } else {
    repc <- 4
    sdcols <- c("target_quarter", "target_year")
  }
  spf_filter_dat <- data_filter_cyandny |>
    DT(, .SD, .SDcols = sdcols) |>
    DT(, spf_filter_cy := c(spf_filter_vals_cy, rep(NA, repc))) |>
    DT(, spf_var_cy := c(spf_filter_vars_cy, rep(NA, repc))) |>
    DT(, spf_filter_ny := spf_filter_vals_cyandny) |>
    DT(, spf_var_ny := spf_filter_vars_cyandny) |>
    DT(, origin_quarter := current_quarter) |>
    DT(, origin_year := current_year)


  return(list(spf_filter_dat = spf_filter_dat, ll = c(cy = cyres$logLik, cyandny = cyandnyres$logLik), params = list(cy = cyres$par_est, cyandny=cyandnyres$par_est)))

}



run_filter <- function(combs,
                       SPF_data,
                       real_time_data,
                       SPF_fixedhorizon,
                       est_gamma,
                       rtd_issue  ="latest_vintage",
                       rtd_shift,
                       rtd_match_data,
                       approx_err,
                       monthly){



  res_spf_filter <- vector(mode = "list", length = nrow(combs))
  res_spf_additionalinfo <- vector(mode = "list", length = nrow(combs))
  for(i in 1:nrow(combs)){

    cissue <- combs[i,]

    cqu <- cissue$quarter
    cyr <- cissue$year

    res <- filter_dat(current_quarter = cqu,
                      current_year = cyr,
                      SPF_data = SPF_data,
                      real_time_data = real_time_data,
                      SPF_fixedhorizon = SPF_fixedhorizon,
                      est_gamma = est_gamma,
                      rtd_issue = rtd_issue,
                      rtd_shift = rtd_shift,
                      rtd_match_data = rtd_match_data,
                      approx_err = approx_err,
                      monthly = monthly)


    res_spf_filter[[i]] <- res$spf_filter_dat
    res_spf_additionalinfo[[i]] <- data.table(origin_quarter = cqu,
                                              origin_year = cyr,
                                              cy_logLik = res$ll["cy"],
                                              cyandny_logLik = res$ll["cyandny"],
                                              cy_rw_sd = res$params$cy["rw_sd"],
                                              cyandny_rw_sd = res$params$cyandny["rw_sd"])
  }
  res_spf_filter <- rbindlist(res_spf_filter)
  res_spf_additionalinfo <- rbindlist(res_spf_additionalinfo)

  return(list(filter_output = res_spf_filter,
              metadata = res_spf_additionalinfo))


}


run_filter_from_settings <- function(settings){

  SPF_data <- data.table::fread(here("data", "processed", settings$data_input_primary))

  if(!is.null(settings$data_input_secondary)){
    SPF_fixedhorizon <- data.table::fread(here("data", "processed", settings$data_input_secondary))
  } else {
    SPF_fixedhorizon <- NULL
  }

  real_time_data <- fread(here("data", "processed", "revdatfull.csv")) |>
    DT(is.na(rgdp_growth), rgdp_growth := NaN) |>
    DT(is.na(rgdp_growth_ann), rgdp_growth_ann := NaN) |>
    DT(, imputed := NULL)

  spf_deadlines_match <- fread(here("data", "helpers", "spf_deadlines_match_rtd.csv"))

  #make a combination of all years and quarters, to loop over
  quarters <- 1:4
  years <- settings$start_year:settings$end_year

  combs <- CJ(year = years,
              quarter = quarters)


  if(is.null(SPF_data$forecaster_id)){
    filter_result <- run_filter(
      combs = combs,
      SPF_data = SPF_data,
      real_time_data = real_time_data,
      SPF_fixedhorizon = SPF_fixedhorizon,
      est_gamma = settings$gamma_est,
      rtd_issue = "latest_vintage",
      rtd_shift = settings$rtd_shift,
      rtd_match_data = spf_deadlines_match,
      approx_err = settings$approx_error,
      monthly = settings$monthly
    )
  } else {

    if(grepl("fixedhorizon", settings$spec_id)){

      SPF_fixedhorizon <- SPF_data |>
        copy() |>
        DT(type_target == "quarterly") |>
        DT(, type_target := NULL) |>
        split(by = "forecaster_id")

      SPF_data <- SPF_data |>
        DT(type_target == "annual")|>
        DT(, .SD, .SDcols = c("forecaster_id",
                              "target_year",
                              "forecast_year",
                              "forecast_quarter",
                              "ens_fc"))
    } else{

      SPF_fixedhorizon <- NULL
    }

    SPF_data <- split(SPF_data, by = "forecaster_id")

    #get only combs for which forecaster id has submitted
    fcid_combs <- lapply(SPF_data, function(fcdat){

      fcdat_sub <- fcdat |>
        DT(, .SD, .SDcols = c("forecast_year", "forecast_quarter")) |>
        unique()|>
        setnames(c("forecast_year", "forecast_quarter"), c("year", "quarter"))

      combs_sub <- merge(combs, fcdat_sub, by = c("year", "quarter"), all = FALSE)

      return(combs_sub)
    })


    filter_result <- lapply(
      SPF_data,
      function(spf_dat_indiv){

        fcid <- unique(spf_dat_indiv$forecaster_id)

        run_filter(
          combs = fcid_combs[[as.character(fcid)]],
          SPF_data = spf_dat_indiv,
          real_time_data = real_time_data,
          SPF_fixedhorizon = SPF_fixedhorizon[[as.character(fcid)]],
          SPF_data_US = SPF_data_US,
          release_US_SPF = settings$release_US_SPF,
          est_gamma = settings$gamma_est,
          rtd_issue = "latest_vintage",
          rtd_shift = settings$rtd_shift,
          rtd_match_data = spf_deadlines_match,
          approx_err = settings$approx_error,
          monthly = settings$monthly
        ) |>
          lapply(function(dat){
            dat |>
              DT(, forecaster_id := fcid)
          })
      })
    temp <- vector(mode = "list", length = 2)
    temp[[1]] <- rbindlist(lapply(filter_result, `[[`, 1))
    temp[[2]] <- rbindlist(lapply(filter_result, `[[`, 2))

    filter_result <- list(filter_output = temp[[1]],
                          metadata = temp[[2]])
  }

  return(filter_result)
}

na_to_null <- function(x) if (is.na(x)) NULL else x

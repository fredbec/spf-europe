source(here("scripts", "kalman_filter.R"))
source(here("scripts", "kalman_filter_us.R"))
source(here("scripts", "kalman_filter_us_gamma.R"))
source(here("scripts", "kalman_filter_fe_fh.R"))

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
                       SPF_data_US = NULL,
                       release_US_SPF = "latest",
                       est_gamma = FALSE,
                       rtd_issue = c("latest_vintage", "rtd"),
                       rtd_shift = 0,
                       rtd_match_data,
                       approx_err = 0.01){

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

      data_filter_cy <- cpreds_zero[data_filter_cy, on = c("target_year", "target_quarter")] |>
        DT(, rgdp_growth_usspf := ifelse(is.na(rgdp_growth_usspf), NaN, rgdp_growth_usspf))

      data_filter_cyandny <- cpreds_zero[data_filter_cyandny, on = c("target_year", "target_quarter")] |>
        DT(, rgdp_growth_usspf := ifelse(is.na(rgdp_growth_usspf), NaN, rgdp_growth_usspf))

    } else {
    stop("release_US_SPF must either be 'latest' or numeric")
  }
  }

  if(is.null(SPF_data_US)){

    # Calibration of approximation error to observed data (Code should be improved)
    if (is.null(approx_err)) {
      test = cbind(data_filter_cy$rgdp_growth,data_filter_cy$rgdp_growth_ann)

      if (all(is.na(test[,2]))) {
        approx_err = 0.1
        print("Warning: No annual growth rates!")
      } else {
        last_valid <- max(max(which(!is.na(test[,2]))), dim(test)[1])
        test <- test[1:last_valid, ]

        n <- nrow(test)
        rem <- n %% 4

        if (rem != 0) {
          test <- test[(rem + 1):n, ]
        }

        test = cbind(test,NaN)
        for (i in 8:dim(test)[1]) {
          test[i,3] = 1/16 * (test[i,1] + 2*test[i-1,1] + 3*test[i-2,1] + 4*test[i-3,1] +
                                + 3*test[i-4,1] +  + 2*test[i-5,1] + 2*test[i-6,1])

        }
        ApprErr <- test[,2] - test[,3]
        approx_err <- sd(ApprErr, na.rm = TRUE)
      }
    }

    if(!is.null(SPF_fixedhorizon)){
      fixedhorizon_fc <- SPF_fixedhorizon |>
        DT(forecast_year == current_year & forecast_quarter == current_quarter)

      fixedhorizon_fc <- fixedhorizon_fc$ens_fc
      print(fixedhorizon_fc)

      quarterID <- ((current_quarter + 2 - 1) %% 4) + 1
      message("here")
      cyres <- SPF_filter_fe_fh(rgdp = data_filter_cy$rgdp_growth,
                                spf = data_filter_cy$spf_fc,
                                spfFixHor = fixedhorizon_fc,
                                QuarterID = quarterID,
                                approx_err = approx_err)
      spf_filter_vals_cy <- cyres$SPF_filtered
      message("here")
      cyandnyres <- SPF_filter_fe_fh(rgdp = data_filter_cyandny$rgdp_growth,
                                     spf = data_filter_cyandny$spf_fc,
                                     spfFixHor = fixedhorizon_fc,
                                     QuarterID = quarterID,
                                     approx_err = approx_err)
      spf_filter_vals_cyandny <- cyandnyres$SPF_filtered

    } else {
      cyres <- SPF_filter(data_filter_cy$rgdp_growth, data_filter_cy$spf_fc, approx_err = approx_err)
      spf_filter_vals_cy <- cyres$SPF_filtered
      cyandnyres <- SPF_filter(data_filter_cyandny$rgdp_growth, data_filter_cyandny$spf_fc, approx_err = approx_err)
      spf_filter_vals_cyandny <- cyandnyres$SPF_filtered
    }
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



run_filter <- function(combs,
                       SPF_data,
                       real_time_data,
                       SPF_fixedhorizon,
                       SPF_data_US,
                       release_US_SPF = "latest",
                       est_gamma,
                       rtd_issue  ="latest_vintage",
                       rtd_shift,
                       rtd_match_data,
                       approx_err){



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
                      SPF_data_US = SPF_data_US,
                      release_US_SPF = release_US_SPF,
                      est_gamma = est_gamma,
                      rtd_issue = rtd_issue,
                      rtd_shift = rtd_shift,
                      rtd_match_data = rtd_match_data,
                      approx_err = approx_err)


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
    if(grepl("fixedhorizon", settings$spec_id)){
      SPF_fixedhorizon <- data.table::fread(here("data", "processed", settings$data_input_secondary))
      SPF_data_US <- NULL
    } else {
      SPF_data_US <- data.table::fread(here("data", "processed", settings$data_input_secondary))
      SPF_fixedhorizon <- NULL
    }
  } else {
    SPF_data_US <- NULL
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
      SPF_data_US = SPF_data_US,
      release_US_SPF = settings$release_US_SPF,
      est_gamma = settings$gamma_est,
      rtd_issue = "latest_vintage",
      rtd_shift = settings$rtd_shift,
      rtd_match_data = spf_deadlines_match,
      approx_err = settings$approx_error
    )
  } else {

    SPF_data <- SPF_data |>
      DT(type_format == "POINT") |>
      DT(type_target == "annual") |>
      DT(forecast_year >= 2001) |>
      setnames("prediction", "ens_fc") #for compatibility

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
          SPF_data_US = SPF_data_US,
          release_US_SPF = settings$release_US_SPF,
          est_gamma = settings$gamma_est,
          rtd_issue = "latest_vintage",
          rtd_shift = settings$rtd_shift,
          rtd_match_data = spf_deadlines_match,
          approx_err = settings$approx_error
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

read_spec <- function(spec_id) {
  path <- here("output", "filter_spf", spec_id, "specs.csv")
  stopifnot(file.exists(path))
  spec <- read.csv(path, stringsAsFactors = FALSE)
  as.list(setNames(spec$value, spec$key))
}

read_run <- function(spec_id, run_id) {
  path <- here("output", "filter_spf", spec_id, "runs.csv")
  runs <- read.csv(path, stringsAsFactors = FALSE)
  run <- runs[runs$run_id == run_id, ]
  stopifnot(nrow(run) == 1)
  as.list(run)
}

merge_reformat_settings <- function(spec, run) {
  c(spec, run) |>
    lapply(na_to_null)

}

write_outputs <- function(results, settings, base_path = "output/filter_spf") {

  #  Build directory path
  spec_dir <- here("output", "filter_spf", settings$spec_id)

  # Define base filename prefix
  prefix <- sprintf("%03d", settings$run_id)
  #name <- paste0(settings$name)

  # Write filtered results
  if (!is.null(results$filter_output)) {
    filtered_file <- here(spec_dir, paste0(prefix, "_run", ".csv"))
    data.table::fwrite(results$filter_output, filtered_file)
  }

  # Write estimated parameters / metadata
  if (!is.null(results$metadata)) {
    meta_file <- here(spec_dir, paste0(prefix,"_metadata.csv"))
    data.table::fwrite(results$metadata, meta_file)


  }

  # Optional message
  message("Outputs saved for ", settings$spec_id, " / ", settings$run_id)
}

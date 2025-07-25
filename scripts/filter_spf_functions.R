source(here("scripts", "kalman_filter.R"))
source(here("scripts", "kalman_smoother.R"))

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


  spf_filter_vals_cy <- SPF_filter(data_filter_cy$rgdp_growth, data_filter_cy$spf_fc)
  spf_filter_vals_cyandny <- SPF_filter(data_filter_cyandny$rgdp_growth, data_filter_cyandny$spf_fc)

  #make spf_filter_dat (take target columns from data_filter_cyandny)
  spf_filter_dat <- data_filter_cyandny |>
    DT(, .SD, .SDcols = c("target_quarter", "target_year")) |>
    DT(, spf_filter_cy := c(spf_filter_vals_cy, rep(NA, 4))) |>
    DT(, spf_filter_ny := spf_filter_vals_cyandny) |>
    DT(, origin_quarter := current_quarter) |>
    DT(, origin_year := current_year)

  return(spf_filter_dat)

}


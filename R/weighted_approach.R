#' Weight calculator for the approach outlined in Knüppel and Vladu (2024)
#'
#' Computes the weights for an optimal combination of the current-year
#' and next-year forecasts
#'
#' @param Sigma A covariance matrix
#' @param B1
#' @param B2
#' @param An
#'
#' @return A (2x1) vector, first entry corresponds to the current-year forecast

w_opt <- function(Sigma,
                  B1,
                  B2,
                  An
){
  #dimension checks
  stopifnot(all(dim(Sigma) == c(12,12)))
  stopifnot(length(B1) == 12)
  stopifnot(length(B2) == 12)
  stopifnot(length(An) == 12)

  M <- t(An - B2)
  N <- t(B2 - B1)

  numer <- - M %*% Sigma %*% t(N)
  denom <- N %*% Sigma %*% t(N)

  if(abs(denom) < 1e-10) stop("denominator near zero")

  res <- numer/denom

  return(res)

}

w_calc <- function(t_now,
                   fc_horizon,
                   G,
                   Sigma
){
  #dimension checks
  if(fc_horizon != as.integer(fc_horizon)){stop("fc_horizon has to be integer")}
  if(t_now != as.integer(t_now)){stop("t_now has to be integer")}

  ###value range for fc_horizon
  if(!t_now %in% 1:4){
    stop("t_now has to be in the current year")
  }
  if(t_now + fc_horizon > 8){
    stop("the period to be forecast has to lie in current or next year, check
         values of t_now and fc_hor (have to add up to 8 or below)")
  }

  pos_A <- (8 - (t_now + fc_horizon)) + 1
  An <- numeric(12)
  An[(pos_A):(pos_A + fc_horizon - 1)] <- 4/fc_horizon

  B1 <- c(0,0,0,0,seq(0.25, 1, by = 0.25),seq(0.75, 0.25, by = -0.25),0)
  B2 <- c(seq(0.25, 1, by = 0.25),seq(0.75, 0.25, by = -0.25),0,0,0,0,0)

  weights <- w_opt(Sigma,B1,B2,An)

  return(weights)

}


#' Weight calculator for the approach outlined in Knüppel and Vladu (2024)
#'
#' Computes the weights for an optimal combination of the current-year
#' and next-year forecasts
#'
#' @param G_hist vector with all previous quarterly observations
#' @param p AR order assumed for DGP
#' @param t_now current quarter, first quarter of the current year is coded as 1
#' @param fc_horizon fixed horizon value, relative to current quarter
#' @param lastqu_shift last observed growth rate, relative to current quarter, -2 by
#' default (last known quarter is 2 quarters prior)
#'
#' @return A (2x1) vector, first entry corresponds to the current-year forecast
estimate_weights <- function(G_hist,
                             p,
                             t_now,
                             fc_horizon,
                             lastqu_shift = -2){

  if(p > 1){
    stop("method not implemented yet for p > 1")
  }

  last_g <- t_now + lastqu_shift

  #demean series
  G_dm <- G_hist - mean(G_hist)

  #estimate or set Sigma
  if(p == 0){
    Sigma <- rbind(
      cbind(matrix(0, 8-last_g, 8-last_g), matrix(0, 8-last_g, 4+last_g)),
      cbind(matrix(0, 4+last_g, 8-last_g), diag(4+last_g))
    )

    #extract last- and possibly current-year observations from G
    obsid_G <- t_now + lastqu_shift + 4
    G <- rep(0, 12)
    G[((12-obsid_G)+1):12] <- G_hist[1:obsid_G]

  } else if(p == 1){

    #estimate AR model (on full history, demeaned series)
    ar_fit <- arima(G_dm, order = c(1,0,0))
    phi <- ar_fit$coef["ar1"]
    sigma2_eps <- ar_fit$sigma2

    Sigma <- build_sigma_ar1(phi, sigma2_eps, t_now, lastqu_shift)

    #extract last- and possibly current-year observations from G
    obsid_G <- t_now + lastqu_shift + 4 #"from the back"
    rev_obsid_G <- 12 - obsid_G #"from the front"
    G <- rep(NA, 12)
    G[(rev_obsid_G+1):12] <- G_hist[1:obsid_G]
    G[1:rev_obsid_G] <- phi^(rev_obsid_G:1)*G_hist[1]
  }

  wopt <- w_calc(t_now, fc_horizon, G, Sigma)

  return(wopt)
}

#' Small helper function to construct covariance matrix for an AR(1) process
#' @return A matrix
cov_ar1_block <- function(n_rows, startexp, endexp, phi){

  sapply(
    seq_len(n_rows),
    function(id){
      exponent <- abs(seq((startexp - id)+1, (endexp - id)))
      return(phi^exponent)
    }) |> t()
}

build_sigma_ar1 <- function(phi, sigma2_eps, t_now, lastqu_shift){

  gamma0 <- sigma2_eps / (1 - phi^2)

  n_fc  <- (8 - t_now) - lastqu_shift
  n_real <- 12 - n_fc

  Sigma11 <- cov_ar1_block(n_fc, 2*n_fc, n_fc+2, phi)
  Sigma12 <- cov_ar1_block(n_fc, n_fc, n_real+n_fc, phi)
  Sigma22 <- cov_ar1_block(n_real, 0, n_real, phi)

  Sigma <- rbind(
    cbind(Sigma11, Sigma12),
    cbind(t(Sigma12), Sigma22)
  )

  gamma0 * Sigma
}



fixedhor_forecasts <- function(real_time_data,
                               SPF_data,
                               rtd_match_data,
                               current_year,
                               current_quarter,
                               fc_horizon,
                               ar_order = 1){

  DT <- `[`


  rtd_date <- rtd_match_data |>
    DT(origin_year == current_year & origin_quarter == current_quarter)
  rtd_date <- rtd_date$closest_rtd_release

  SPF_release <- SPF_data |>
    DT(forecast_year == current_year & forecast_quarter == current_quarter) |>
    DT(order(target_year))

  if(length(SPF_release$ens_fc) != 2){
    stop("Expected exactly two SPF forecasts for current/next year")
  }
  SPF_current <- SPF_release$ens_fc[1]
  SPF_next <- SPF_release$ens_fc[2]

  rtd_current <- real_time_data |>
    DT(origin_year == lubridate::year(rtd_date) &
         origin_month == lubridate::month(rtd_date) &
         origin_day == lubridate::day(rtd_date)) |>
    DT(order(target_year, target_quarter))

  last_available_quarter <- rtd_current[!is.na(rgdp_growth), .(target_year, target_quarter)][.N]

  #calculate lastqu_shift
  lastqu_shift <- (last_available_quarter$target_year * 4 + last_available_quarter$target_quarter) -
    (current_year * 4 + current_quarter)
  if(lastqu_shift != -2){
    warning("Last available quarter is not two quarters prior; check data")
  }

  G_hist <- rtd_current$rgdp_growth[!is.na(rtd_current$rgdp_growth)]

  weight_current <- estimate_weights(G_hist = G_hist,
                                     p = ar_order,
                                     t_now = current_quarter,
                                     fc_horizon = fc_horizon,
                                     lastqu_shift = lastqu_shift)

  weight_next <- 1-weight_current

  fh_forecast <- weight_current * SPF_current + weight_next * SPF_next

  return(fh_forecast)
}



run_fixed_hor_forecasts <-
  function(combs,
           SPF_data,
           real_time_data,
           rtd_match_data,
           ar_order){

  res_fixed_hor <- vector(mode = "list", length = nrow(combs))
  for(i in 1:nrow(combs)){

    cissue <- combs[i,]

    cqu <- cissue$quarter
    cyr <- cissue$year
    fc_hor <- cissue$horizon

    res <- fixedhor_forecasts(
      real_time_data = real_time_data,
      SPF_data = SPF_data,
      rtd_match_data = rtd_match_data,
      current_year = cyr,
      current_quarter = cqu,
      fc_horizon = fc_hor,
      ar_order = ar_order)


    res_fixed_hor[[i]] <- res
  }
  res_fixed_hor <- rbindlist(res_fixed_hor)

  return(res_fixed_hor)
}


run_fixed_hor_forecasts_from_settings <- function(settings){

  SPF_data <- data.table::fread(here("data", "processed", settings$data_input_primary))

  real_time_data <- fread(here("data", "processed", "revdatfull.csv"))

  spf_deadlines_match <- fread(here("data", "helpers", "spf_deadlines_match_rtd.csv"))

  ar_order <- settings$ar_order
  #make a combination of all years and quarters, to loop over
  quarters <- 1:4
  years <- settings$start_year:settings$end_year
  fc_horizons <- 1:7

  combs <- CJ(year = years,
              quarter = quarters,
              horizon = fc_horizons) |>
    DT(quarter+horizon <= 8)

  if(is.null(SPF_data$forecaster_id)){
    fh_results <- run_fixed_hor_forecasts(
      combs = combs,
      SPF_data = SPF_data,
      real_time_data = real_time_data,
      rtd_match_data = spf_deadlines_match,
      ar_order = ar_order
    )
  } else {
  ##IMPLEMENT
  }

  return(fh_results)
}

#' Weight calculator for the approach outlined in Knüppel and Vladu (2024)
#'
#' Computes the weights for an optimal combination of the current-year
#' and next-year forecasts
#'
#' @param G A Tx1 vector of quarterly observations
#' @param Sigma A covariance matrix
#' @param B1
#' @param B2
#' @param An
#'
#' @return A (2x1) vector, first entry corresponds to the current-year forecast

w_opt <- function(G,
                  Sigma,
                  B1,
                  B2,
                  An
){
  #dimension checks
  if(!length(G) == 12){
    stop(paste0("G has the wrong number of observations, expected 12, got ", (length(G))))
  }
  if(!(nrow(Sigma) == 12 & ncol(Sigma) == 12)){
    stop(paste0("Sigma has the wrong dimension, expected 12x12"))
  }
  if(!length(B1) == 12){stop(paste0("B1 has the wrong dimension, expected 12x1"))}
  if(!length(B2) == 12){stop(paste0("B2 has the wrong dimension, expected 12x1"))}
  if(!length(An) == 12){stop(paste0("An has the wrong dimension, expected 12x1"))}

  M <- t(An - B2)
  N <- t(B2 - B1)

  numer <- - M %*% Sigma %*% t(N)
  denom <- N %*% Sigma %*% t(N)

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
  An <- rep(0, 12)
  An[(pos_A):(pos_A + fc_horizon - 1)] <- 4/fc_horizon

  B1 <- c(0,0,0,0,seq(0.25, 1, by = 0.25),seq(0.75, 0.25, by = -0.25),0)
  B2 <- c(seq(0.25, 1, by = 0.25),seq(0.75, 0.25, by = -0.25),0,0,0,0,0)

  weights <- w_opt(G,Sigma,B1,B2,An)

}


#' Weight calculator for the approach outlined in Knüppel and Vladu (2024)
#'
#' Computes the weights for an optimal combination of the current-year
#' and next-year forecasts
#'
#' @param real_time_dat data frame with quarterly observations
#' @param p AR order assumed for DGP
#' @param t_now current quarter, first quarter of the current year is coded as 1
#' @param fc_horizon fixed horizon value, relative to current quarter
#' @param rtd_shift last observed growth rate, relative to current quarter, -2 by
#' default (last known quarter is 2 quarters prior)
#'
#' @return A (2x1) vector, first entry corresponds to the current-year forecast
placeholder <- function(G,
                        p,
                        t_now,
                        fc_horizon,
                        rtd_shift = -2){

  if(p > 1){
    stop("method not implemented yet for p > 1")
  }

  last_g <- t_now + rtd_shift

  #estimate or set Sigma
  if(p == 0){
    Sigma <- rbind(
      cbind(matrix(0, 8-last_g, 8-last_g), matrix(0, 8-last_g, 4+last_g)),
      cbind(matrix(0, 4+last_g, 8-last_g), diag(4+last_g))
    )
  } else if(p == 1){

    ar_fit <- ar(G, aic = FALSE, order.max = p)
    phi <- ar_fit$ar
    n_fc <- (8 - t_now) - rtd_shift
    n_real <- length(G) - n_fc

    Sigma11 <- Sigma_AR1(n_fc, 2*n_fc, n_fc+2, phi)
    Sigma12 <- Sigma_AR1(n_fc, n_fc, n_real+n_fc, phi)
    Sigma22 <- Sigma_AR1(n_real, 0, n_real, phi)

    Sigma <- rbind(
      cbind(Sigma11, Sigma12),
      cbind(t(Sigma12), Sigma22)
    )
  }

  wopt <- w_calc(t_now, fc_horizon, G, Sigma)

  return(wopt)
}

#' Small helper function to construct covariance matrix for an AR(1) process
#' @return A matrix
Sigma_AR1 <- function(nrow, startexp, endexp, phi){

  sapply(
    seq_len(nrow),
    function(id){
      exponent <- abs(seq((startexp - id)+1, (endexp - id)))
      return(phi^exponent)
    }) |> t()
}

run_weightopt <- function(real_time_data){


}


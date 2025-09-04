#' Kalman Filter for Quarterly SPF Forecasts
#'
#' Computes the Kalman filter-based negative log-likelihood for a
#' given random walk variance in a state-space model.
#'
#' @param y A Tx2 matrix of annualized SPF projections (column 1)
#'   and observed quarterly growth rates (column 2).
#' @param rw_sd Standard deviation of the random walk process.
#' @param approx_err Standard deviation of the approximation error.
#' @param smooth Logical; if TRUE, outputs smoother variables.
#'
#' @return A list containing:
#'   \item{NegLL}{Negative log-likelihood}
#'   \item{x_fc}{Filtered state estimates (if smooth = TRUE)}
#'   \item{x_fc_var}{Filtered state variances (if smooth = TRUE)}
#'   \item{Lt}{Auxiliary variable (if smooth = TRUE)}
#'   \item{v_t}{Prediction residual (if smooth = TRUE)}
#'   \item{y_var_fc}{Covariance for predicting y_t (if smooth = TRUE)}
#'   \item{nan_ind}{Indicator for informative states (if smooth = TRUE)}
#'
#' @examples
#' q <- rbind(-1.975905496,-0.5638928,2.660615959,2.566018083,2.244165169,2.060216621,
#'            4.861686218,3.396030031,1.409498959,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,
#'            NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN)
#' a <- rbind(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,2.338002864,NaN,NaN,NaN,
#'            1.65436809,NaN,NaN,NaN,2.055519891,NaN,NaN,NaN,1.915861112,NaN,NaN,NaN,
#'            1.912181401,NaN,NaN,NaN,1.885626914,NaN,NaN,NaN,1.833479425)
#' y <- cbind(a,q)
#' result <- kalman_filter(y, rw_sd = 0.5, approx_err = 0.01, smooth = TRUE)
#'
#' @export
kalman_filter = function(y, rw_sd, approx_err, smooth = FALSE) {

  ###### State space representation

  ### Measurement and transition equation
  A <- matrix(c(
    1,0,0,0,0,0,0,
    1,0,0,0,0,0,0,
    0,1,0,0,0,0,0,
    0,0,1,0,0,0,0,
    0,0,0,1,0,0,0,
    0,0,0,0,1,0,0,
    0,0,0,0,0,1,0 ), nrow = 7, byrow = TRUE)

  B <- matrix(0, 7, 1)
  B[1, 1] <- rw_sd

  C <- matrix(c(
    1/16, 2/16, 3/16, 4/16, 3/16, 2/16, 1/16,
    1,    0,    0,    0,    0,    0,    0    ), nrow = 2, byrow = TRUE)

  D <- diag(c(approx_err, 0))

  # Output required for Kalman smoother (smooth = TRUE)
  TT <- nrow(y)
  if (smooth == TRUE) {
    x_fc <- matrix(data=NA,nrow=TT,ncol=7)
    x_fc_var <- array(rep(NA, 7*7*TT), dim=c(7, 7, TT))
    Lt <- array(rep(NA, 7*7*TT), dim=c(7, 7, TT))
    v_t_out <- matrix(data=NA,nrow=2,ncol=TT)
    y_var_fc_out <- array(rep(NA, 2*2*TT), dim=c(2, 2, TT))
    ind_nan_out <- matrix(data=NA,nrow=2,ncol=TT)
  }


  #### Kalman filter recursion and log-likelihood contributions
  y <- t(y)
  LL <- rep(0, TT)

  # Initialize Kalman filter recursion and matrix for filtered x
  xmean <- rep(0, 7);          # Mean of x0
  xvar <- diag(7) * 1.0e7;     # Variance of x0 = 10000000

  # Kalman filter
  for (t in 1:TT) {

    # Observation at t
    yt = y[,t]

    # Prior mean and covariance of x_t
    x_mean_fc <- A %*% xmean                             # (A1)
    x_var_fc = A %*% xvar %*% t(A) + B %*% t(B)          # (A2)

    # Prediction and prediction error of y_t|t-1
    y_mean_fc <- C %*% x_mean_fc
    v_t <- yt - y_mean_fc

    # Forecasting covariance and variance for predicting y_t
    y_cov_fc <- x_var_fc %*% t(C)
    y_var_fc <- C %*% y_cov_fc + D %*% t(D)              # (A3)

    # Adjust for NaN
    ind_nan <- !is.na(yt)
    y_var_fc_adj <- y_var_fc[ind_nan, ind_nan, drop = FALSE]
    y_cov_fc_adj <- y_cov_fc[, ind_nan, drop = FALSE]
    v_t_adj <- v_t[ind_nan, ,drop = FALSE]


    if (any(ind_nan)) {  # same as (sum(ind_nan)  > 0)

      # Kalman gain at t
      K <- y_cov_fc_adj %*% chol2inv(chol(y_var_fc_adj)) # (A4) = (A2)*C*inv(A3)

      # Posterior mean and covariance of x_t
      xmean <- x_mean_fc + K %*% v_t_adj                 # (A6)
      xvar <- x_var_fc - K %*%t (y_cov_fc_adj)           # (A7)

      # Log-likelihood contribution
      LL[t] = - 0.5 * ( sum(ind_nan)*log(2*pi) + log(det(matrix(y_var_fc_adj)))
                        + t(v_t_adj) %*% chol2inv(chol(y_var_fc_adj)) %*% v_t_adj)

    } else {

      # No information at time t, y = [NaN,NaN]
      xmean <- x_mean_fc
      xvar <- x_var_fc

    }

    # Output for Kalman smoother
    if (smooth == TRUE) {
      x_fc[t,] <- t(x_mean_fc)
      x_fc_var[, ,t] <- x_var_fc
      v_t_out[,t] <- v_t
      y_var_fc_out[, ,t] <- y_var_fc

      # Kalman gain
      K_aux <- matrix(data=0,nrow=7,ncol=2)
      K_aux[,ind_nan] <- K;
      ind_nan_out[,t] <- ind_nan

      # Auxiliary variable
      Lt[, ,t] <- A - A %*% K_aux %*% C
    }

  }

  # Negative Log-Likelihood
  LogL = -sum(LL)

  # Set up output
  if (smooth == TRUE) {
    retlist <- list(NegLL = LogL, x_fc = x_fc, x_fc_var = x_fc_var, v_t = v_t_out,
                    y_var_fc = y_var_fc_out, ind_nan = ind_nan_out, Lt = Lt)
  } else {
    retlist <- list(NegLL = LogL)
  }

  return(retlist)
}

# https://acp.copernicus.org/articles/14/9707/2014/acp-14-9707-2014.pdf



#' Kalman Smoother for Quarterly SPF Forecasts
#'
#' This function applies the Kalman smoothing algorithm to a set of filtered
#' state estimates obtained from from the `kalman_filter` function.
#'
#' @param states_filtered A list containing the outputs from the `kalman_filter` function:
#'   \itemize{
#'     \item{\code{NegLL}}{ Negative log-likelihood.}
#'     \item{\code{x_fc}}{ Filtered state estimates.}
#'     \item{\code{x_fc_var}}{ Filtered state variances.}
#'     \item{\code{Lt}}{ Auxiliary matrix for smoothing recursion.}
#'     \item{\code{v_t}}{ Prediction residuals.}
#'     \item{\code{y_var_fc}}{ Covariance matrix for predicting \eqn{y_t}.}
#'     \item{\code{nan_ind}}{ Indicator for missing observations.}
#'   }
#'
#' @return A matrix of smoothed state estimates, where each row corresponds
#' to a time step and each column corresponds to a state variable.
#'
#' @examples
#' q <- rbind(-1.975905496,-0.5638928,2.660615959,2.566018083,2.244165169,2.060216621,
#'            4.861686218,3.396030031,1.409498959,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,
#'            NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN)
#' a <- rbind(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,2.338002864,NaN,NaN,NaN,
#'            1.65436809,NaN,NaN,NaN,2.055519891,NaN,NaN,NaN,1.915861112,NaN,NaN,NaN,
#'            1.912181401,NaN,NaN,NaN,1.885626914,NaN,NaN,NaN,1.833479425)
#' y <- cbind(a,q)
#' states <- kalman_filter(y, rw_sd = 0.5, approx_err = 0.01, smooth = TRUE)
#' smoothed_states <- kalman_smoother(states)
#'
#' @export
kalman_smoother = function(states_filtered) {

  ###### State space representation

  ### Measurement equation
  C <- matrix(c(
    1/16, 2/16, 3/16, 4/16, 3/16, 2/16, 1/16,
    1,    0,    0,    0,    0,    0,    0    ), nrow = 2, byrow = TRUE)


  #### Kalman smoother recursion

  # Read out objects form list 'states_filtered'
  ind_nan_mat <- states_filtered$ind_nan  # Indicator of NaN
  y_fc_var <- states_filtered$y_var_fc    # Forecast error variance Cov(y_t)
  v_t <- states_filtered$v_t              # Prediction residual
  Lt <- states_filtered$Lt                # Auxiliary variable
  x_fc <- states_filtered$x_fc            # Filtered states
  x_fc_var <- states_filtered$x_fc_var    # Filtered variance

  # Initialize Kalman smoother recursion and matrix for smoothed x
  TT <- nrow(x_fc)
  r_t <- rep(0, 7);                                # Auxiliary variable (A9)
  x_smoothed <- matrix(data=NA,nrow=TT,ncol=7)     # Smoothed states


  # Kalman smoother
  for (t in TT:1) {

    # Extract relevant objects at time t
    ind_nan <- ind_nan_mat[,t]
    Lt_aux <- Lt[, ,t]                                    # (A8)
    v_t_aux <- v_t[,t]
    y_fc_var_aux <- y_fc_var[, ,t]
    x_fc_aux <- x_fc[t,]
    x_fc_var_aux <- x_fc_var[, ,t]


    ### Smooth states at time t|T

    # Auxiliary variable                                  # (A9)
    if (any(ind_nan)) {
      r_t <- t(Lt_aux) %*% r_t + t(C[ind_nan, ,drop = FALSE]) %*%
        chol2inv(chol(y_fc_var_aux[ind_nan, ind_nan, drop = FALSE])) %*%
        v_t_aux[ind_nan, drop = FALSE]
    } else {
      r_t <- t(Lt_aux) %*% r_t
    }

    # Smoothed state mean
    x_smoothed[t,] <- t(x_fc_aux + x_fc_var_aux %*% r_t)  # (A11)

  }

  return(x_smoothed)
}




#' Log-Likelihood Function for Kalman Filter (for rw_sd optimization)
#'
#' This helper function computes the negative log-likelihood (NegLL)
#' for a given value of the random walk standard deviation (rw_sd),
#' using the `kalman_filter` function. It is intended for use in optimization
#' procedures to estimate `rw_sd`.
#'
#' @param rw_sd The random walk standard deviation (rw_sd) to optimize.
#' @param y A Tx2 matrix of annualized SPF projections (column 1)
#'   and observed quarterly growth rates (column 2).
#' @param approx_err The fixed standard deviation of the approximation error.
#'
#' @return The negative log-likelihood value (NegLL) for the given `rw_sd`.
#'
#' @examples
#' log_likelihood_function(rw_sd = 0.5, y = y, approx_err = 0.01)
#'
#' @export
log_likelihood_function <- function(rw_sd, y, approx_err) {

  # Run the Kalman filter to get the negative log-likelihood
  result <- kalman_filter(y, rw_sd, approx_err, smooth = FALSE)
  return(result$NegLL)
}



#' SPF Filter to Estimate Implied Forecasts Using Kalman Smoothing
#'
#' This function applies a Kalman filter and smoother to quarterly real GDP
#' growth rates and SPF (Survey of Professional Forecasters) projections to
#' estimate the implied latent forecast path. It handles missing data by masking
#' SPF projections prior to the first observed real GDP data point, and estimates
#' the random walk variance using maximum likelihood.
#'
#' @param rgdp A numeric vector of observed quarterly real GDP growth rates.
#' @param spf A numeric vector of SPF projections, annualized.
#'
#' @return A matrix with one column named \code{"SPF_implied"}, representing the
#' smoothed latent forecast series from the state-space model.
#'
#' @details Internally, this function:
#' \itemize{
#'   \item Combines \code{rgdp} and \code{spf} into a 2-column matrix.
#'   \item Masks SPF projections prior to the first observed \code{rgdp} value.
#'   \item Optimizes the standard deviation of the random walk process (\code{rw_sd})
#'         using maximum likelihood via the \code{log_likelihood_function}.
#'   \item Applies the Kalman filter and smoother using the estimated \code{rw_sd}.
#' }
#'
#' @seealso \code{\link{kalman_filter}}, \code{\link{log_likelihood_function}}
#'
#' @examples
#' # Example usage with synthetic data:
#' SPF <- SPF_filter(rgdp = y[,2], spf = y[,1])
#'
#' @export
SPF_filter <- function(rgdp,spf) {

  # Prepare input for Kalman filter and smoother
  y <- cbind(spf,rgdp)

  # SPF forecasts prior to latest GDP release is not considered by the filter
  nan_idx <- which(is.nan(rgdp))
  first_nan_pos <- if (length(nan_idx) > 0) min(nan_idx) else NA
  y[1:(first_nan_pos-1),1] <- NaN

  # Estimate the random walk error standard deviation
  start <- 0.5
  est_sd <- optim(par = start,
                  fn = log_likelihood_function,
                  y = y,
                  approx_err = 0.01,
                  method = "L-BFGS-B",
                  lower = 0.0001,
                  upper = Inf)

  # Given the estimate 'est_sd', filter and smooth states, i.e., implied SPF
  filtered_states <- kalman_filter(y, rw_sd = est_sd$par, approx_err = 0.01, smooth = TRUE)
  SPF <- as.matrix(kalman_smoother(filtered_states)[,1])
  colnames(SPF) <- "SPF_implied"

  return(SPF)

}


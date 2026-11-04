#' Kalman Filter for Monthly SPF-Consistent Unemployment Rate Forecasts
#'
#' Monthly analog for the unemployment rate. The state is the latent monthly
#' unemployment-rate LEVEL and follows a random walk. The calendar-year SPF
#' projection is the average of the 12 monthly levels, so row 1 of \code{C}
#' has equal weights 1/12 on a 12-month state (no triangular weights: levels,
#' not growth rates). The unemployment rate is published monthly, so row 2 is
#' the identity on the current month, observed without noise (D22 = 0).
#' Only the annual row carries an (externally calibrated) approximation error.
#'
#' @param y A Tx2 matrix, monthly-indexed. Column 1: annual SPF projections,
#'   non-missing only at December positions. Column 2: monthly unemployment rate,
#'   NaN for not-yet-released months.
#' @param rw_sd Standard deviation of the monthly random walk process.
#' @param approx_err_annual Standard deviation of the annual-from-monthly
#'   approximation error (row 1 of \code{C}), calibrated externally.
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
#' @export
kalman_filter_monthly_unemp = function(y, rw_sd, approx_err_annual, smooth = FALSE) {

  ###### State space representation (12-month state, see derivation note)

  n_state <- 12

  ### Transition matrix: monthly lag-shift, same structure as the quarterly A,
  ### just resized (state = current month + 22 lags)
  A <- rbind(
    c(1, rep(0, n_state - 1)),
    cbind(diag(n_state - 1), 0)
  )

  ### Random walk shock hits only the current-month element, as before
  B <- matrix(0, n_state, 1)
  B[1, 1] <- rw_sd

  ### Measurement matrix: one approximate annual row + noiseless monthly identity row
  # Row 1: annual-from-monthly, equal weights 1/12 (annual average of the 12 monthly levels)
  w_annual <- rep(1, 12) / 12
  # Row 2: monthly unemployment rate observed directly (identity on current month)

  C <- matrix(0, nrow = 2, ncol = n_state)
  C[1, 1:length(w_annual)] <- w_annual
  C[2, 1] <- 1   # monthly unemployment rate is observed directly: identity row

  ### Only the annual row is approximate
  D <- diag(c(approx_err_annual, 0))   # identity row is noiseless

  # Output required for Kalman smoother (smooth = TRUE)
  TT <- nrow(y)
  if (smooth == TRUE) {
    x_fc <- matrix(data=NA, nrow=TT, ncol=n_state)
    x_fc_var <- array(rep(NA, n_state*n_state*TT), dim=c(n_state, n_state, TT))
    Lt <- array(rep(NA, n_state*n_state*TT), dim=c(n_state, n_state, TT))
    v_t_out <- matrix(data=NA, nrow=2, ncol=TT)
    y_var_fc_out <- array(rep(NA, 2*2*TT), dim=c(2, 2, TT))
    ind_nan_out <- matrix(data=NA, nrow=2, ncol=TT)
  }


  #### Kalman filter recursion and log-likelihood contributions
  y <- t(y)
  LL <- rep(0, TT)

  # Initialize Kalman filter recursion and matrix for filtered x
  xmean <- rep(0, n_state);          # Mean of x0
  xvar <- diag(n_state) * 1.0e7;     # Variance of x0 = 10000000

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
      K_aux <- matrix(data=0, nrow=n_state, ncol=2)
      if (any(ind_nan)) {
        K_aux[,ind_nan] <- K
      }
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



#' Kalman Smoother for Monthly SPF-Consistent Forecasts
#'
#' Monthly analog of \code{kalman_smoother}. Applies the Kalman smoothing
#' algorithm (means and, in parallel, smoothed state covariances P_{t|T}) to
#' the filtered output of \code{kalman_filter_monthly_unemp}. Structurally identical
#' recursion to the quarterly version; only the state dimension and the
#' (now two-row-approximate) measurement matrix \code{C} change.
#'
#' @param states_filtered A list containing the outputs from
#'   \code{kalman_filter_monthly_unemp} (see that function's documentation for the
#'   required fields).
#'
#' @return A list containing:
#'   \item{x_smoothed}{A TTx12 matrix of smoothed state estimates (monthly
#'     growth rate and 22 lags), one row per month.}
#'   \item{x_smoothed_var}{A 12x12xTT array of smoothed state covariance
#'     matrices P_{t|T}, one 12x12 slice per month.}
#'
#' @export
kalman_smoother_monthly_unemp = function(states_filtered) {

  n_state <- 12

  ###### State space representation (measurement matrix, must match the filter)
  w_annual <- rep(1, 12) / 12

  C <- matrix(0, nrow = 2, ncol = n_state)
  C[1, 1:length(w_annual)] <- w_annual
  C[2, 1] <- 1   # monthly unemployment rate is observed directly: identity row


  #### Kalman smoother recursion

  # Read out objects form list 'states_filtered'
  ind_nan_mat <- states_filtered$ind_nan  # Indicator of NaN
  y_fc_var <- states_filtered$y_var_fc    # Forecast error variance Cov(y_t)
  v_t <- states_filtered$v_t              # Prediction residual
  Lt <- states_filtered$Lt                # Auxiliary variable
  x_fc <- states_filtered$x_fc            # Filtered states
  x_fc_var <- states_filtered$x_fc_var    # Filtered variance

  # Initialize Kalman smoother recursion and matrices for smoothed x
  TT <- nrow(x_fc)
  r_t <- rep(0, n_state);                                              # Auxiliary variable (A9), for the mean
  N_t <- matrix(0, nrow = n_state, ncol = n_state);                    # Auxiliary matrix, for the variance
  x_smoothed <- matrix(data=NA, nrow=TT, ncol=n_state)                 # Smoothed states (means)
  x_smoothed_var <- array(rep(NA, n_state*n_state*TT), dim=c(n_state, n_state, TT))  # Smoothed state variances


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

    # Auxiliary variable for the mean                      # (A9)
    if (any(ind_nan)) {
      r_t <- t(Lt_aux) %*% r_t + t(C[ind_nan, ,drop = FALSE]) %*%
        chol2inv(chol(y_fc_var_aux[ind_nan, ind_nan, drop = FALSE])) %*%
        v_t_aux[ind_nan, drop = FALSE]
    } else {
      r_t <- t(Lt_aux) %*% r_t
    }

    # Auxiliary matrix for the variance
    if (any(ind_nan)) {
      N_t <- t(C[ind_nan, ,drop = FALSE]) %*%
        chol2inv(chol(y_fc_var_aux[ind_nan, ind_nan, drop = FALSE])) %*%
        C[ind_nan, ,drop = FALSE] +
        t(Lt_aux) %*% N_t %*% Lt_aux
    } else {
      N_t <- t(Lt_aux) %*% N_t %*% Lt_aux
    }

    # Smoothed state mean                                  # (A11)
    x_smoothed[t,] <- t(x_fc_aux + x_fc_var_aux %*% r_t)

    # Smoothed state variance
    x_smoothed_var[, ,t] <- x_fc_var_aux - x_fc_var_aux %*% N_t %*% x_fc_var_aux

  }

  return(list(x_smoothed = x_smoothed, x_smoothed_var = x_smoothed_var))
}



#' Log-Likelihood Function for Monthly Kalman Filter (for rw_sd optimization)
#'
#' Monthly analog of \code{log_likelihood_function}. Computes the negative
#' log-likelihood for a given monthly random walk standard deviation
#' (\code{rw_sd}), using \code{kalman_filter_monthly_unemp}. Both approximation
#' error SD is treated as a fixed, externally calibrated input (not
#' optimized here).
#'
#' @param rw_sd The monthly random walk standard deviation to optimize.
#' @param y A Tx2 matrix, monthly-indexed (see \code{kalman_filter_monthly_unemp}).
#' @param approx_err_annual Fixed SD of the annual-from-monthly approximation
#'   error, calibrated externally.
#'
#' @return The negative log-likelihood value (NegLL) for the given \code{rw_sd}.
#'
#' @export
log_likelihood_function_monthly_unemp <- function(rw_sd, y, approx_err_annual) {

  # Run the monthly Kalman filter to get the negative log-likelihood
  result <- kalman_filter_monthly_unemp(y, rw_sd, approx_err_annual, smooth = FALSE)
  return(result$NegLL)
}



#' SPF Filter to Estimate Implied Monthly Forecasts Using Kalman Smoothing
#'
#' Monthly analog of \code{SPF_filter} for unemployment rate. Inputs are already on
#' the monthly grid, so no expansion is needed. As in \code{SPF_filter}, SPF
#' projections up to and including the latest released unemployment rate month are
#' masked, so the annual SPF only informs the region the data do not yet cover.
#'
#' @param runemp Numeric vector of monthly unemployment rate (level), NaN for
#'   not-yet-released months.
#' @param spf Numeric vector of annual SPF projections on the monthly grid
#'   (same length as \code{runemp}), non-missing only at December positions.
#' @param approx_err_annual SD of the annual-from-monthly approximation
#'   error, calibrated externally (e.g.\ from real-time IP vintages).
#'
#' @return A list with the following components:
#' \describe{
#'   \item{SPF_filtered}{A matrix with one column named \code{"SPF_implied"},
#'   representing the smoothed latent monthly forecast series.}
#'   \item{SPF_filtered_var}{A vector of the corresponding smoothed monthly
#'   state variances (the (1,1) diagonal entries of \code{x_smoothed_var}
#'   across time).}
#'   \item{par_est}{A named numeric vector of estimated parameters
#'   (\code{rw_sd}).}
#'   \item{logLik}{The value of the log-likelihood at the optimum.}
#' }
#'
#' @seealso \code{\link{kalman_filter_monthly_unemp}}, \code{\link{log_likelihood_function_monthly_unemp}}
#'
#' @export
SPF_filter_monthly_unemp <- function(runemp, spf, approx_err_annual) {

  # --- Inputs are already monthly: no expansion needed ---
  # runemp: monthly unemployment rate (level), NaN for not-yet-released months
  # spf:   annual SPF projection on the monthly grid, non-missing only at
  #        December positions (as in the quarterly pipeline's q~=4 entries)
  y <- cbind(spf, runemp)

  # SPF projections up to and including the latest unemployment rate release month are
  # not considered by the filter (same logic as the quarterly SPF_filter)
  last_infl_month <- max(which(!is.na(runemp)))
  y[1:last_infl_month, 1] <- NaN

  # Estimate the monthly random walk error standard deviation;
  # approx_err_annual are fixed (externally calibrated)
  start <- 0.1   # monthly unemployment-rate changes are small
  est_sd <- optim(par = start,
                  fn = log_likelihood_function_monthly_unemp,
                  y = y,
                  approx_err_annual = approx_err_annual,
                  method = "L-BFGS-B",
                  lower = 0.0001,
                  upper = Inf)

  # Given the estimate 'est_sd', filter and smooth states, i.e., implied
  # monthly SPF-consistent forecasts and their smoothed variance
  filtered_states <- kalman_filter_monthly_unemp(y, rw_sd = est_sd$par,
                                                 approx_err_annual = approx_err_annual,
                                                 smooth = TRUE)
  smoothed <- kalman_smoother_monthly_unemp(filtered_states)
  SPF <- as.matrix(smoothed$x_smoothed[,1])
  SPF_var <- smoothed$x_smoothed_var[1, 1, ]

  # Define output of this function
  colnames(SPF) <- "SPF_implied"
  est_par <- est_sd$par
  names(est_par) <- c("rw_sd")

  Output <- list(
    SPF_filtered     = SPF,
    SPF_filtered_var = SPF_var,
    par_est          = est_par,
    logLik           = est_sd$value
  )
  return(Output)

}

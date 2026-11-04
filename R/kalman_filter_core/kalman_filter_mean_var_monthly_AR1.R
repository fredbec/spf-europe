#' Kalman Filter for Monthly SPF-Consistent Forecasts with AR(1) transition
#'
#' AR(1) variant of \code{kalman_filter_monthly}. The state space is identical
#' (23-month state, two approximate measurement rows with externally
#' calibrated error SDs); only the transition equation changes from a random
#' walk to
#' \deqn{x_t = \phi_0 + \phi_1 x_{t-1} + v_t,}
#' implemented as in the quarterly fixed-event/fixed-horizon filter: an
#' intercept vector \code{Cons} in the prediction step and \code{phi1} in the
#' (1,1) element of \code{A}.
#'
#' @param y A Tx2 matrix, monthly-indexed. Column 1: annual SPF projections
#'   (non-missing only at December positions), column 2: quarterly real GDP
#'   growth (non-missing only at quarter-end months).
#' @param par Numeric vector \code{c(phi0, phi1, sd)} on the natural scale:
#'   intercept, AR(1) coefficient, and innovation standard deviation.
#' @param approx_err_annual SD of the annual-from-monthly approximation error
#'   (row 1 of \code{C}), calibrated externally.
#' @param approx_err_quarterly SD of the quarterly-from-monthly approximation
#'   error (row 2 of \code{C}), calibrated externally.
#' @param smooth Logical; if TRUE, outputs smoother variables.
#'
#' @return A list containing NegLL and, if \code{smooth = TRUE}, x_fc,
#'   x_fc_var, Lt, v_t, y_var_fc and ind_nan (as in \code{kalman_filter_monthly}).
#'
#' @export
kalman_filter_monthly_ar1 = function(y, par, approx_err_annual, approx_err_quarterly, smooth = FALSE) {

  ###### State space representation (23-month state)

  n_state <- 23
  phi0  <- par[1]
  phi1  <- par[2]
  sd_v  <- par[3]

  ### Intercept of the AR(1), only in the current-month element
  Cons <- matrix(0, n_state, 1)
  Cons[1, 1] <- phi0

  ### Transition matrix: AR(1) coefficient in (1,1), lag-shift elsewhere
  A <- rbind(
    c(phi1, rep(0, n_state - 1)),
    cbind(diag(n_state - 1), 0)
  )

  ### Shock hits only the current-month element
  B <- matrix(0, n_state, 1)
  B[1, 1] <- sd_v

  ### Measurement matrix: two approximate rows (unchanged from random-walk version)
  w_annual    <- c(1:12, 11:1) / 144
  w_quarterly <- c(1, 2, 3, 2, 1) / 9

  C <- matrix(0, nrow = 2, ncol = n_state)
  C[1, 1:length(w_annual)]    <- w_annual
  C[2, 1:length(w_quarterly)] <- w_quarterly

  D <- diag(c(approx_err_annual, approx_err_quarterly))

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
  xmean <- rep(0, n_state)          # Mean of x0
  xvar <- diag(n_state) * 1.0e7     # Variance of x0 = 10000000

  # Kalman filter
  for (t in 1:TT) {

    # Observation at t
    yt = y[,t]

    # Prior mean and covariance of x_t (intercept added for the AR(1))
    x_mean_fc <- Cons + A %*% xmean                      # (A1)
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


    if (any(ind_nan)) {

      # Kalman gain at t
      K <- y_cov_fc_adj %*% chol2inv(chol(y_var_fc_adj)) # (A4)

      # Posterior mean and covariance of x_t
      xmean <- x_mean_fc + K %*% v_t_adj                 # (A6)
      xvar <- x_var_fc - K %*% t(y_cov_fc_adj)           # (A7)

      # Log-likelihood contribution (det of the square adjusted matrix)
      LL[t] = - 0.5 * ( sum(ind_nan)*log(2*pi) + log(det(y_var_fc_adj))
                        + t(v_t_adj) %*% chol2inv(chol(y_var_fc_adj)) %*% v_t_adj)

    } else {

      # No information at time t
      xmean <- x_mean_fc
      xvar <- x_var_fc

    }

    # Output for Kalman smoother
    if (smooth == TRUE) {
      x_fc[t,] <- t(x_mean_fc)
      x_fc_var[, ,t] <- x_var_fc
      v_t_out[,t] <- v_t
      y_var_fc_out[, ,t] <- y_var_fc

      # Kalman gain (stays zero when nothing is observed)
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



#' Kalman Smoother for Monthly SPF-Consistent Forecasts (AR(1) variant)
#'
#' Identical recursion to \code{kalman_smoother_monthly}. The AR(1) intercept
#' and coefficient enter only through the stored filtered states (\code{x_fc})
#' and \code{Lt}, so no change to the smoother itself is needed; the function
#' is repeated here only so that this file is self-contained.
#'
#' @param states_filtered Output list of \code{kalman_filter_monthly_ar1}
#'   with \code{smooth = TRUE}.
#'
#' @return A list with \code{x_smoothed} (TTx23 smoothed means) and
#'   \code{x_smoothed_var} (23x23xTT smoothed covariances).
#'
#' @export
kalman_smoother_monthly_ar1 = function(states_filtered) {

  n_state <- 23

  ###### Measurement matrix (must match the filter)
  w_annual    <- c(1:12, 11:1) / 144
  w_quarterly <- c(1, 2, 3, 2, 1) / 9

  C <- matrix(0, nrow = 2, ncol = n_state)
  C[1, 1:length(w_annual)]    <- w_annual
  C[2, 1:length(w_quarterly)] <- w_quarterly


  #### Kalman smoother recursion
  ind_nan_mat <- states_filtered$ind_nan
  y_fc_var <- states_filtered$y_var_fc
  v_t <- states_filtered$v_t
  Lt <- states_filtered$Lt
  x_fc <- states_filtered$x_fc
  x_fc_var <- states_filtered$x_fc_var

  TT <- nrow(x_fc)
  r_t <- rep(0, n_state)
  N_t <- matrix(0, nrow = n_state, ncol = n_state)
  x_smoothed <- matrix(data=NA, nrow=TT, ncol=n_state)
  x_smoothed_var <- array(rep(NA, n_state*n_state*TT), dim=c(n_state, n_state, TT))

  for (t in TT:1) {

    ind_nan <- ind_nan_mat[,t]
    Lt_aux <- Lt[, ,t]
    v_t_aux <- v_t[,t]
    y_fc_var_aux <- y_fc_var[, ,t]
    x_fc_aux <- x_fc[t,]
    x_fc_var_aux <- x_fc_var[, ,t]

    # Auxiliary variable for the mean
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

    # Smoothed state mean and variance
    x_smoothed[t,] <- t(x_fc_aux + x_fc_var_aux %*% r_t)
    x_smoothed_var[, ,t] <- x_fc_var_aux - x_fc_var_aux %*% N_t %*% x_fc_var_aux

  }

  return(list(x_smoothed = x_smoothed, x_smoothed_var = x_smoothed_var))
}



#' Log-Likelihood Function for the AR(1) Monthly Kalman Filter
#'
#' Helper for \code{optim}. As in the quarterly fixed-event/fixed-horizon
#' filter, the innovation SD is optimized on the log scale to keep it positive.
#'
#' @param par Numeric vector \code{c(phi0, phi1, log(sd))}.
#' @param y A Tx2 matrix, monthly-indexed.
#' @param approx_err_annual,approx_err_quarterly Fixed, externally calibrated SDs.
#'
#' @return The negative log-likelihood.
#'
#' @export
log_likelihood_function_monthly_ar1 <- function(par, y, approx_err_annual, approx_err_quarterly) {

  par_nat <- c(par[1], par[2], exp(par[3]))
  result <- kalman_filter_monthly_ar1(y, par_nat, approx_err_annual, approx_err_quarterly, smooth = FALSE)
  return(result$NegLL)
}



#' SPF Filter for Implied Monthly Forecasts, AR(1) transition
#'
#' AR(1) variant of \code{SPF_filter_monthly}, with the same quarterly-indexed
#' inputs and internal expansion to the monthly grid. Estimates
#' \code{phi0}, \code{phi1} and the innovation SD jointly by QML; the two
#' approximation-error SDs remain fixed, externally calibrated inputs.
#'
#' @param rgdp Quarterly real GDP growth, NaN for not-yet-observed quarters.
#' @param spf Annual SPF projections on the quarterly grid, non-missing only at
#'   \eqn{\tilde q=4} positions.
#' @param approx_err_annual,approx_err_quarterly Externally calibrated SDs.
#'
#' @return A list with \code{SPF_filtered} (smoothed monthly path),
#'   \code{SPF_filtered_var} (its smoothed variance), \code{par_est}
#'   (\code{phi0}, \code{phi1}, \code{sd} on the natural scale),
#'   \code{uncond_mean} (\code{phi0/(1-phi1)}) and \code{logLik}.
#'
#' @export
SPF_filter_monthly_ar1 <- function(rgdp, spf, approx_err_annual, approx_err_quarterly) {

  # --- Expand quarterly-indexed inputs to the monthly grid ---
  TQ <- length(rgdp)
  TM <- 3 * TQ

  rgdp_q <- rep(NaN, TM)
  spf_m  <- rep(NaN, TM)
  quarter_end_months <- 3 * (1:TQ)
  rgdp_q[quarter_end_months] <- rgdp
  spf_m[quarter_end_months]  <- spf

  y <- cbind(spf_m, rgdp_q)

  # SPF projections up to and including the latest GDP release month are not
  # considered by the filter (same logic as the quarterly SPF_filter; on the
  # monthly grid this must be based on the last observed GDP month, since
  # non-quarter-end months of rgdp_q are always NaN)
  last_gdp_month <- max(which(!is.na(rgdp_q)))
  y[1:last_gdp_month, 1] <- NaN

  # Estimate phi0, phi1 and log(sd) jointly
  start <- c(0, 0.8, log(0.5))
  est <- optim(par = start,
               fn = log_likelihood_function_monthly_ar1,
               y = y,
               approx_err_annual = approx_err_annual,
               approx_err_quarterly = approx_err_quarterly,
               method = "L-BFGS-B",
               lower = c(-Inf, -0.99, -Inf),
               upper = c( Inf,  0.99,  Inf))

  # Transform back from the log scale before the final filter/smoother pass
  par_nat <- c(est$par[1:2], exp(est$par[3]))

  filtered_states <- kalman_filter_monthly_ar1(y, par = par_nat,
                                               approx_err_annual = approx_err_annual,
                                               approx_err_quarterly = approx_err_quarterly,
                                               smooth = TRUE)
  smoothed <- kalman_smoother_monthly_ar1(filtered_states)
  SPF <- as.matrix(smoothed$x_smoothed[,1])
  SPF_var <- smoothed$x_smoothed_var[1, 1, ]

  colnames(SPF) <- "SPF_implied"
  names(par_nat) <- c("phi0", "phi1", "sd")

  Output <- list(
    SPF_filtered     = SPF,
    SPF_filtered_var = SPF_var,
    par_est          = par_nat,
    uncond_mean      = unname(par_nat["phi0"] / (1 - par_nat["phi1"])),
    logLik           = est$value
  )
  return(Output)

}
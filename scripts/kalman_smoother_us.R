#' Kalman Smoother for Quarterly SPF Forecasts
#'
#' This function applies the Kalman smoothing algorithm to a set of filtered
#' state estimates obtained from from the `kalman_filter_us` function.
#'
#' @param states_filtered A list containing the outputs from the `kalman_filter_us` function:
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
#' rgdp <- rbind(-1.975905496,-0.5638928,2.660615959,2.566018083,2.244165169,2.060216621,
#'            4.861686218,3.396030031,1.409498959,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,
#'            NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN)
#' spf <- rbind(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,2.338002864,NaN,NaN,NaN,
#'            1.65436809,NaN,NaN,NaN,2.055519891,NaN,NaN,NaN,1.915861112,NaN,NaN,NaN,
#'            1.912181401,NaN,NaN,NaN,1.885626914,NaN,NaN,NaN,1.833479425)
#' us_spf <- rgdp * 0.8
#' us_spf[10:13] = c(0.9, 1.1, 1, 3)
#'
#' y <- cbind(spf,rgdp,us_spf)
#' states <- kalman_filter_us(y, rw_sd = 0.5, rw_us_sd = 0.5, approx_err = 0.01, smooth = TRUE)
#' smoothed_states <- kalman_smoother_us(states)
#'
#' @export
kalman_smoother_us = function(states_filtered) {

  ###### State space representation

  ### Measurement equation
  C <- matrix(c(
    1/16, 2/16, 3/16, 4/16, 3/16, 2/16, 1/16,
    1,    0,    0,    0,    0,    0,    0,
    1,    0,    0,    0,    0,    0,    0    ), nrow = 3, byrow = TRUE)


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

# https://acp.copernicus.org/articles/14/9707/2014/acp-14-9707-2014.pdf

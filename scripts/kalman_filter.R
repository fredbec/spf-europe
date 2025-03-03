### Text




kalman_filter = function(y, start_val, approx_error = 0.1, smooth = FALSE) {

  ###### State space representation

  ### Measurement and transition equation
  A <- rbind(c(1,0,0,0,0,0,0),
            c(1,0,0,0,0,0,0),
            c(0,1,0,0,0,0,0),
            c(0,0,1,0,0,0,0),
            c(0,0,0,1,0,0,0),
            c(0,0,0,0,1,0,0),
            c(0,0,0,0,0,1,0))

  B <- rbind(c(start_val,0,0,0,0,0,0),
            c(0,0,0,0,0,0,0),
            c(0,0,0,0,0,0,0),
            c(0,0,0,0,0,0,0),
            c(0,0,0,0,0,0,0),
            c(0,0,0,0,0,0,0),
            c(0,0,0,0,0,0,0))

  C <- rbind(c(1/16,2/16,3/16,4/17,3/16,2/16,1/16),
            c(1,0,0,0,0,0,0))

  D <- rbind(c(approx_error,0),
            c(0,0))


  # Output required for Kalman smoother (smooth = 1)
  TT <- nrow(y)


  #### Kalman filter recursion and log-likelihood contributions
  y <- t(y)
  LL <- rep(0, TT)

  # Initialize Kalman filter recursion and matrix for filtered x
  xmean = rep(0, 7);            # Mean of x0
  xvar = diag(7)*1.0e7;          # Variance of x0 = 10000000

  # Klman filter
  for (t in 1:TT) {

    # Obervation at t
    yt = y[,t]

    # Prior mean and covariance of x_t
    x_mean_fc = A%*%xmean                            # (A1)
    x_var_fc = A%*%xvar%*%t(A) + B%*%t(B)            # (A2)

    # Prediction and prediction error of y_t|t-1
    y_mean_fc = C%*%x_mean_fc
    v_t = yt - y_mean_fc

    # Forecasting covariance and variance for predicting y_t
    y_cov_fc = x_var_fc%*%t(C)
    y_var_fc = C%*%y_cov_fc + D%*%t(D)                   # (A3)

    # Adjust for NaN
    ind_nan <- !is.na(yt)
    y_var_fc_adj = y_var_fc[ind_nan, ind_nan]
    y_cov_fc_adj = y_cov_fc[, ind_nan]
    v_t_adj = v_t[ind_nan,]

    # Kalman gain at t
    K = y_cov_fc_adj%*%solve(y_var_fc_adj)                  # (A4) = (A2)*C*inv(A3)


    #
    retlist <- list(LL = LL, K = K, y)

    mything <- retlist$LL
    return()
  }



  test <- "hi"


  return(test)
}

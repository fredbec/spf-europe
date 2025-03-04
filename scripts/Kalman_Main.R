### Kalman Main - to test Kalman filter and smoother
library(here)

rm(list = ls())
cat("\014")


source(here("scripts", "kalman_filter.R"))
source(here("scripts", "kalman_smoother.R"))

q <- rbind(-1.975905496,-0.5638928,2.660615959,2.566018083,2.244165169,2.060216621,
           4.861686218,3.396030031,1.409498959,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,
           NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN)
a <- rbind(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,2.338002864,NaN,NaN,NaN,1.65436809,
           NaN,NaN,NaN,2.055519891,NaN,NaN,NaN,1.915861112,NaN,NaN,NaN,1.912181401,NaN,NaN,
           NaN,1.885626914,NaN,NaN,NaN,1.833479425)

y <- cbind(a,q)

# start will be estimated using built-in optimizer
matlab_est <- 1.33424292247901 # was estimated using Matlab

# Estimate the random walk error standard deviation
start <- 0.5
est_sd <- optim(par = start,
                fn = log_likelihood_function,
                y = y,
                approx_err = 0.01,
                method = "L-BFGS-B",
                lower = 0.0001,
                upper = Inf)

# Check difference to Matlab implementation
matlab_est - est_sd$par

# Given the estimate 'est_sd', we can filter and smooth states, i.e., implied SPF
filtered_states <- kalman_filter(y, rw_sd = est_sd$par, approx_err = 0.01, smooth = TRUE)
SPF <- kalman_smoother(filtered_states)


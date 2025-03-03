### Kalman Main - to test Kalman filter and smoother
library(here)

rm(list = ls())
cat("\014")


source(here("scripts", "kalman_filter.R"))

q <- rbind(-1.975905496,-0.5638928,2.660615959,2.566018083,2.244165169,2.060216621,4.861686218,3.396030031,1.409498959,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN)
a <- rbind(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,2.338002864,NaN,NaN,NaN,1.65436809,NaN,NaN,NaN,2.055519891,NaN,NaN,NaN,1.915861112,NaN,NaN,NaN,1.912181401,NaN,NaN,NaN,1.885626914,NaN,NaN,NaN,1.833479425)

y <- cbind(a,q)

start <- 0.5
smooth <- FALSE
approx_error <- 0.01

test <- kalman_filter(y,start,approx_error)






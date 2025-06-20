### Kalman Main - to test Kalman filter and smoother


# This Code is a mess and just to play around!!!!!!!!!!!!!!
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

# Test the SPF-filter function
test <- SPF_filter(q,a)

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
SPF <- as.matrix(kalman_smoother(filtered_states)[,1])



# Real-time filter conditional on latest vintage available

rm(list = ls())
cat("\014")


source(here("scripts", "kalman_filter.R"))
source(here("scripts", "kalman_smoother.R"))


# Set up hypothetical data set
rgdp <- rbind(-1.975905496,-0.5638928,2.660615959,2.566018083,2.244165169,2.060216621,
           4.861686218,3.396030031,1.409498959,1.343469,2.1361234,0.5125,-0.22352,1.121177,0.659696,1.7478568,1.12568865,2.0120120,
           1.23234,0.8124124,1.123452345,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN)

skip = F
if (skip == T) {
rgdp_test <- rbind(-1.975905496,-0.5638928,2.660615959,2.566018083,2.244165169,2.060216621,
              4.861686218,3.396030031,1.409498959,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,
              NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN)

spf1 <- rbind(NaN,NaN,NaN,2.0119931,NaN,NaN,NaN,1.3453435,NaN,NaN,NaN,2.338002864,NaN,NaN,NaN,1.65436809,
             NaN,NaN,NaN,2.055519891,NaN,NaN,NaN,1.915861112,NaN,NaN,NaN,1.912181401,NaN,NaN,
             NaN,1.885626914,NaN,NaN,NaN,1.833479425)
spf2 <- rbind(NaN,NaN,NaN,2.0119931,NaN,NaN,NaN,1.3453435,NaN,NaN,NaN,2.338002864,NaN,NaN,NaN,1.65436809,
              NaN,NaN,NaN,2.055519891,NaN,NaN,NaN,1.915861112,NaN,NaN,NaN,1.912181401,NaN,NaN,
              NaN,1.885626914,NaN,NaN,NaN,1.833479425)*0.9
spf3 <- rbind(NaN,NaN,NaN,2.0119931,NaN,NaN,NaN,1.3453435,NaN,NaN,NaN,2.338002864,NaN,NaN,NaN,1.65436809,
              NaN,NaN,NaN,2.055519891,NaN,NaN,NaN,1.915861112,NaN,NaN,NaN,1.912181401,NaN,NaN,
              NaN,1.885626914,NaN,NaN,NaN,1.833479425)*1.1
spf4 <- rbind(NaN,NaN,NaN,2.0119931,NaN,NaN,NaN,1.3453435,NaN,NaN,NaN,2.338002864,NaN,NaN,NaN,1.65436809,
              NaN,NaN,NaN,2.055519891,NaN,NaN,NaN,1.915861112,NaN,NaN,NaN,1.912181401,NaN,NaN,
              NaN,1.885626914,NaN,NaN,NaN,1.833479425)*1.05
}

years <- 2000:2008
quarters <- paste0("Q", 1:4)
qdates <- unlist(lapply(years, function(y) paste0(y, ":", quarters)))



### Example being at 2005:Q3
# Input to function
latest_date <- "2005:Q3"
data_all <- data.frame(qdates, rgdp, stringsAsFactors = FALSE)
colnames(data_all) <- c("date", "rgdp")
# data: Quarterly date, annualized quarterly GDP growth rates, SPF year over year
#       growth taken from the latest available survey. NOTE: The annual growth rates
#       of the SPF for a respective year need to be in YYYY:Q4.
data <- data_all[1:28,]


##### Inside the function
cols <- paste0("SPF_Q")
data[cols] <- NaN
cols <- paste0("SPF", 0:3, "_rt")
data[cols] <- NaN



# Current quarter
TT <- which(data$date == latest_date)


for (t in TT:10) {   # Should start at first survey round!

  # Latest GDP observation
  nan_idx <- which(is.nan(data$rgdp))
  T_latest <- if (length(nan_idx) > 0) min(nan_idx) else NA
  T_latest <- T_latest - 1

  # Current quarter
  Q_id <- as.numeric(sub(".*Q", "", data$date[t]))

  # h-step ahead SPF-forecasts
  if (Q_id == 3) {

    # Read out relevant SPF data in Q3, check with Rike's function!!!
    SPF_Q <- c(1.9,0.8)

    # h = 0 and 1
    data_filter <- data[1:(t+5),]
    data_filter$SPF_Q[c(t+1,t+5)] <- SPF_Q
    data_filter <- data_filter[1:(t+1),]
    SPF_rt <- SPF_filter(data_filter$rgdp,data_filter$SPF_Q)
    data_filter <- cbind(data_filter,SPF_rt)
    data$SPF0_rt[t] <- data_filter$SPF_implied[t]
    data$SPF1_rt[t] <- data_filter$SPF_implied[t+1]

    # h = 2 and 3
    data_filter <- data[1:(t+5),]
    data_filter$SPF_Q[c(t+1,t+5)] <- SPF_Q
    data_filter <- data_filter[1:(t+5),] # ?
    SPF_rt <- SPF_filter(data_filter$rgdp,data_filter$SPF_Q)
    data_filter <- cbind(data_filter,SPF_rt)
    data$SPF2_rt[t] <- data_filter$SPF_implied[t+2]
    data$SPF3_rt[t] <- data_filter$SPF_implied[t+3]


  } else if (Q_id == 2) {


    # Read out relevant SPF data in Q2, check with Rike's function!!!
    SPF_Q <- c(1.9,0.8)

    # h = 0, 1, and 2
    data_filter <- data[1:(t+6),]
    data_filter$SPF_Q[c(t+2,t+6)] <- SPF_Q
    data_filter <- data_filter[1:(t+2),]
    SPF_rt <- SPF_filter(data_filter$rgdp,data_filter$SPF_Q)
    data_filter <- cbind(data_filter,SPF_rt)
    data$SPF0_rt[t] <- data_filter$SPF_implied[t]
    data$SPF1_rt[t] <- data_filter$SPF_implied[t+1]
    data$SPF2_rt[t] <- data_filter$SPF_implied[t+2]

    # h = 3
    data_filter <- data[1:(t+6),]
    data_filter$SPF_Q[c(t+2,t+6)] <- SPF_Q
    data_filter <- data_filter[1:(t+6),] # ?
    SPF_rt <- SPF_filter(data_filter$rgdp,data_filter$SPF_Q)
    data_filter <- cbind(data_filter,SPF_rt)
    data$SPF3_rt[t] <- data_filter$SPF_implied[t+3]

  } # here also Q = 1 and Q = 4

  data$rgdp[T_latest] <- NaN

}







gr <- test$SPF_implied
quarterly_growth <- (1 + gr / 100)^0.25
cumprod_test = cumprod(quarterly_growth)

gr1 = mean(cumprod_test[17:20])
gr2 = mean(cumprod_test[21:24])

gr2/gr1*100 - 100

T_all <- dim(data)[1]

# Find latest RGDP
nan_idx <- which(is.nan(data$rgdp))
first_nan_pos <- if (length(nan_idx) > 0) min(nan_idx) else NA

# Lag order with respect to the SPF
lag <- TT - first_nan_pos + 1


# Loop
t <- TT
#t <- t - 1

# Filter SPF
Q_id <- as.numeric(sub(".*Q", "", data$date[t]))
data_aux <- data[1:T_all,c(1,2,2+Q_id)]
#data_aux$rgdp[2:8] <- NaN
filter_data <- cbind(data_aux$rgdp,data_aux$spf_q3)
filter_data[1:20,2] <- NaN
filter_data <- filter_data[21:28,]
spf_test <- SPF_filter(filter_data[,1],filter_data[,2])
data_aux <- cbind(data_aux,spf_test)

# Read out h-step ahead forecasts
data$SPF0_rt[t] <- data_aux$SPF_implied[lag+1]
data$SPF1_rt[t] <- data_aux$SPF_implied[lag+2]
data$SPF2_rt[t] <- data_aux$SPF_implied[lag+3]
data$SPF3_rt[t] <- data_aux$SPF_implied[lag+4]

# Update index
T_all <- T_all - 1












































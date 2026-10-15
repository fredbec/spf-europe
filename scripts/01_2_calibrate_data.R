library(here)
library(data.table)
DT <- `[`

real_time_data <- fread(here("data", "processed", "revdatfull.csv")) |>
  DT(is.na(rgdp_growth), rgdp_growth := NaN) |>
  DT(is.na(rgdp_growth_ann), rgdp_growth_ann := NaN) |>
  DT(, imputed := NULL) |>
  split(by = c("origin_year", "origin_month", "origin_day"))


calc_approx_errors <- function(rtd_release){
  inds_ann <- which(!is.na(rtd_release$rgdp_growth_ann))
  inds_ann <- inds_ann[inds_ann>6]


  rtd_release$approx_annual <- as.numeric(NA)
  rtd_release$approx_error <- as.numeric(NA)
  rtd_release$sigma_err <- as.numeric(NA)
  rtd_release$approx_qonq <- as.numeric(NA)
  rtd_release$approx_qonq_error <- as.numeric(NA)
  rtd_release$sigma_qonq_err <- as.numeric(NA)

  for(ind in inds_ann){

    ann_val <- rtd_release$rgdp_growth_ann[ind]
    quar_vals <- rtd_release$rgdp_growth[(ind-6):ind]
    tri_weights <- 1/16 * c(1:4,3:1)

    rtd_release$approx_annual[ind] <- sum(tri_weights * quar_vals)
    rtd_release$approx_error[ind] <-
      ann_val - rtd_release$approx_annual[ind]
    rtd_release$sigma_err[ind] <- sd(rtd_release$approx_error[1:ind], na.rm = TRUE)


    quar_vals_fc <- rtd_release$rgdp_growth[(ind-3):ind]

    rtd_release$approx_qonq <- sum(quar_vals_fc)/4
    rtd_release$approx_qonq_error[ind] <-
      ann_val - rtd_release$approx_qonq[ind]
    rtd_release$sigma_qonq_err[ind] <- sd(rtd_release$approx_qonq_error[1:ind], na.rm = TRUE)
  }

  return(rtd_release)

}

approx_errors <- lapply(real_time_data, calc_approx_errors) |>
  rbindlist()

data.table::fwrite(approx_errors,here("data", "processed", "approximation_errors.csv"))

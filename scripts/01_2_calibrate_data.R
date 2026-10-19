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


ipdata_m <- data.table::fread(here("data", "processed", "ip_consolidated_monthly.csv")) |>
  setnames("ip", "ip_m") |>
  DT(, target_quarter := ifelse(target_month %% 3 == 0, target_month %/% 3, NA))

ipdata_q <- data.table::fread(here("data", "processed", "ip_consolidated.csv")) |>
  setnames("ip", "ip_q")

ipdata_all <- ipdata_q[ipdata_m, on = c("target_year", "target_quarter")]

inds_q <- which(!is.na(ipdata_all$ip_q))
inds_q <- inds_q[inds_q>4]

ipdata_all$approx_quarterly <- as.numeric(NA)
ipdata_all$approx_error <- as.numeric(NA)
ipdata_all$sigma_err <- as.numeric(NA)

for(ind in inds_q){
  q_val <- ipdata_all$ip_q[ind]
  m_vals <- ipdata_all$ip_m[(ind-4):ind]
  tri_weights <- 1/3 * c(1:3,2:1)

  ipdata_all$approx_quarterly[ind] <- sum(tri_weights * m_vals)
  ipdata_all$approx_error[ind] <-
    q_val - ipdata_all$approx_quarterly[ind]
  ipdata_all$sigma_err[ind] <- sd(ipdata_all$approx_error[1:ind], na.rm = TRUE)
}



data.table::fwrite(ipdata_all,here("data", "processed", "approximation_errors_ip.csv"))


library(here)
library(data.table)
#chain operator for data.table
DT <- `[`

spfdat <- fread(here("data", "spf_median_forecast.csv"))
spfdat_mean <- fread(here("data", "spf_mean_forecast.csv"))


SPF_dataUS <- fread(here("data", "spf_us_median_forecast.csv"))

IP_data <- fread(here("data", "ip_consolidated.csv")) |>
  DT(, origin_quarter := target_quarter + 1) |>
  DT(, origin_year := target_year) |>
  DT(, prediction := ip) |>
  DT(, ip := NULL)

#make a combination of all years and quarters, to loop over
quarters <- 1:4

years <- 2001:2024
combs <- CJ(year = years,
            quarter = quarters)

rtd <- fread(here("data", "revdatfull.csv")) |>
  #DT(, .SD, .SDcols = c("origin_year", "origin_month", "target_quarter", "target_year", "rgdp_growth")) |>
  DT(is.na(rgdp_growth), rgdp_growth := NaN)

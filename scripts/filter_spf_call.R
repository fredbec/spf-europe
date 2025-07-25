library(here)
library(data.table)

source(here("scripts", "filter_spf_functions.R"))
#chain operator for data.table
DT <- `[`

#adhoc calculation of growth rates
#!!!!!!!!!!!!! might be wrong !!!!!!!!!!!!!!
rtd <- fread(here("data", "revdatpre14.csv")) |>
  setorder(origin_year, origin_month, target_year, target_quarter) |>
  DT(, rgdp_growth := (rgdp / shift(rgdp,4) - 1) * 100,
     by = .(origin_year, origin_month)) |>
  DT(, rgdp := NULL) |>
  DT(!is.na(rgdp_growth))


#adhoc calculation of SPF ensemble forecasts
#!!!!!!!!!!!!! might be wrong !!!!!!!!!!!!!!
#median or mean forecast?
spfdat <- fread(here("data", "spf_consolidated.csv")) |>
  DT(type_target == "annual" & type_format == "POINT") |>
  DT(, horizon := target_year - forecast_year) |>
  DT(horizon <=1) |>
  DT(!is.na(prediction)) |>
  DT(, ens_fc := mean(prediction), by = c("target_year",
                                           "forecast_year",
                                           "forecast_quarter")) |>
  DT(, .SD, .SDcols = c("target_year",
                        "forecast_year",
                        "forecast_quarter",
                        "ens_fc")) |>
  unique()


#make a combination of all years and quarters, to loop over
quarters <- 1:4
years <- 2001:2013
combs <- CJ(year = years,
            quarter = quarters)

res_spf_filter <- vector(mode = "list", length = nrow(combs))
for(i in 1:nrow(combs)){

  cissue <- combs[i,]

  cqu <- cissue$quarter
  cyr <- cissue$year

  res_spf_filter[[i]] <- filter_dat(current_quarter = cqu,
                                    current_year = cyr,
                                    SPF_data = spfdat,
                                    real_time_data = rtd,
                                    rtd_issue = "latest_vintage")
}

res_spf_filter <- rbindlist(res_spf_filter)

data.table::fwrite(res_spf_filter, here("data", "filter_spf_data.csv"))

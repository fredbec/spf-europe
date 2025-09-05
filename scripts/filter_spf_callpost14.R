library(here)
library(data.table)

source(here("scripts", "filter_spf_functions.R"))
#chain operator for data.table
DT <- `[`

#adhoc calculation of growth rates
#!!!!!!!!!!!!! might be wrong !!!!!!!!!!!!!!
rtd <- fread(here("data", "revdatpost14.csv")) |>
  #filter out months with two vintage releases
  DT(, number := .N,
     by = c("origin_year", "origin_month", "target_year", "target_quarter")) |>
  DT(number == 1) |>
  setorder(origin_year, origin_month, origin_day, target_year, target_quarter) |>
  DT(, rgdp_growth := ((rgdp / shift(rgdp,1))^4 - 1) * 100,
     by = .(origin_year, origin_month, origin_day)) |>
  DT(, rgdp := NULL) |>
  DT(!is.na(rgdp_growth)) |>
  DT(, c("number", "origin_day") := NULL)|>
  DT(, flag := "post")

rtd_pre14 <- fread(here("data", "revdatpre14.csv")) |>
  setorder(origin_year, origin_month, target_year, target_quarter) |>
  DT(, rgdp_growth := ((rgdp / shift(rgdp,1))^4 - 1) * 100,
     by = .(origin_year, origin_month)) |>
  DT(, rgdp := NULL) |>
  DT(!is.na(rgdp_growth)) |>
  DT(, flag := "pre")

rtd <- rbind(rtd_pre14, rtd) |>
  DT(, number := .N,
     by = c("target_year", "target_quarter", "origin_year", "origin_month")) |>
  DT(, flag2 := (number > 1 & flag == "pre")) |>
  DT(flag2 == FALSE) |> # filter out duplicates
  DT(, c("number", "flag2", "flag") := NULL)

rtd_full <- CJ(origin_month = 1:12,
               origin_year = unique(rtd$origin_year),
               target_quarter = 1:4,
               target_year = unique(rtd$target_year))

rtd_full <- merge(rtd_full, rtd, by = c("origin_year", "origin_month",
                                        "target_quarter", "target_year"), all.x = TRUE) |>
  setorder(origin_year, origin_month) |>
  #kick out first observation (as growth rate is NA)
  DT(, fill := (target_quarter == 1 & target_year == 1991)) |>
  DT(fill == FALSE) |>
  DT(, fill := NULL)

#fill in missing values by carrying forward the last vintage
rtd_full[, rgdp_growth := zoo::na.locf(rgdp_growth, na.rm = FALSE),
        by = .(target_year, target_quarter)]

rtd <- rtd_full |>
  DT(target_year <= origin_year) |>
  DT(, rgdp_growth := ifelse(is.na(rgdp_growth), NaN, rgdp_growth)) |>
  setorder(origin_year, origin_month, target_year, target_quarter)

#adhoc calculation of SPF ensemble forecasts
#!!!!!!!!!!!!! might be wrong !!!!!!!!!!!!!!
#median or mean forecast?
spfdat <- fread(here("data", "spf_consolidated.csv")) |>
  DT(type_target == "annual" & type_format == "POINT") |>
  DT(, horizon := target_year - forecast_year) |>
  DT(horizon <=1) |>
  DT(!is.na(prediction)) |>
  DT(, ens_fc := median(prediction), by = c("target_year",
                                          "forecast_year",
                                          "forecast_quarter")) |>
  DT(, .SD, .SDcols = c("target_year",
                        "forecast_year",
                        "forecast_quarter",
                        "ens_fc")) |>
  unique()


SPF_dataUS <- fread(here("data", "spf_us_consolidated.csv"))

#make a combination of all years and quarters, to loop over
quarters <- 1:4

years <- 2001:2024
combs <- CJ(year = years,
            quarter = quarters)

res_spf_filter <- vector(mode = "list", length = nrow(combs))

#without US SPF
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

data.table::fwrite(res_spf_filter, here("data", "filter_spf_data_medianfc.csv"))


res_spf_filter <- vector(mode = "list", length = nrow(combs))

#with US SPF, "latest"
for(i in 1:nrow(combs)){

  cissue <- combs[i,]

  cqu <- cissue$quarter
  cyr <- cissue$year

  res_spf_filter[[i]] <- filter_dat(current_quarter = cqu,
                                    current_year = cyr,
                                    SPF_data = spfdat,
                                    real_time_data = rtd,
                                    SPF_data_US = SPF_dataUS,
                                    release_US_SPF = "latest",
                                    rtd_issue = "latest_vintage")
}

res_spf_filter <- rbindlist(res_spf_filter)

data.table::fwrite(res_spf_filter, here("data", "filter_spf_data_medianfc_withus.csv"))
#with US SPF, fixed release
for(rel_hor in 0:3){
  res_spf_filter <- vector(mode = "list", length = nrow(combs))
  for(i in 1:nrow(combs)){

    cissue <- combs[i,]

    cqu <- cissue$quarter
    cyr <- cissue$year
    print(cissue)
    res_spf_filter[[i]] <- filter_dat(current_quarter = cqu,
                                      current_year = cyr,
                                      SPF_data = spfdat,
                                      real_time_data = rtd,
                                      SPF_data_US = SPF_dataUS,
                                      release_US_SPF = rel_hor,
                                      rtd_issue = "latest_vintage")
  }

  res_spf_filter <- rbindlist(res_spf_filter)

  data.table::fwrite(res_spf_filter, here("data", paste0("filter_spf_data_medianfc_withus_stepahead", rel_hor,  ".csv")))
}


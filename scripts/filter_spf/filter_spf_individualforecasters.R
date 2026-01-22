library(here)
library(data.table)

source(here("scripts", "filter_spf", "filter_spf_functions.R"))
source(here("scripts", "filter_spf", "filter_spf_loaddata.R"))
#chain operator for data.table
DT <- `[`


spf_indiv <- fread(here("data", "spf_consolidated.csv")) |>
  DT(type_format == "POINT") |>
  DT(type_target == "annual") |>
  DT(forecast_year >= 2001) |>
  setnames("prediction", "ens_fc") #for compatibility


res_spf_filter <- vector(mode = "list", length = nrow(spf_indiv))
res_spf_additionalinfo <- vector(mode = "list", length = nrow(spf_indiv))
for(i in 1:nrow(combs)){

  cissue <- combs[i,]

  cqu <- cissue$quarter
  cyr <- cissue$year

  fcsts <- spf_indiv |>
    copy() |>
    DT(forecast_year == cyr & forecast_quarter == cqu, forecaster_id)

  spf_curr <- spf_indiv |>
    copy() |>
    DT(forecast_year == cyr & forecast_quarter == cqu)

  res_spf_filter_indiv <- vector(mode = "list", length = length(fcsts))
  res_spf_additionalinfo_indiv <- vector(mode = "list", length = length(fcsts))
  print(length(fcsts))
  for(k in 1:length(fcsts)){
    fcid <- fcsts[k]

    spf_curr_fcid <- spf_curr |>
      DT(forecaster_id == fcid)

    res <- filter_dat(current_quarter = cqu,
                      current_year = cyr,
                      SPF_data = spf_curr_fcid,
                      real_time_data = rtd,
                      rtd_issue = "latest_vintage")

    res_spf_filter_indiv[[k]] <- res$spf_filter_dat |>
      DT(, forecaster_id := fcid)
    res_spf_additionalinfo_indiv[[k]] <- data.table(forecaster_id = fcid,
                                                    origin_quarter = cqu,
                                                    origin_year = cyr,
                                                    cy_logLik = res$ll["cy"],
                                                    cyandny_logLik = res$ll["cyandny"],
                                                    cy_rw_sd = res$params$cy["rw_sd"],
                                                    cyandny_rw_sd = res$params$cyandny["rw_sd"])
  }
  res_spf_filter_indiv <- rbindlist(res_spf_filter_indiv)
  res_spf_additionalinfo_indiv <- rbindlist(res_spf_additionalinfo_indiv)

  res_spf_filter[[i]] <- res_spf_filter_indiv
  res_spf_additionalinfo[[i]] <- res_spf_additionalinfo_indiv
}
res_spf_filter <- rbindlist(res_spf_filter)
res_spf_additionalinfo <- rbindlist(res_spf_additionalinfo)

data.table::fwrite(res_spf_filter, here("data", paste0("filter_spf_data_individual", ".csv")))
data.table::fwrite(res_spf_additionalinfo, here("data", paste0("filter_spf_data_individual_supplementary", ".csv")))


countdat <- res_spf_filter |>
  DT(, .SD, .SDcols = c("origin_quarter", "origin_year", "forecaster_id")) |>
  unique() |>
  DT(, count := .N, by = c("forecaster_id")) |>
  DT(, .SD, .SDcols = c("forecaster_id", "count")) |>
  unique()

data.table::fwrite(countdat, here("data", paste0("filter_spf_data_individual_counts", ".csv")))

library(here)
library(data.table)

source(here("scripts", "filter_spf_functions.R"))
#chain operator for data.table
DT <- `[`

spfdat <- fread(here("data", "spf_median_forecast.csv"))
data.table::fwrite(spfdat, here("data", "spf_median_forecast.csv"))

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

#without US SPF
for(apprerr in c(0.001, 0.005, 0.01, 0.05, 0.1)){
  res_spf_filter <- vector(mode = "list", length = nrow(combs))
  res_spf_additionalinfo <- vector(mode = "list", length = nrow(combs))
  for(i in 1:nrow(combs)){

    cissue <- combs[i,]

    cqu <- cissue$quarter
    cyr <- cissue$year

    res <- filter_dat(current_quarter = cqu,
                      current_year = cyr,
                      SPF_data = spfdat,
                      real_time_data = rtd,
                      rtd_issue = "latest_vintage",
                      approx_err = apprerr)

    res_spf_filter[[i]] <- res$spf_filter_dat
    res_spf_additionalinfo[[i]] <- data.table(origin_quarter = cqu,
                                              origin_year = cyr,
                                              cy_logLik = res$ll["cy"],
                                              cyandny_logLik = res$ll["cyandny"],
                                              cy_rw_sd = res$params$cy["rw_sd"],
                                              cyandny_rw_sd = res$params$cyandny["rw_sd"])
  }
  res_spf_filter <- rbindlist(res_spf_filter)
  res_spf_additionalinfo <- rbindlist(res_spf_additionalinfo)

  data.table::fwrite(res_spf_filter, here("data", paste0("filter_spf_data_medianfc_", "approx_err", 1000*apprerr, ".csv")))
  data.table::fwrite(res_spf_additionalinfo, here("data", paste0("filter_spf_data_medianfc_supplementary", "approx_err", 1000*apprerr,".csv")))
}


res_spf_filter <- vector(mode = "list", length = nrow(combs))
res_spf_additionalinfo <- vector(mode = "list", length = nrow(combs))

for(shft in -1:1){
  res_spf_filter <- vector(mode = "list", length = nrow(combs))
  res_spf_additionalinfo <- vector(mode = "list", length = nrow(combs))
#with US SPF, "latest"
  for(i in 1:nrow(combs)){

    cissue <- combs[i,]

    cqu <- cissue$quarter
    cyr <- cissue$year

    res <- filter_dat(current_quarter = cqu,
                      current_year = cyr,
                      SPF_data = spfdat,
                      real_time_data = rtd,
                      SPF_data_US = SPF_dataUS,
                      release_US_SPF = "latest",
                      rtd_issue = "latest_vintage",
                      rtd_shift = shft)



    res_spf_filter[[i]] <- res$spf_filter_dat
    res_spf_additionalinfo[[i]] <- data.table(origin_quarter = cqu,
                                            origin_year = cyr,
                                            cy_logLik = res$ll["cy"],
                                            cyandny_logLik = res$ll["cyandny"],
                                            cy_rw_sd = res$params$cy["rw_sd"],
                                            cyandny_rw_sd = res$params$cyandny["rw_sd"],
                                            cy_rw_us_sd = res$params$cy["rw_us_sd"],
                                            cyandny_rw_us_sd = res$params$cyandny["rw_us_sd"])
  }
  res_spf_filter <- rbindlist(res_spf_filter)
  res_spf_additionalinfo <- rbindlist(res_spf_additionalinfo)

  data.table::fwrite(res_spf_filter, here("data", paste0("filter_spf_data_medianfc_withus", "_month", shft+2, ".csv")))
  data.table::fwrite(res_spf_additionalinfo, here("data", paste0("filter_spf_data_medianfc_withus_supplementary", "_month", shft+2, ".csv")))
}

for(shft in -1:1){
#with US SPF, fixed release
  for(rel_hor in 0:3){
    res_spf_filter <- vector(mode = "list", length = nrow(combs))
    res_spf_additionalinfo <- vector(mode = "list", length = nrow(combs))
    for(i in 1:nrow(combs)){

      cissue <- combs[i,]

      cqu <- cissue$quarter
      cyr <- cissue$year
      res <- filter_dat(current_quarter = cqu,
                        current_year = cyr,
                        SPF_data = spfdat,
                        real_time_data = rtd,
                        SPF_data_US = SPF_dataUS,
                        release_US_SPF = rel_hor,
                        rtd_issue = "latest_vintage",
                        rtd_shift = shft)

      res_spf_filter[[i]] <- res$spf_filter_dat
      res_spf_additionalinfo[[i]] <- data.table(origin_quarter = cqu,
                                                origin_year = cyr,
                                                cy_logLik = res$ll["cy"],
                                                cyandny_logLik = res$ll["cyandny"],
                                                cy_rw_sd = res$params$cy["rw_sd"],
                                                cyandny_rw_sd = res$params$cyandny["rw_sd"],
                                                cy_rw_us_sd = res$params$cy["rw_us_sd"],
                                                cyandny_rw_us_sd = res$params$cyandny["rw_us_sd"])
    }

    res_spf_filter <- rbindlist(res_spf_filter)
    res_spf_additionalinfo <- rbindlist(res_spf_additionalinfo)


    data.table::fwrite(res_spf_filter, here("data", paste0("filter_spf_data_medianfc_withus_stepahead", rel_hor, "_month", shft+2,  ".csv")))
    data.table::fwrite(res_spf_additionalinfo, here("data", paste0("filter_spf_data_medianfc_withus_stepahead", rel_hor, "_month", shft+2,"_supplementary.csv")))

  }
}

for(shft in -1:1){
  res_spf_filter <- vector(mode = "list", length = nrow(combs))
  res_spf_additionalinfo <- vector(mode = "list", length = nrow(combs))

  #with US SPF, "latest" plus gamma estimation
  for(i in 1:nrow(combs)){

    cissue <- combs[i,]
    print(cissue)

    cqu <- cissue$quarter
    cyr <- cissue$year

    res <- filter_dat(current_quarter = cqu,
                      current_year = cyr,
                      SPF_data = spfdat,
                      real_time_data = rtd,
                      SPF_data_US = SPF_dataUS,
                      release_US_SPF = "latest",
                      est_gamma = TRUE,
                      rtd_issue = "latest_vintage",
                      rtd_shift = shft)



    res_spf_filter[[i]] <- res$spf_filter_dat
    res_spf_additionalinfo[[i]] <- data.table(origin_quarter = cqu,
                                              origin_year = cyr,
                                              cy_logLik = res$ll["cy"],
                                              cyandny_logLik = res$ll["cyandny"],
                                              cy_rw_sd = res$params$cy["rw_sd"],
                                              cy_gamma = res$params$cy["gamma"],
                                              cyandny_rw_sd = res$params$cyandny["rw_sd"],
                                              cy_rw_us_sd = res$params$cy["rw_us_sd"],
                                              cyandny_rw_us_sd = res$params$cyandny["rw_us_sd"],
                                              cyandny_gamma = res$params$cyandny["gamma"])
  }

  res_spf_filter <- rbindlist(res_spf_filter)
  res_spf_additionalinfo <- rbindlist(res_spf_additionalinfo)

  data.table::fwrite(res_spf_filter, here("data", "filter_spf_data_medianfc_withus_withgammaest", "_month", shft+2, ".csv"))
  data.table::fwrite(res_spf_additionalinfo, here("data", "filter_spf_data_medianfc_withus_withgammaest", "_month", shft+2, "_supplementary.csv"))

}


#with US SPF, fixed release
for(rel_hor in 0:3){
  res_spf_filter <- vector(mode = "list", length = nrow(combs))
  res_spf_additionalinfo <- vector(mode = "list", length = nrow(combs))
  for(i in 1:nrow(combs)){

    cissue <- combs[i,]

    cqu <- cissue$quarter
    cyr <- cissue$year
    print(cissue)
    res <- filter_dat(current_quarter = cqu,
                      current_year = cyr,
                      SPF_data = spfdat,
                      real_time_data = rtd,
                      SPF_data_US = SPF_dataUS,
                      release_US_SPF = rel_hor,
                      est_gamma = TRUE,
                      rtd_issue = "latest_vintage")

    res_spf_filter[[i]] <- res$spf_filter_dat
    res_spf_additionalinfo[[i]] <- data.table(origin_quarter = cqu,
                                              origin_year = cyr,
                                              cy_logLik = res$ll["cy"],
                                              cyandny_logLik = res$ll["cyandny"],
                                              cy_rw_sd = res$params$cy["rw_sd"],
                                              cy_gamma = res$params$cy["gamma"],
                                              cyandny_rw_sd = res$params$cyandny["rw_sd"],
                                              cy_rw_us_sd = res$params$cy["rw_us_sd"],
                                              cyandny_rw_us_sd = res$params$cyandny["rw_us_sd"],
                                              cyandny_gamma = res$params$cyandny["gamma"])
  }

  res_spf_filter <- rbindlist(res_spf_filter)
  res_spf_additionalinfo <- rbindlist(res_spf_additionalinfo)


  data.table::fwrite(res_spf_filter, here("data", paste0("filter_spf_data_medianfc_withus_withgammaest_stepahead", rel_hor,  ".csv")))
  data.table::fwrite(res_spf_additionalinfo, here("data", paste0("filter_spf_data_medianfc_withus_withgammaest_stepahead", rel_hor,  "_supplementary.csv")))

}


res_spf_filter <- vector(mode = "list", length = nrow(combs))
res_spf_additionalinfo <- vector(mode = "list", length = nrow(combs))

#with IP data SPF, "latest" plus gamma estimation
for(i in 1:nrow(combs)){

  cissue <- combs[i,]
  print(cissue)

  cqu <- cissue$quarter
  cyr <- cissue$year

  res <- filter_dat(current_quarter = cqu,
                    current_year = cyr,
                    SPF_data = spfdat,
                    real_time_data = rtd,
                    SPF_data_US = IP_data,
                    release_US_SPF = -1,
                    est_gamma = TRUE,
                    rtd_issue = "latest_vintage")



  res_spf_filter[[i]] <- res$spf_filter_dat
  res_spf_additionalinfo[[i]] <- data.table(origin_quarter = cqu,
                                            origin_year = cyr,
                                            cy_logLik = res$ll["cy"],
                                            cyandny_logLik = res$ll["cyandny"],
                                            cy_rw_sd = res$params$cy["rw_sd"],
                                            cy_gamma = res$params$cy["gamma"],
                                            cyandny_rw_sd = res$params$cyandny["rw_sd"],
                                            cy_rw_us_sd = res$params$cy["rw_us_sd"],
                                            cyandny_rw_us_sd = res$params$cyandny["rw_us_sd"],
                                            cyandny_gamma = res$params$cyandny["gamma"])
}

res_spf_filter <- rbindlist(res_spf_filter)
res_spf_additionalinfo <- rbindlist(res_spf_additionalinfo)

data.table::fwrite(res_spf_filter, here("data", "filter_spf_data_medianfc_withip_withgammaest.csv"))
data.table::fwrite(res_spf_additionalinfo, here("data", "filter_spf_data_medianfc_withip_withgammaest_supplementary.csv"))

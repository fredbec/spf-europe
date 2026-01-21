library(here)
library(data.table)

source(here("scripts", "filter_spf", "filter_spf_functions.R"))
source(here("scripts", "filter_spf", "filter_spf_loaddata.R"))
#chain operator for data.table
DT <- `[`


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

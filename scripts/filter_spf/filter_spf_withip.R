


res_spf_filter <- vector(mode = "list", length = nrow(combs))
res_spf_additionalinfo <- vector(mode = "list", length = nrow(combs))

#with IP data SPF, "latest" plus gamma estimation
for(i in 1:nrow(combs)){

  cissue <- combs[i,]

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

data.table::fwrite(res_spf_filter, here("data", paste0("filter_spf_data_medianfc_withip_withgammaest.csv")))
data.table::fwrite(res_spf_additionalinfo, here("data", paste0("filter_spf_data_medianfc_withip_withgammaest_supplementary.csv")))

library(here)
library(data.table)
library(stringr)

#chain operator for data.table
DT<- `[`

#first and last release available
start_release <- "1999Q1"
end_release <- "2024Q4"


#compute year and quarter
#and "mix": e.g. 2015.25 will be first quarter in 2025
#"mix", since it's easier to work with pure numeric
start_yr <- substr(start_release, 1, 4) |> as.numeric()
start_qrtr <- substr(start_release, 6, 6) |> as.numeric()
start_release_num <- start_yr + start_qrtr/4

end_yr <- substr(end_release, 1, 4) |> as.numeric()
end_qrtr <- substr(end_release, 6, 6) |> as.numeric()
end_release_num <- end_yr + end_qrtr/4

allqs <- vector(mode = "list", length = 100)
i <- 0

# go through all releases and check which horizons are present
for(release in seq(start_release_num, end_release_num, 0.25)){

  i <- i + 1

  qnum <- ifelse(release%%1 * 4 == 0, 4, release%%1 * 4)
  yrnum <- ifelse(release%%1 == 0, floor(release)-1, floor(release))

  origin_name <- paste0(yrnum,
                        "Q",
                        qnum)

  origin_yr <- substr(origin_name, 1, 4) |> as.numeric()
  origin_qrtr <- substr(origin_name, 6, 6) |> as.numeric()

  spf_curr <- data.table::fread(here("data", "spf_raw_data", paste0(origin_name, ".csv")),
                               skip = 1,
                               header = TRUE)

  startrow <- which(spf_curr$TARGET_PERIOD == "GROWTH EXPECTATIONS; YEAR-ON-YEAR CHANGE IN REAL GDP")
  spf_curr <- spf_curr[(startrow+5):1000,]
  endrow <- which(spf_curr$TARGET_PERIOD == "EXPECTED UNEMPLOYMENT RATE; PERCENTAGE OF LABOUR FORCE")
  if(length(endrow)>0){ #after ~2015, unemployment rate is not in data anymore
    spf_curr <- spf_curr[1:(endrow-2),]
  }

  tperiods <- unique(spf_curr$TARGET_PERIOD)

  qvalues <- tperiods[grepl("Q", tperiods)] |> as.list()

  allqs[[i]] <- lapply(qvalues, function(qval){

    yr <- substr(qval, 1, 4)|> as.numeric()
    qrter <- substr(qval, 6, 6) |> as.numeric()

    num_resp <- sum(spf_curr$TARGET_PERIOD==qval)

    return(data.table(
      forecast_year = origin_yr, forecast_quarter = origin_qrtr,
      target_year = yr, target_quarter = qrter,
      num_respones = num_resp))

  }) |>
    rbindlist()


}
#concatenate all data.tables and compute
allqs <- rbindlist(allqs) |>
  DT(, forecast_origin := forecast_year + forecast_quarter*0.25) |>
  DT(, target_point := target_year + target_quarter*0.25) |>
  DT(, diffin := target_point - forecast_origin)


data.table::fwrite(allqs, here("data", "spf_availability_quartersbyhor.csv"))

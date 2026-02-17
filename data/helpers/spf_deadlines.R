library(here)
library(data.table)

DT <- `[`

deadlines <- fread(here("data", "helpers", "spf_deadlines_raw.csv")) |>
  DT(, sentout := as.Date(sentout, "%d/%m/%Y")) |>
  DT(, deadline := as.Date(deadline, "%d/%m/%Y")) |>
  DT(, published := as.Date(published, "%d/%m/%Y")) |>
  DT(, origin_year := as.numeric(substr(survey, 1, 4))) |>
  DT(, origin_quarter := as.numeric(substr(survey, 6,6))) |>
  DT(, survey := NULL) |>
  DT(origin_year >= 2001) |>
  DT(, .SD, .SDcols = c("deadline", "origin_year", "origin_quarter"))


rtd <- fread(here("data", "processed", "revdatfull.csv")) |>
  DT(, origin_date := as.Date(paste(origin_year, origin_month, origin_day,sep="-"), "%Y-%m-%d"))

rtd_vec <- rtd$origin_date |> unique()

deadlines_vec <- deadlines$deadline

match_deadlines <- sapply(deadlines_vec, function(ddl){

  idx <- max(which(rtd_vec <= ddl))

  return(rtd_vec[idx])

})

deadlines$closest_rtd_release <- as.Date(match_deadlines)

deadlines$deadline <- NULL


###Change closest release manually for 2002Q2 survey
#this is not totally clean, but justified. The SPF assumes that a value for two
#quarters prior is available to respondents (because of the fixed horizon forecast
#for two quarters ahead. 2002-04 is the first release than contains a value for Q4-2001
#we don't know the release DAY (only month and year) for releases prior to 2014
#but clearly they must have had a value, ergo the release for April must have
#come out sooner than the 30th
deadlines[6,3] <- as.Date("2002-04-30")

data.table::fwrite(deadlines, here("data", "helpers", "spf_deadlines_match_rtd.csv"))



### Check which quarter was available at each point
deadlines <- deadlines |>
  setnames(c("origin_year", "origin_quarter", "closest_rtd_release"), c("origin_year_spf", "origin_quarter_spf", "origin_date"))

rtd <- rtd |>
  merge(deadlines, by = "origin_date") |>
  DT(!is.na(rgdp_growth)) |>
  DT(order(-target_year, -target_quarter), .SD[1], by = .(origin_year_spf, origin_quarter_spf)) |>
  DT(, spf_instance := origin_year_spf + (origin_quarter_spf-1)*0.25) |>
  DT(, rtd_instance := target_year + (target_quarter-1)*0.25) |>
  DT(, diffvals := spf_instance - rtd_instance)

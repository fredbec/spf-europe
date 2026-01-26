library(here)
library(data.table)
library(lubridate)

#chain operator for data.table
DT <- `[`

#first and last release available
start_release <- "200101"
end_release <- "201412"

#compute year and month
start_yr <- substr(start_release, 1, 4) |> as.numeric()
start_mth <- substr(start_release, 5, 6) |> as.numeric()
#start_release_num <- start_yr + start_qrtr/4

end_yr <- substr(end_release, 1, 4) |> as.numeric()
end_mth <- substr(end_release, 5, 6) |> as.numeric()
#end_release_num <- end_yr + end_qrtr/4

#read in variable names
varnames <- fread(here("data", "helpers", "rtd_varnames.csv"))

#make
reslist <- vector(mode = "list", length = length(start_yr:end_yr)*12)
j <- 0
for(yrnum in start_yr:end_yr){

  for(mnum in 1:12){

    c_varname <- varnames[year == yrnum & month == mnum]$varname

    mnum <- ifelse(mnum < 10, paste0("0",mnum), paste0(mnum))


    j <- j+1

    origin_name <- paste0(yrnum, mnum)

    reslist[[j]] <- read.csv(here("data", "raw", "rtd_quarterly", paste0("quarterly_", origin_name, ".csv")),
                         skip = 1,
                         header = TRUE) |>
      as.data.table() |>
      DT(, c("Name.", c_varname), with = FALSE) |>
      DT(3:.N,) |>
      setnames("Name.", "target") |>
      setnames(c_varname, "rgdp") |>
      DT(, target_quarter := as.numeric(substr(target, 2,2))) |>
      DT(, target_year := as.numeric(substr(target, 4, 8))) |>
      DT(, target := NULL) |>
      DT(, origin_year := yrnum) |>
      DT(, origin_month := as.numeric(mnum)) |>
      DT(!grepl("NA*", rgdp)) |>
      DT(, rgdp := as.numeric(rgdp))


  }

}

rtd_pre14 <- rbindlist(reslist)

data.table::fwrite(rtd_pre14, here("data", "processed", "revdatpre14.csv"))

### real-time vintages starting from 2014, downloaded from
## https://ec.europa.eu/eurostat/databrowser/view/ei_na_q_vtg__custom_16834433/default/table?lang=en
rtd_post14 <- fread(here("data", "raw", "estat_ei_na_q_vtg_filtered_en.csv")) |>
  DT(, .SD, .SDcols = c("revdate", "TIME_PERIOD", "OBS_VALUE")) |>
  DT(, target_year := as.numeric(substr(TIME_PERIOD, 1, 4))) |>
  DT(, target_quarter := as.numeric(substr(TIME_PERIOD, 7,7))) |>
  setnames("OBS_VALUE", "rgdp") |>
  DT(, origin_year := lubridate::year(revdate)) |>
  DT(, origin_month := lubridate::month(revdate)) |>
  DT(, origin_day := lubridate::day(revdate)) |>
  DT(, .SD, .SDcols = c("rgdp", "target_year", "target_quarter", "origin_year", "origin_month", "origin_day"))


data.table::fwrite(rtd_post14, here("data", "processed", "revdatpost14.csv"))


### consolidate all into one dataset with growth rates

rtd_post14 <- rtd_post14 |>
  #filter out months with two vintage releases
  DT(, number := .N,
     by = c("origin_year", "origin_month", "target_year", "target_quarter")) |>
  DT(number == 1) |>
  setorder(origin_year, origin_month, origin_day, target_year, target_quarter) |>
  DT(, rgdp_growth := ((rgdp / shift(rgdp,1))^4 - 1) * 100,
     by = .(origin_year, origin_month, origin_day)) |>
  #DT(, rgdp := NULL) |>
  DT(!is.na(rgdp_growth)) |>
  DT(, c("number", "origin_day") := NULL)|>
  DT(, flag := "post")

rtd_pre14 <- rtd_pre14 |>
  setorder(origin_year, origin_month, target_year, target_quarter) |>
  DT(, rgdp_growth := ((rgdp / shift(rgdp,1))^4 - 1) * 100,
     by = .(origin_year, origin_month)) |>
  #DT(, rgdp := NULL) |>
  DT(!is.na(rgdp_growth)) |>
  DT(, flag := "pre")

rtd <- rbind(rtd_pre14, rtd_post14) |>
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

rtd_full[, rgdp := zoo::na.locf(rgdp, na.rm = FALSE),
         by = .(target_year, target_quarter)]

rtd <- rtd_full |>
  DT(target_year <= origin_year) |>
  DT(, rgdp_growth := ifelse(is.na(rgdp_growth), NaN, rgdp_growth)) |>
  setorder(origin_year, origin_month, target_year, target_quarter)


rtd <- rtd |>
  DT(, rgdp_growth_ann := ifelse(
    target_quarter == 4,
    ((rgdp + shift(rgdp,1) + shift(rgdp,2) + shift(rgdp,3)) /
       (shift(rgdp,4) + shift(rgdp,5) + shift(rgdp,6) + shift(rgdp,7)) - 1) * 100,
    NA_real_
  ),
  by = .(origin_year, origin_month)) |>
  DT(, rgdp := NULL)


data.table::fwrite(rtd, here("data", "processed", "revdatfull.csv"))

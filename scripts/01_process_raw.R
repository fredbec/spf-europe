library(here)
library(data.table)

#chain operator for data.table
DT <- `[`

process_rtd <- function() {

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
      stopifnot(length(c_varname) == 1)

      mnum_char <- sprintf("%02d", mnum)


      j <- j+1

      origin_name <- paste0(yrnum, mnum_char)

      # use read.csv because fread() fails due to ragged rows (inconsistent
      # column counts across vintages)
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
        DT(, origin_month := mnum) |>
        DT(trimws(rgdp) != "NA") |>
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
    DT(!is.na(rgdp_growth)) |>
    DT(, c("number") := NULL)|>
    DT(origin_year >= 2015) #potentially change later

  rtd_pre14 <- rtd_pre14 |>
    setorder(origin_year, origin_month, target_year, target_quarter) |>
    DT(, rgdp_growth := ((rgdp / shift(rgdp,1))^4 - 1) * 100,
       by = .(origin_year, origin_month)) |>
    DT(!is.na(rgdp_growth)) |>
    DT(, origin_day := ifelse(origin_month == 2, 28,
                              ifelse(origin_month %in% c(1,3,5,7,8,10,12), 31,
                                     30)))

  rtd <- rbind(rtd_pre14, rtd_post14) |>
    DT(, imputed := FALSE)

  revdates <- rtd |>
    DT(, .SD, .SDcols = c("origin_year", "origin_month", "origin_day")) |>
    unique()

  #some revisions are not included as they
  fictional_revdates <- CJ(origin_month = 1:12,
                           origin_year = unique(rtd$origin_year)) |>
    DT(, origin_day := ifelse(origin_month == 2, 28,
                              ifelse(origin_month %in% c(1,3,5,7,8,10,12), 31,
                                     30)))

  revdates <- rbind(revdates, fictional_revdates) |>
    unique() |>
    DT(, tmp := 1)

  targets <- CJ(
    target_quarter = 1:4,
    target_year = unique(rtd$target_year)
  ) |>
    DT(, tmp := 1)

  # Cartesian join
  rtd_full <- revdates[
    targets,
    on = "tmp",
    allow.cartesian = TRUE
  ] |>
    DT(, tmp := NULL)

  rtd_full <- merge(rtd_full, rtd, by = c("origin_year", "origin_month", "origin_day",
                                          "target_quarter", "target_year"), all.x = TRUE) |>
    setorder(origin_year, origin_month, origin_day) |>
    #kick out first observation (as growth rate is NA)
    DT(, fill := (target_quarter == 1 & target_year == 1991)) |>
    DT(fill == FALSE) |>
    DT(, fill := NULL) |>
    DT(is.na(imputed), imputed := TRUE)

  #fill in missing values by carrying forward the last vintage
  rtd_full[, rgdp_growth := zoo::na.locf(rgdp_growth, na.rm = FALSE),
           by = .(target_year, target_quarter)]

  rtd_full[, rgdp := zoo::na.locf(rgdp, na.rm = FALSE),
           by = .(target_year, target_quarter)]

  rtd <- rtd_full |>
    DT(target_year <= origin_year) |>
    DT(, rgdp_growth := ifelse(is.na(rgdp_growth), NaN, rgdp_growth)) |>
    setorder(origin_year, origin_month, origin_day, target_year, target_quarter)


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
}

process_rtd()


process_spf <- function() {

  #chain operator for data.table
  DT <- `[`

  #first and last release available
  start_release <- "1999Q1"
  end_release <- "2024Q4"


  ##data with concrete release dates
  reldates <- read.delim(here("data", "raw", "spf_release_dates.txt"), sep = " ") |>
    as.data.table() |>
    setnames("Date", "reldate") |>
    setnames("Deadline", "release_deadline") |>
    setnames("Publication", "release_publication") |>
    DT(, .SD, .SDcols = c("reldate", "release_deadline", "release_publication"))


  #compute year and quarter
  #and "mix": e.g. 2015.25 will be first quarter in 2025
  #"mix", since it's easier to work with pure numeric
  start_yr <- substr(start_release, 1, 4) |> as.numeric()
  start_qrtr <- substr(start_release, 6, 6) |> as.numeric()
  start_release_num <- start_yr + start_qrtr/4

  end_yr <- substr(end_release, 1, 4) |> as.numeric()
  end_qrtr <- substr(end_release, 6, 6) |> as.numeric()
  end_release_num <- end_yr + end_qrtr/4

  alldata <- vector(mode = "list", length = 104)
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

    spf_curr <- data.table::fread(here("data", "raw", "spf_surveys", paste0(origin_name, ".csv")),
                                  skip = 1,
                                  header = TRUE)

    startrow <- which(spf_curr$TARGET_PERIOD == "GROWTH EXPECTATIONS; YEAR-ON-YEAR CHANGE IN REAL GDP")
    names(spf_curr) <- unlist(spf_curr[startrow + 1,])
    spf_curr <- spf_curr[(startrow+2):1000,]
    endrow <- which(spf_curr$TARGET_PERIOD == "EXPECTED UNEMPLOYMENT RATE; PERCENTAGE OF LABOUR FORCE")
    if(length(endrow)>0){ #after ~2015, unemployment rate is not in data anymore
      spf_curr <- spf_curr[1:(endrow-2),]
    }
    #delete empty column
    nmtodel <- names(spf_curr) == ""
    spf_curr[, which(nmtodel)] <- NULL

    spf_long <- spf_curr |>
      setnames("TARGET_PERIOD", "target_id") |>
      setnames("FCT_SOURCE", "forecaster_id") |>
      melt(id.vars = c("target_id", "forecaster_id"),
           variable.name = "type_format",
           value.name = "prediction") |>
      DT(, type_target := ifelse(grepl("Q", target_id), "quarterly", "annual")) |>
      DT(, target_year := as.numeric(substr(target_id, 1, 4))) |>
      DT(, target_quarter := as.numeric(substr(target_id, 6, 6))) |>
      DT(, target_id := ifelse(type_target == "annual", paste0(target_id, "A"), target_id)) |>
      DT(, forecast_year := yrnum) |>
      DT(, forecast_quarter := qnum) |>
      DT(, .SD, .SDcols = c("forecaster_id", "forecast_year", "forecast_quarter",
                            "target_id", "target_year", "target_quarter",
                            "type_target", "type_format", "prediction")) |>
      DT(, prediction := as.numeric(prediction))

    alldata[[i]] <- spf_long

  }
  #concatenate all data.tables and compute
  alldata <- rbindlist(alldata) |>
    DT(,reldate := paste0(forecast_year, "Q", forecast_quarter))

  #merge data with release dates
  alldata <- reldates[alldata, on = "reldate"] |>
    DT(, .SD, .SDcols = c("forecaster_id", "forecast_year", "forecast_quarter",
                          "target_id", "target_year", "target_quarter",
                          "type_target", "type_format",
                          "release_deadline", "release_publication", "prediction"))

  data.table::fwrite(alldata, here("data", "processed", "spf_consolidated.csv"))

  #individual forecasts
  spfdat_individual <- alldata |>
    copy() |>
    DT(type_format == "POINT") |>
    DT(forecast_year >= 2001) |>
    DT(!is.na(prediction)) |>
    setnames("prediction", "ens_fc") |>
    DT(, horizon := target_year - forecast_year) |>
    DT(horizon <= 1) |>
    DT(, horizon := NULL)

  spfdat_individual_annual <- spfdat_individual |>
    copy() |>
    DT(type_target == "annual")|>
    DT(, .SD, .SDcols = c("forecaster_id",
                          "target_year",
                          "forecast_year",
                          "forecast_quarter",
                          "ens_fc"))

  spfdat_individual_annualandquarterly <- spfdat_individual |>
    copy() |>
    DT(, fc_inst := forecast_year + 0.25*(forecast_quarter-1)) |>
    DT(, tg_inst := target_year + 0.25*(target_quarter-1)) |>
    DT(type_target == "quarterly", horizon := tg_inst - fc_inst) |>
    DT(is.na(horizon) | horizon == 0.5) |>
    DT(, count := .N, by = c("forecast_year", "forecast_quarter", "forecaster_id")) |>
    DT(count == 3)  |> #only keep instances with all 3 forecast (annual c, annual n, quarterly)
    DT(, .SD, .SDcols = c("forecaster_id",
                          "target_year",
                          "target_quarter",
                          "forecast_year",
                          "forecast_quarter",
                          "ens_fc",
                          "type_target"))



  #compute median forecasts
  spfdat_median <- alldata |>
    copy() |>
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

  #compute mean forecasts
  spfdat_mean <- alldata |>
    copy() |>
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


  #compute median quarterly forecasts
  spfdat_qu_median <- alldata |>
    copy() |>
    DT(type_target == "quarterly" & type_format == "POINT") |>
    DT(, fc_inst := forecast_year + 0.25*(forecast_quarter-1)) |>
    DT(, tg_inst := target_year + 0.25*(target_quarter-1)) |>
    DT(, horizon := tg_inst - fc_inst) |>
    DT(horizon == 0.5) |>
    DT(!is.na(prediction)) |>
    DT(, ens_fc := median(prediction), by = c("target_year",
                                              "target_quarter",
                                              "forecast_year",
                                              "forecast_quarter")) |>
    DT(, .SD, .SDcols = c("target_year",
                          "target_quarter",
                          "forecast_year",
                          "forecast_quarter",
                          "ens_fc")) |>
    unique()

  spfdat_qu_mean <- alldata |>
    copy() |>
    DT(type_target == "quarterly" & type_format == "POINT") |>
    DT(, fc_inst := forecast_year + 0.25*(forecast_quarter-1)) |>
    DT(, tg_inst := target_year + 0.25*(target_quarter-1)) |>
    DT(, horizon := tg_inst - fc_inst) |>
    DT(horizon == 0.5) |>
    DT(!is.na(prediction)) |>
    DT(, ens_fc := mean(prediction), by = c("target_year",
                                              "target_quarter",
                                              "forecast_year",
                                              "forecast_quarter")) |>
    DT(, .SD, .SDcols = c("target_year",
                          "target_quarter",
                          "forecast_year",
                          "forecast_quarter",
                          "ens_fc")) |>
    unique()


  data.table::fwrite(spfdat_individual_annual, here("data", "processed", "spf_individual_annual.csv"))
  data.table::fwrite(spfdat_individual_annualandquarterly, here("data", "processed", "spf_individual_annualandquarterly.csv"))
  data.table::fwrite(spfdat_median, here("data", "processed", "spf_median_forecast.csv"))
  data.table::fwrite(spfdat_mean, here("data", "processed", "spf_mean_forecast.csv"))
  data.table::fwrite(spfdat_qu_median, here("data", "processed", "spf_median_quarterly_forecast.csv"))
  data.table::fwrite(spfdat_qu_mean, here("data", "processed", "spf_mean_quarterly_forecast.csv"))
}

process_spf()


process_usspf <- function(){


  spfus <- readxl::read_xlsx(here("data", "raw", "Median_RGDP_Growth.xlsx")) |>
    setDT() |>
    setnames(c("YEAR", "QUARTER"), c("origin_year", "origin_quarter")) |>
    DT(, DRGDP6 := NULL) |>
    melt(id.vars = c("origin_year", "origin_quarter"), variable.name = "qu_ahead", value.name = "prediction") |>
    DT(, qu_ahead := as.numeric(substr(qu_ahead, 6,6)) - 2) |>
    DT(, target_year := origin_year + (origin_quarter + qu_ahead - 1) %/% 4) |>
    DT(, target_quarter := (origin_quarter + qu_ahead - 1) %% 4 + 1) |>
    DT(, .SD, .SDcols = c("origin_year", "origin_quarter", "target_year", "target_quarter", "prediction"))


  data.table::fwrite(spfus, here("data", "processed", "spf_us_median_forecast.csv"))
}

process_usspf()

process_ip <- function(){

  ipeu <- data.table::fread(here("data", "raw", "estat_ei_is_m_vtg.tsv.gz"),
                            sep = "\t") ##|>

  #for now, run everything on latest vintage (because coverage is non-perfect for any given base year)
  ipeu <- ipeu |>
    DT(grepl("*,EU$", get(names(ipeu)[1])))  |>
    DT(grepl("*,I", get(names(ipeu)[1]))) |>
    setnames("freq,revdate,s_adj,nace_r2,unit,geo\\TIME_PERIOD", "idmat") |>
    melt(id.vars = "idmat", variable.name = "target_period") |>
    DT(value != ":") |>
    DT(, c("freq","origin_date","seasonal_adj_type","nace_sector","unit","loc") := (tstrsplit(idmat, ","))) |>
    DT(, c("freq", "seasonal_adj_type", "nace_sector", "unit", "loc") :=
         lapply(.SD, as.factor),
       .SDcols = c("freq", "seasonal_adj_type", "nace_sector", "unit", "loc")) |>
    DT(, c("freq", "nace_sector") := NULL) |>
    DT(, idmat := NULL) |>
    DT(unit == "I21") |>
    DT(origin_date == "2025-08-14") |>
    DT(seasonal_adj_type == "SCA") |>
    DT(, .SD, .SDcols = c("target_period", "value")) |>
    DT(, target_year := as.numeric(sub("-.*", "", target_period)))|>
    DT(, target_month := as.numeric(sub(".*-", "", target_period))) |>
    DT(, target_period := NULL) |>
    DT(, target_quarter := ceiling(target_month/3)) |>
    DT(, value := as.numeric(value)) |>
    DT(, ip := mean(value), by = c("target_year", "target_quarter")) |>
    DT(, .SD, .SDcols = c("target_year", "target_quarter", "ip")) |>
    unique() |>
    DT(, growth_rate := (ip - shift(ip)) / shift(ip) * 100) |>
    DT(, ip := NULL) |>
    setnames("growth_rate", "ip")


  data.table::fwrite(ipeu, here("data", "processed", "ip_consolidated.csv"))
}

process_ip()

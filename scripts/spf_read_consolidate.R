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

  spf_curr <- data.table::fread(here("data", "spf_raw_data", paste0(origin_name, ".csv")),
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
alldata <- rbindlist(alldata)

data.table::fwrite(alldata, here("data", "spf_consolidated.csv"))

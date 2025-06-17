library(data.table)
library(here)

DT <- `[`

#GOAL OF THIS SCRIPT
#first check if data are the same for the period that data overlaps

pre14dat <- data.table::fread(here("data", "revdatpre14.csv")) |>
  DT(, ind := "pre")|>
  DT(, origin_day := NA) #for rbinding, since we don't have info on day for this data
post14dat <- data.table::fread(here("data", "revdatpost14.csv"))|>
  DT(, ind := "post")


#data to count number of updates per month
fulldatcounter <- rbind(pre14dat,
                        post14dat) |>
  DT(origin_year == 2014 & origin_month %in% c(10,11,12)) |>
  DT(, .SD, .SDcols = c("origin_year", "origin_month", "ind", "origin_day")) |>
  unique() |>
  DT(, counter := .N, by = c("origin_year", "origin_month", "ind"))


#check if there is ever more than one revision per month (this would only be the
#case for data in the post14 period, which (at least in later times) sometimes
#has more than one update per month)
any(fulldatcounter$counter > 1)
#-> no, there isn't. Can delete day column and convert to wide format


fulldat <- rbind(pre14dat,
                 post14dat) |>
  DT(origin_year == 2014 & origin_month %in% c(10,11,12)) |>
  DT(, origin_day := NULL) |>
  dcast(target_quarter + target_year + origin_year + origin_month ~ ind, value.var = "rgdp") |>
  DT(, diff := (post-pre)/post)
#-> there are differences! Calculate growth rates instead

fulldat <- rbind(pre14dat,
                 post14dat) |>
  DT(origin_year == 2014 & origin_month %in% c(10,11,12)) |>
  DT(, origin_day := NULL) |>
  setorder(ind, origin_year, origin_month, target_year, target_quarter) |>
  DT(, rgdp_growth := (rgdp / shift(rgdp) - 1) * 100,
     by = .(origin_year, origin_month, ind)) |>
  dcast(target_quarter + target_year + origin_year + origin_month ~ ind, value.var = "rgdp_growth") |>
  DT(, diff := (post - pre)/pre)

#plot
hist(fulldat$diff)

#classify a percentage difference of > 10 percent as "large" (somewhat arbitrary,
#but just for illustration purposes)
nrow(fulldat |> DT(diff > 0.1)) / nrow(fulldat)
#-> in 15% of instances, we have a "large" discrepancy between the vintage values
#from the pre14 data, vs. the post14 data.

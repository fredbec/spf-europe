library(here)
library(data.table)

#chain operator for data.table
DT <- `[`

#first and last release available
spfus <- readxl::read_xlsx(here("data", "raw", "Median_RGDP_Growth.xlsx")) |>
  setDT() |>
  setnames(c("YEAR", "QUARTER"), c("origin_year", "origin_quarter")) |>
  DT(, DRGDP6 := NULL) |>
  melt(id.vars = c("origin_year", "origin_quarter"), variable.name = "qu_ahead", value.name = "prediction") |>
  DT(, qu_ahead := as.numeric(substr(qu_ahead, 6,6)) - 2) |>
  DT(, target_year := floor(origin_year + (origin_quarter - 1)*0.25 + qu_ahead*0.25)) |>
  DT(, target_quarter := (origin_quarter + qu_ahead)%%4) |>
  DT(, target_quarter := ifelse(target_quarter == 0, 4, target_quarter)) |>
  DT(, .SD, .SDcols = c("origin_year", "origin_quarter", "target_year", "target_quarter", "prediction"))


data.table::fwrite(spfus, here("data", "spf_us_consolidated.csv"))

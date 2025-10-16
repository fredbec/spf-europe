library(here)
library(data.table)

#chain operator for data.table
DT <- `[`


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


data.table::fwrite(ipeu, here("data", "ip_consolidated.csv"))

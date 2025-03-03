library(here)
library(data.table)

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
source(here("scripts", "rtd_varnames.R"))
varnames <- fread(here("data", "rtd_varnames.csv"))

#make
reslist <- vector(mode = "list", length = length(start_yr:end_yr)*12)
j <- 0
for(yrnum in start_yr:end_yr){

  for(mnum in 1:12){

    c_varname <- varnames[year == yrnum & month == mnum]$varname

    mnum <- ifelse(mnum < 10, paste0("0",mnum), paste0(mnum))


    j <- j+1

    origin_name <- paste0(yrnum, mnum)

    reslist[[j]] <- read.csv(here("data", "rtd_quarterly", paste0("quarterly_", origin_name, ".csv")),
                         skip = 1,
                         header = TRUE) |>
      as.data.table() |>
      DT(, c("Name.", c_varname), with = FALSE) |>
      DT(3:.N,) |>
      setnames("Name.", "target") |>
      setnames(c_varname, "rgdp") |>
      DT(, target_quarter := substr(target, 1,2)) |>
      DT(, target_year := substr(target, 4, 8)) |>
      DT(, target := NULL) |>
      DT(, origin_year := yrnum) |>
      DT(, origin_month := as.numeric(mnum)) |>
      DT(!grepl("NA*", rgdp))

  }

}

rtd_dat <- rbindlist(reslist)

data.table::fwrite(rtd_dat, here("data", "rtd_consolidated.csv"))



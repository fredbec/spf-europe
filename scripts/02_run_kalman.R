library(here)
library(data.table)
DT <- `[`

source(here("R", "kalman_filter_runner.R"))
source(here("R", "config.R"))
source(here("R", "io.R"))
source(here("R", "kalman_filter_core", "kalman_filter.R"))
source(here("R", "kalman_filter_core", "kalman_filter_fe_fh.R"))


for(spec_id in c("consensus_median", "consensus_mean", "individual",
                 "inc_fixedhorizon_median", "inc_fixedhorizon_mean", "inc_fixedhorizon_individual")){

  spec <- read_spec(spec_id)

  runs <- read.csv(
    here("output", "filter_spf", spec_id, "runs.csv"),
    stringsAsFactors = FALSE
  )


for (i in seq_len(nrow(runs))) {
  run <- as.list(runs[i, ])
  print(run)
  settings <- merge_reformat_settings(spec, run)

  message("Running ", spec_id, " / ", run$run_id, " / ", run$name)

  results <- run_filter_from_settings(settings)
  write_outputs(results, settings)
}
}

library(here)
library(data.table)
DT <- `[`

source(here("scripts", "filter_spf", "filter_spf_functions.R"))



for(spec_id in c("consensus_median", "consensus_mean", "individual")){

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

library(here)
library(data.table)
source(here("R", "weighted_approach.R"))
source(here("R", "config.R"))
source(here("R", "io.R"))

DT <- `[`

for(spec_id in c("consensus_median", "individual")){

  spec <- read_spec(spec_id, loc = "benchmarks/optimal_weights")

  runs <- read.csv(
    here("output", "benchmarks", "optimal_weights", spec_id, "runs.csv"),
    stringsAsFactors = FALSE
  )


  for (i in seq_len(nrow(runs))) {
    run <- as.list(runs[i, ])
    print(run)
    settings <- merge_reformat_settings(spec, run)

    message("Running ", spec_id, " / ", run$run_id, " / ", run$name)

    results <- run_fixed_hor_forecasts_from_settings(settings)
    write_outputs(results, settings, base_path = "output/benchmarks/optimal_weights")
  }
}

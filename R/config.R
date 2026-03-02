read_spec <- function(spec_id) {
  path <- here("output", "filter_spf", spec_id, "specs.csv")
  stopifnot(file.exists(path))
  spec <- read.csv(path, stringsAsFactors = FALSE)
  as.list(setNames(spec$value, spec$key))
}

read_run <- function(spec_id, run_id) {
  path <- here("output", "filter_spf", spec_id, "runs.csv")
  runs <- read.csv(path, stringsAsFactors = FALSE)
  run <- runs[runs$run_id == run_id, ]
  stopifnot(nrow(run) == 1)
  as.list(run)
}

merge_reformat_settings <- function(spec, run) {
  c(spec, run) |>
    lapply(na_to_null)

}

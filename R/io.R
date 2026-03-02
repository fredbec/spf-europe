write_outputs <- function(results, settings, base_path = "output/filter_spf") {

  #  Build directory path
  spec_dir <- here("output", "filter_spf", settings$spec_id)

  # Define base filename prefix
  prefix <- sprintf("%03d", settings$run_id)
  #name <- paste0(settings$name)

  # Write filtered results
  if (!is.null(results$filter_output)) {
    filtered_file <- here(spec_dir, paste0(prefix, "_run", ".csv"))
    data.table::fwrite(results$filter_output, filtered_file)
  }

  # Write estimated parameters / metadata
  if (!is.null(results$metadata)) {
    meta_file <- here(spec_dir, paste0(prefix,"_metadata.csv"))
    data.table::fwrite(results$metadata, meta_file)


  }

  # Optional message
  message("Outputs saved for ", settings$spec_id, " / ", settings$run_id)
}

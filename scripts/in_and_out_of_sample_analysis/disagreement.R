#' Constructing disagreement of filtered SPF
#'
#' Computes disagreement in the cross-section of SPF GDP forecasts across
#' horizons (h = 0,...,4).
#'
#' @param spf_data Data frame with SPF forecasts and realized GDP growth (from `data_function_spf.R`).
#' @param EvalPeriod Numeric 2×1 matrix giving start and end years for evaluation (default: 2002–2019).
#'
#' @return Interquartile range and summary statistics.
#' @export
SPF_IQR <- function(spf_panel, EvalPeriod = cbind(2002, 2019)) {

  # Construct interquartile range
  disagreement <- spf_panel %>%
    group_by(ref_period) %>%
    summarise(
      target_year = first(target_year),
      IQR_h0 = IQR(spf_h0, na.rm = TRUE),
      IQR_h1 = IQR(spf_h1, na.rm = TRUE),
      IQR_h2 = IQR(spf_h2, na.rm = TRUE),
      IQR_h3 = IQR(spf_h3, na.rm = TRUE),
      IQR_h4 = IQR(spf_h4, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(ref_period) %>%
    mutate(
      target_year = target_year,
      IQR_h1 = dplyr::lead(IQR_h1, 1),
      IQR_h2 = dplyr::lead(IQR_h2, 2),
      IQR_h3 = dplyr::lead(IQR_h3, 3),
      IQR_h4 = dplyr::lead(IQR_h4, 4)
    ) %>%
    filter(!is.na(IQR_h0))

  # Summary statistics
  disagreement <- disagreement %>%
    filter(target_year >= EvalPeriod[1],
           target_year <= EvalPeriod[2]) %>%
    select(-target_year)


  disagreement_summary <- sapply(
    disagreement %>%
      select(starts_with("IQR")),
    function(x) {
      c(
        N    = sum(!is.na(x)),
        mean = mean(x, na.rm = TRUE),
        sd   = sd(x, na.rm = TRUE),
        min  = min(x, na.rm = TRUE),
        max  = max(x, na.rm = TRUE)
      )
    }
  )

  Output <- list(disagreement = disagreement, summary_stats = disagreement_summary)

  return(Output)

}


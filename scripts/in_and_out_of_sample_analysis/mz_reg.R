library(sandwich)   # For Newey-West standard errors
library(lmtest)     # For coeftest()
library(car)        # For testing linear hypothesis


#' Mincer-Zarnowitz Regression for SPF Forecasts
#'
#' Runs Mincer-Zarnowitz regressions of realized GDP growth on SPF forecasts
#' across horizons h = 0,...,4, computes HAC (Newey–West) standard errors,
#' and returns coefficients, R-squared, and Wald test p-values.
#'
#' @param SPF_cons Data frame with SPF forecasts (`spf_h0`–`spf_h4`) and realized GDP growth (`gdp_growth`).
#' @param EvalPeriod Numeric vector of length 2 giving start and end years for evaluation (default: c(2002, 2019)).
#' @param digits Number of digits to round coefficients, SEs, and statistics (default: 3).
#'
#' @return Data frame (or tibble) with alpha, beta, HAC standard errors, R-squared, Wald p-values, and numeric horizon as row names.
#' @export
MZReg = function(SPF_cons, EvalPeriod = cbind(2002, 2019), digits = 3) {

  SPF_cons <- SPF_cons %>%
    filter(target_year >= EvalPeriod[1],
           target_year <= EvalPeriod[2])

  # Horizons
  horizons <- 0:4

  # Run Mincer-Zarnowitz regressions
  mz_results <- lapply(horizons, function(h) {
    # Extract the forecast column for this horizon
    f <- SPF_cons[[paste0("spf_h", h)]]

    # Only keep rows with non-NA forecast
    y <- SPF_cons$gdp_growth[!is.na(f)]
    f <- f[!is.na(f)]

    # lm with a clean variable name
    lm(y ~ f)
  })


  # Wald test and table
  fmt <- function(coef, se) {
    paste0("$\\underset{(", se, ")}{", coef, "}$")
  }

  mz_table <- data.frame(
    horizon = horizons,
    alpha = mapply(fmt,
                   sapply(mz_results, function(m) round(coef(m)[1], digits)),
                   sapply(mz_results, function(m) round(sqrt(diag(NeweyWest(m)))[1], digits))),
    beta = mapply(fmt,
                  sapply(mz_results, function(m) round(coef(m)[2], digits)),
                  sapply(mz_results, function(m) round(sqrt(diag(NeweyWest(m)))[2], digits))),
    r2 = sapply(mz_results, function(m) round(summary(m)$r.squared, digits)),
    wald_p = sapply(mz_results, function(m) round(
      linearHypothesis(
        m,
        c("(Intercept)=0", "f=1"),
        vcov = NeweyWest(m)
      )$`Pr(>F)`[2], digits))
  )

  # Set horizon as row names
  row.names(mz_table) <- mz_table$horizon
  mz_table$horizon <- NULL

  return(mz_table)

}

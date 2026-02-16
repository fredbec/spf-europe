library(sandwich)   # For Newey-West standard errors
library(lmtest)     # For coeftest()
library(car)        # For testing linear hypothesis


#' Mincer-Zarnowitz Regression for SPF Consensus Forecasts
#'
#' Runs Mincer-Zarnowitz regressions of realized GDP growth on SPF forecasts
#' across horizons h = 0,...,4, computes HAC (Newey–West) standard errors,
#' and returns coefficients, R-squared, and Wald test p-values.
#'
#' @param SPF_cons Data frame with SPF forecasts (`spf_h0`–`spf_h4`) and realized GDP growth (`gdp_growth`).
#' @param EvalPeriod Numeric vector of length 2 giving start and end years for evaluation (default: c(2002, 2019)).
#' @param digits Number of digits to round coefficients, SEs, and statistics (default: 3).
#' @param DropPeriod Optional matrix of periods (start–end years) to exclude from evaluation.
#'
#' @return Data frame (or tibble) with alpha, beta, HAC standard errors, R-squared, Wald p-values, and numeric horizon as row names.
#' @export
MZReg = function(SPF_cons, EvalPeriod = cbind(2002, 2019), digits = 3, DropPeriod = NA) {

  SPF_cons <- SPF_cons %>%
    filter(target_year >= EvalPeriod[1],
           target_year <= EvalPeriod[2])

  # Drop periods if specified
  if (any(!is.na(DropPeriod))) {
    for (i in 1:dim(DropPeriod)[1]) {
      SPF_cons <- SPF_cons %>%
        filter(!(target_year %in% c(DropPeriod[i,1]:DropPeriod[i,2]) ))
    }
  }

  # Horizons
  horizons <- 0:4

  # Lag-length for HAC standrd errors
  lagNW <- max(4, as.integer( dim(SPF_cons)[1]^0.25 ) )

  # Run Mincer-Zarnowitz regressions
  mz_results <- lapply(horizons, function(h) {
    # Extract the forecast column for this horizon
    f <- SPF_cons[[paste0("spf_h", h)]]

    # Only keep rows with non-NA forecast
    y <- SPF_cons$gdp_growth[!is.na(f)]
    f <- f[!is.na(f)]

    # OLS regression
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
                   sapply(mz_results, function(m)
                     round(sqrt(diag(NeweyWest(m, lag = lagNW, prewhite = FALSE)))[1], digits))
    ),

    beta = mapply(fmt,
                  sapply(mz_results, function(m) round(coef(m)[2], digits)),
                  sapply(mz_results, function(m)
                    round(sqrt(diag(NeweyWest(m, lag = lagNW, prewhite = FALSE)))[2], digits))
    ),

    r2 = sapply(mz_results, function(m)
      round(summary(m)$r.squared, digits)),

    wald_p = sapply(mz_results, function(m)
      round(
        linearHypothesis(
          m,
          c("(Intercept)=0", "f=1"),
          vcov = NeweyWest(m, lag = lagNW, prewhite = FALSE)
        )$`Pr(>F)`[2],
        digits
      )),

    N = sapply(mz_results, function(m) nobs(m))
  )


  # Set horizon as row names
  row.names(mz_table) <- mz_table$horizon
  mz_table$horizon <- NULL

  return(mz_table)

}




#' Mincer-Zarnowitz Regression for individual SPF Forecasts
#'
#' Runs Mincer-Zarnowitz regressions of realized GDP growth on panel of SPF
#' forecasts across horizons h = 0,...,4, computes HAC (Newey–West) standard
#' errors, and returns coefficients, R-squared, and Wald test p-values.
#'
#' @param SPF_panel Data frame with SPF forecasts (`spf_h0`–`spf_h4`) and realized GDP growth (`gdp_growth`).
#' @param EvalPeriod Numeric vector of length 2 giving start and end years for evaluation (default: c(2002, 2019)).
#' @param digits Number of digits to round coefficients, SEs, and statistics (default: 3).
#' @param DropPeriod Optional matrix of periods (start–end years) to exclude from evaluation.
#'
#' @return Data frame (or tibble) with alpha, beta, HAC standard errors, R-squared, Wald p-values, and numeric horizon as row names.
#' @export
MZRegPanel = function(SPF_panel, EvalPeriod = cbind(2002, 2019), digits = 3, DropPeriod = NA) {

  SPF_panel <- SPF_panel %>%
    filter(target_year >= EvalPeriod[1],
           target_year <= EvalPeriod[2])

  # Drop periods if specified
  if (any(!is.na(DropPeriod))) {
    for (i in 1:nrow(DropPeriod)) {
      SPF_panel <- SPF_panel %>%
        filter(!(target_year %in% (DropPeriod[i,1]:DropPeriod[i,2])))
    }
  }

  # Horizons
  horizons <- 0:4

  # Run Mincer-Zarnowitz regressions
  mz_results <- lapply(horizons, function(h) {

    df <- SPF_panel %>%
      mutate(f = .data[[paste0("spf_h", h)]],
             y = gdp_growth) %>%
      filter(!is.na(f))

    model <- lm(y ~ f, data = df)

    # Cluster-robust covariance by forecaster
    vcov_cluster <- vcovCL(model, cluster = df$forecaster_id)

    list(model = model,
         vcov  = vcov_cluster,
         df    = df)
  })


  # Wald test and table
  fmt <- function(coef, se) {
    paste0("$\\underset{(", se, ")}{", coef, "}$")
  }

  mz_table <- data.frame(
    horizon = horizons,

    alpha = mapply(fmt,
                   sapply(mz_results, function(x) round(coef(x$model)[1], digits)),
                   sapply(mz_results, function(x)
                     round(sqrt(diag(x$vcov))[1], digits))
    ),

    beta = mapply(fmt,
                  sapply(mz_results, function(x) round(coef(x$model)[2], digits)),
                  sapply(mz_results, function(x)
                    round(sqrt(diag(x$vcov))[2], digits))
    ),

    r2 = sapply(mz_results, function(x)
      round(summary(x$model)$r.squared, digits)),

    N = sapply(mz_results, function(x) nobs(x$model)),

    wald_p = sapply(mz_results, function(x)
      formatC(
        linearHypothesis(
          x$model,
          c("(Intercept)=0", "f=1"),
          vcov = x$vcov
        )$`Pr(>F)`[2],
        digits = digits,
        format = "f"
      ))
  )


  return(mz_table)

}



library(sandwich)   # For Newey-West standard errors
library(lmtest)     # For coeftest()
library(car)        # For testing linear hypothesis


#' Forecast Errors on Revisions Regression for Consensus SPF Forecasts
#'
#' Runs regressions of consensus SPF forecast errors on forecast revisions across
#' horizons h = 0,...,3, computes HAC (Newey–West) standard errors,
#' and returns coefficients and R-squared statistics.
#'
#' @param SPF_cons Data frame with SPF forecasts (`spf_h0`–`spf_h4`) and realized GDP growth (`gdp_growth`).
#' @param EvalPeriod Numeric vector of length 2 giving start and end years for evaluation (default: c(2002, 2019)).
#' @param digits Number of digits to round coefficients, SEs, and statistics (default: 3).
#' @param DropPeriod Optional matrix of periods (start–end years) to exclude from evaluation.
#'
#' @return Data frame (or tibble) with alpha and beta coefficients (with HAC standard errors),
#' R-squared values, and numeric horizon as row names.
#' @export
ErrorsOnRevisionCons = function(SPF_cons, EvalPeriod = cbind(2002, 2019), digits = 3, DropPeriod = NA) {

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
  horizons <- 0:3

  # Run regressions
  ErrOnRev_results <- lapply(horizons, function(h) {

    ConsError    <- SPF_cons$gdp_growth - SPF_cons[[paste0("spf_h", h)]]
    ConsRevision <- SPF_cons[[paste0("spf_h", h)]] - SPF_cons[[paste0("spf_h", h + 1)]]

    df <- data.frame(e = ConsError, r = ConsRevision)
    df <- df[complete.cases(df), ]  # drop NA pairs

    lm(e ~ r, data = df)
  })


  # HAC lag rule
  T <- dim(SPF_cons)[1]
  lagNW <- max(4, as.integer(T^0.25))


  # Table
  fmt <- function(coef, se) {
    paste0("$\\underset{(", se, ")}{", coef, "}$")
  }

  ErrOnRev_table <- data.frame(
    horizon = horizons,

    alpha = mapply(fmt,
                   sapply(ErrOnRev_results, function(m) round(coef(m)[1], digits)),
                   sapply(ErrOnRev_results, function(m)
                     round(sqrt(diag(NeweyWest(m, lag = lagNW, prewhite = FALSE)))[1], digits))
    ),

    beta = mapply(fmt,
                  sapply(ErrOnRev_results, function(m) round(coef(m)[2], digits)),
                  sapply(ErrOnRev_results, function(m)
                    round(sqrt(diag(NeweyWest(m, lag = lagNW, prewhite = FALSE)))[2], digits))
    ),

    r2 = sapply(ErrOnRev_results, function(m)
      round(summary(m)$r.squared, digits)),

    N = sapply(ErrOnRev_results, function(m) nobs(m))
  )

  # Horizon as row names
  row.names(ErrOnRev_table) <- ErrOnRev_table$horizon
  ErrOnRev_table$horizon <- NULL

  return(ErrOnRev_table)

}




#' Forecast Revisions on Revisions Regression for SPF Forecasts
#'
#' Runs regressions of SPF forecast revisions on the lag of revisions across
#' horizons h = 0, 1, 2, computes HAC (Newey–West) standard errors,
#' and returns coefficients and R-squared statistics.
#'
#' @param SPF_cons Data frame with SPF forecasts (`spf_h0`–`spf_h4`) and realized GDP growth (`gdp_growth`).
#' @param EvalPeriod Numeric vector of length 2 giving start and end years for evaluation (default: c(2002, 2019)).
#' @param digits Number of digits to round coefficients, SEs, and statistics (default: 3).
#' @param DropPeriod Optional matrix of periods (start–end years) to exclude from evaluation.
#'
#' @return Data frame (or tibble) with alpha and beta coefficients (with HAC standard errors),
#' R-squared values, and numeric horizon as row names.
#' @export
RevisionsOnRevisionCons = function(SPF_cons, EvalPeriod = cbind(2002, 2019), digits = 3, DropPeriod = NA) {

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
  horizons <- 0:2

  # Run regressions
  RevOnRev_results <- lapply(horizons, function(h) {

    ConsRev    <- SPF_cons[[paste0("spf_h", h)]]     - SPF_cons[[paste0("spf_h", h + 1)]]
    ConsRevlag <- SPF_cons[[paste0("spf_h", h + 1)]] - SPF_cons[[paste0("spf_h", h + 2)]]

    df <- data.frame(r = ConsRev, rlag = ConsRevlag)
    df <- df[complete.cases(df), ]  # drop NA pairs

    lm(r ~ rlag, data = df)
  })


  # HAC lag rule
  T <- dim(SPF_cons)[1]
  lagNW <- max(4, as.integer(T^0.25))


  # Table
  fmt <- function(coef, se) {
    paste0("$\\underset{(", se, ")}{", coef, "}$")
  }

  RevOnRev_table <- data.frame(
    horizon = horizons,

    alpha = mapply(fmt,
                   sapply(RevOnRev_results, function(m) round(coef(m)[1], digits)),
                   sapply(RevOnRev_results, function(m)
                     round(sqrt(diag(NeweyWest(m, lag = lagNW, prewhite = FALSE)))[1], digits))
    ),

    beta = mapply(fmt,
                  sapply(RevOnRev_results, function(m) round(coef(m)[2], digits)),
                  sapply(RevOnRev_results, function(m)
                    round(sqrt(diag(NeweyWest(m, lag = lagNW, prewhite = FALSE)))[2], digits))
    ),

    r2 = sapply(RevOnRev_results, function(m)
      round(summary(m)$r.squared, digits)),

    N = sapply(RevOnRev_results, function(m) nobs(m))
  )

  # Horizon as row names
  row.names(RevOnRev_table) <- RevOnRev_table$horizon
  RevOnRev_table$horizon <- NULL

  return(RevOnRev_table)

}




#' Forecast Errors on Revisions Regression for Panel SPF Forecasts
#'
#' Runs regressions of panel SPF forecast errors on forecast revisions across
#' horizons h = 0,...,3, computes cluster-robust standard errors (by forecaster),
#' and returns alpha/beta coefficients, R², and sample size.
#'
#' @param SPF_panel Data frame with individual SPF forecasts (`spf_h0`–`spf_h4`),
#'        realized GDP growth (`gdp_growth`), and `forecaster_id`.
#' @param EvalPeriod Numeric vector of length 2 giving start and end years for evaluation (default: c(2002, 2019)).
#' @param digits Number of digits to round coefficients, SEs, and statistics (default: 3).
#' @param DropPeriod Optional matrix of periods (start–end years) to exclude from evaluation.
#' @param FixedEffects Indicator for estimating fixed-effects (default: FALSE).
#'
#' @return Data frame with alpha and beta coefficients (with cluster-robust SEs),
#'         R-squared, sample size, and numeric horizon as row names.
#' @export
ErrorsOnRevisionPanel <- function(SPF_panel, EvalPeriod = c(2002, 2019), digits = 3, DropPeriod = NA, FixedEffects = FALSE) {

  # Filter evaluation period
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
  horizons <- 0:3

  # Run panel regressions
  ErrOnRev_results <- lapply(horizons, function(h) {

    df <- SPF_panel %>%
      mutate(
        e = gdp_growth - .data[[paste0("spf_h", h)]],
        r = .data[[paste0("spf_h", h)]] - .data[[paste0("spf_h", h + 1)]]
      ) %>%
      filter(!is.na(e), !is.na(r), !is.na(forecaster_id))

    if (FixedEffects) {
      library(plm)        # For fixed-effects (this causes problems sometimes now)
      # Fixed-effects
      model <- plm(e ~ r,
                   data = df,
                   index = c("forecaster_id", "ref_period"),
                   model = "within")

      # Cluster-robust covariance by forecaster
      vcov_cluster <- vcovHC(model,
                             method = "arellano",
                             type = "HC1",
                             cluster = "group")

    } else {
      # OLS
      model <- lm(e ~ r, data = df)

      # Cluster-robust covariance by forecaster
      vcov_cluster <- vcovCL(model, cluster = df$forecaster_id)
    }

    list(model = model, vcov = vcov_cluster, df = df)
  })


  # Table
  fmt <- function(coef, se) {
    paste0("$\\underset{(", se, ")}{", coef, "}$")
  }

  if (FixedEffects) {
    ErrOnRev_table <- data.frame(
      horizon = horizons,

      beta = mapply(fmt,
                    sapply(ErrOnRev_results, function(x)
                      round(coef(x$model)["r"], digits)),
                    sapply(ErrOnRev_results, function(x)
                      round(sqrt(diag(x$vcov))["r"], digits))
      ),

      r2 = sapply(ErrOnRev_results, function(x)
        round(summary(x$model)$r.squared["rsq"], digits)),

      N = sapply(ErrOnRev_results, function(x)
        nobs(x$model))
    )

  } else {
    ErrOnRev_table <- data.frame(
      horizon = horizons,

      alpha = mapply(fmt,
                     sapply(ErrOnRev_results, function(x) round(coef(x$model)[1], digits)),
                     sapply(ErrOnRev_results, function(x) round(sqrt(diag(x$vcov))[1], digits))
      ),

      beta = mapply(fmt,
                    sapply(ErrOnRev_results, function(x) round(coef(x$model)[2], digits)),
                    sapply(ErrOnRev_results, function(x) round(sqrt(diag(x$vcov))[2], digits))
      ),

      r2 = sapply(ErrOnRev_results, function(x) round(summary(x$model)$r.squared, digits)),

      N = sapply(ErrOnRev_results, function(x) nobs(x$model))
    )
  }

  # Horizon as row names
  row.names(ErrOnRev_table) <- ErrOnRev_table$horizon
  ErrOnRev_table$horizon <- NULL

  return(ErrOnRev_table)
}




#' Forecast Revisions on Revisions Regression for Panel SPF Forecasts
#'
#' Runs regressions of panel SPF forecast revisions on the lag of revisions across
#' horizons h = 0, 1, 2, computes cluster-robust standard errors (by forecaster),
#' and returns alpha/beta coefficients, R², and sample size.
#'
#' @param SPF_panel Data frame with individual SPF forecasts (`spf_h0`–`spf_h4`),
#'        realized GDP growth (`gdp_growth`), and `forecaster_id`.
#' @param EvalPeriod Numeric vector of length 2 giving start and end years for evaluation (default: c(2002, 2019)).
#' @param digits Number of digits to round coefficients, SEs, and statistics (default: 3).
#' @param DropPeriod Optional matrix of periods (start–end years) to exclude from evaluation.
#'
#' @return Data frame with alpha and beta coefficients (with cluster-robust SEs),
#'         R-squared, sample size, and numeric horizon as row names.
#' @export
RevisionsOnRevisionPanel <- function(SPF_panel, EvalPeriod = c(2002, 2019), digits = 3, DropPeriod = NA) {

  # Filter evaluation period
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
  horizons <- 0:2

  # Run panel regressions
  RevOnRev_results <- lapply(horizons, function(h) {

    df <- SPF_panel %>%
      mutate(
        r = .data[[paste0("spf_h", h)]] - .data[[paste0("spf_h", h + 1)]],
        rlag = .data[[paste0("spf_h", h + 1)]] - .data[[paste0("spf_h", h + 2)]]
      ) %>%
      filter(!is.na(r), !is.na(rlag), !is.na(forecaster_id))

    model <- lm(r ~ rlag, data = df)

    # Cluster-robust covariance by forecaster
    vcov_cluster <- vcovCL(model, cluster = df$forecaster_id)

    list(model = model, vcov = vcov_cluster, df = df)
  })


  # Table
  fmt <- function(coef, se) {
    paste0("$\\underset{(", se, ")}{", coef, "}$")
  }

  RevOnRev_table <- data.frame(
    horizon = horizons,

    alpha = mapply(fmt,
                   sapply(RevOnRev_results, function(x) round(coef(x$model)[1], digits)),
                   sapply(RevOnRev_results, function(x) round(sqrt(diag(x$vcov))[1], digits))
    ),

    beta = mapply(fmt,
                  sapply(RevOnRev_results, function(x) round(coef(x$model)[2], digits)),
                  sapply(RevOnRev_results, function(x) round(sqrt(diag(x$vcov))[2], digits))
    ),

    r2 = sapply(RevOnRev_results, function(x) round(summary(x$model)$r.squared, digits)),

    N = sapply(RevOnRev_results, function(x) nobs(x$model))
  )

  # Horizon as row names
  row.names(RevOnRev_table) <- RevOnRev_table$horizon
  RevOnRev_table$horizon <- NULL

  return(RevOnRev_table)
}


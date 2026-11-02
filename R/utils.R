month_to_num <- function(x) {
  m <- match(x, month.abb)
  bad <- unique(x[is.na(m) & !is.na(x)])
  if (length(bad)) warning("Unmatched months: ", paste(bad, collapse = ", "), call. = FALSE)
  m
}


add_missing_cols <- function(dt, cols, fill = NA_real_) {
  for (col in setdiff(cols, names(dt))) set(dt, j = col, value = fill)
  dt
}

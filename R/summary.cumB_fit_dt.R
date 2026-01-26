
#' @export

summary.cumB_fit_dt <- function(object, summary_by = character(0), probs = c(0.025, 0.975), ...) {
  if (!inherits(object, "cumB_fit_dt")) stop("Not a cumB_fit_dt object.")
  if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table required.")

  P <- data.table::as.data.table(object$params)

  # counts before filtering
  total_curves <- nrow(P)
  conv_curves <- sum(P$converged %in% TRUE, na.rm = TRUE)

  # if no converged curves, return informative table
  if (conv_curves == 0) {
    return(data.table::data.table(
      n_curves_total = total_curves,
      n_curves_converged = 0L,
      note = "No converged curves. Check cumBfit_raw/cumBfit call and parameter extraction."
    ))
  }

  # keep converged only for stats
  P <- P[converged == TRUE]

  # grouping for summary
  if (length(summary_by) == 0) {
    P[, .grp__ := 1L]
    by_cols <- ".grp__"
  } else {
    miss <- setdiff(summary_by, names(P))
    if (length(miss)) stop("summary_by columns not found: ", paste(miss, collapse = ", "))
    by_cols <- summary_by
  }

  param_cols <- intersect(c("LowA","Steepness","TLinfl","BIOinfl","UpA"), names(P))

  stat_vec <- function(v) {
    v <- v[is.finite(v)]
    n <- length(v)
    if (n == 0) return(list(n = 0L, mean = NA_real_, sd = NA_real_, se = NA_real_, cv = NA_real_, lo = NA_real_, hi = NA_real_))
    m <- mean(v)
    s <- stats::sd(v)
    se <- s / sqrt(n)
    cv <- s / abs(m)
    qs <- stats::quantile(v, probs = probs, na.rm = TRUE, names = FALSE, type = 7)
    list(n = n, mean = m, sd = s, se = se, cv = cv, lo = qs[1], hi = qs[2])
  }

  out <- P[, {
    # compute stats for each parameter and stack long
    data.table::rbindlist(lapply(param_cols, function(pc) {
      r <- stat_vec(get(pc))
      data.table::data.table(
        param = pc,
        n_curves = data.table::uniqueN(curve_id),
        n = r$n,
        mean = r$mean,
        sd = r$sd,
        se = r$se,
        cv = r$cv,
        ci_lo = r$lo,
        ci_hi = r$hi
      )
    }))
  }, by = by_cols]

  # attach counts (useful)
  out[, `:=`(n_curves_total = total_curves, n_curves_converged = conv_curves)]

  if (".grp__" %in% names(out)) out[, .grp__ := NULL]
  out[]
}

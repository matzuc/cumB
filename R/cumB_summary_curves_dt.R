#' Summarise bootstrap cumB curves by group
#'
#' Compute summary statistics (mean, median, standard deviation, standard error
#' and confidence intervals) of cumulative biomass (cumB) curves across bootstrap
#' replicates, for each grouping factor and along a common x-axis.
#'
#' The function is designed to work on the output of
#' \code{\link{fit_cumB_curves_dt}}, and operates on fitted curves (or observed
#' curves) stored in \code{fit_obj$curves}. Since individual bootstrap curves may
#' be defined on different x-grids, curves are internally interpolated onto a
#' common grid before aggregation.
#'
#' @param fit_obj Object of class \code{"cumB_fit_dt"}, as returned by
#'   \code{\link{fit_cumB_curves_dt}}.
#' @param group_by Character vector of column names defining the grouping factors
#'   for which curves are summarised (e.g. \code{c("area", "year")}).
#'   The bootstrap column must NOT be included here.
#' @param boot_col Character; name of the bootstrap column
#'   (default \code{".boot"}).
#' @param type Character; which curves to summarise.
#'   Typically \code{"fit"} (default) for fitted curves or \code{"obs"} for
#'   observed points.
#' @param x_col Character; name of the x variable column (default \code{"x"}).
#' @param y_col Character; name of the y variable column to summarise.
#'   For fitted curves this is usually \code{"yhat"}.
#' @param grid_x Optional numeric vector defining a common x-grid.
#'   If \code{NULL}, a regular grid spanning the global x-range is generated.
#' @param npoints Integer; number of points used to generate the common x-grid
#'   when \code{grid_x = NULL} (default 200).
#' @param ci Numeric vector of length 2 defining lower and upper quantiles for
#'   confidence intervals (default \code{c(0.025, 0.975)}).
#' @param min_n Integer; minimum number of bootstrap curves required to compute
#'   summary statistics at a given x (default 3).
#'
#' @return A \code{data.table} of class \code{"cumB_summary_dt"} with one row per
#'   grouping factor and x value, containing:
#'   \itemize{
#'     \item \code{n_boot}: number of bootstrap curves contributing
#'     \item \code{mean}: mean cumB
#'     \item \code{median}: median cumB
#'     \item \code{sd}: standard deviation
#'     \item \code{se}: standard error
#'     \item \code{ci_low}: lower confidence interval
#'     \item \code{ci_up}: upper confidence interval
#'   }
#'
#' @details
#' Confidence intervals are computed using the percentile method on bootstrap
#' replicates. If fewer than \code{min_n} curves are available at a given x,
#' summary statistics are returned as \code{NA}.
#'
#' The function uses linear interpolation (\code{\link[stats]{approx}}) to align
#' individual curves onto a common x-grid.
#'
#' @seealso
#' \code{\link{fit_cumB_curves_dt}},
#' \code{\link{cumBfit}}
#'
#' @importFrom data.table as.data.table rbindlist
#' @importFrom data.table :=
#' @importFrom data.table .SD .SDcols
#' @importFrom stats approx quantile median sd
#'
#' @export
#'
#'
cumB_summary_curves_dt <- function(
    fit_obj,
    group_by,              # es. c("area","year")  (SENZA boot)
    boot_col = ".boot",    # nome colonna bootstrap
    type = "fit",          # "fit" o "obs"
    x_col = "x",
    y_col = "yhat",        # per type="fit" tipicamente "yhat"
    grid_x = NULL,         # se NULL: costruita automaticamente
    npoints = 200,         # usato solo se grid_x=NULL
    ci = c(0.025, 0.975),  # quantili per CI
    min_n = 3              # minimo numero curve per calcolare statistiche
) {
  stopifnot(is.list(fit_obj), !is.null(fit_obj$curves))
  if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table required.")
  DT <- data.table::as.data.table(fit_obj$curves)

  needed <- c(group_by, boot_col, x_col, y_col, "type")
  miss <- setdiff(needed, names(DT))
  if (length(miss)) stop("Missing columns in fit_obj$curves: ", paste(miss, collapse=", "))

  # filtra
  DT <- DT[type == type]
  DT <- DT[is.finite(get(x_col)) & is.finite(get(y_col))]

  if (nrow(DT) == 0) stop("No rows after filtering (check type/x_col/y_col).")

  # griglia comune
  if (is.null(grid_x)) {
    xmin <- DT[, min(get(x_col), na.rm = TRUE)]
    xmax <- DT[, max(get(x_col), na.rm = TRUE)]
    if (!is.finite(xmin) || !is.finite(xmax) || xmin >= xmax) stop("Invalid x range.")
    grid_x <- seq(xmin, xmax, length.out = npoints)
  } else {
    grid_x <- as.numeric(grid_x)
    grid_x <- grid_x[is.finite(grid_x)]
    grid_x <- sort(unique(grid_x))
    if (length(grid_x) < 2) stop("grid_x must have at least 2 finite values.")
  }

  # ID curva = gruppo + boot
  curve_cols <- c(group_by, boot_col)

  # interpola ogni curva su grid_x
  # NB: approx richiede x ordinato e senza NA
  interp <- DT[, {
    x0 <- get(x_col)
    y0 <- get(y_col)

    # ordina
    o <- order(x0)
    x0 <- x0[o]; y0 <- y0[o]

    # se x duplicati, li “collasso” con media su y
    if (anyDuplicated(x0)) {
      tmp <- data.table::data.table(x0 = x0, y0 = y0)[, .(y0 = mean(y0, na.rm=TRUE)), by = x0]
      x0 <- tmp$x0
      y0 <- tmp$y0
    }

    # se troppo pochi punti, restituisco NA
    if (length(x0) < 2 || length(unique(x0)) < 2) {
      list(x = grid_x, y = rep(NA_real_, length(grid_x)))
    } else {
      yy <- stats::approx(x0, y0, xout = grid_x, rule = 2)$y
      list(x = grid_x, y = as.numeric(yy))
    }
  }, by = curve_cols]

  # aggrega per gruppo base + x
  out <- interp[, {
    yv <- y
    yv <- yv[is.finite(yv)]
    n  <- length(yv)

    if (n < min_n) {
      list(
        n_boot = n,
        mean   = NA_real_,
        median = NA_real_,
        sd     = NA_real_,
        se     = NA_real_,
        ci_low = NA_real_,
        ci_up  = NA_real_
      )
    } else {
      sdv <- stats::sd(yv)
      sev <- sdv / sqrt(n)

      qs <- stats::quantile(yv, probs = ci, na.rm = TRUE, names = FALSE, type = 7)

      list(
        n_boot = n,
        mean   = mean(yv),
        median = stats::median(yv),
        sd     = sdv,
        se     = sev,
        ci_low = qs[1],
        ci_up  = qs[2]
      )
    }
  }, by = c(group_by, "x")]

  data.table::setorderv(out, c(group_by, "x"))

  class(out) <- c("cumB_summary_dt", class(out))
  attr(out, "group_by") <- group_by
  attr(out, "boot_col") <- boot_col
  attr(out, "type") <- type
  attr(out, "ci") <- ci
  attr(out, "grid_x") <- grid_x
  out
}

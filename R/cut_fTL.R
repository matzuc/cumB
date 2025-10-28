#' Cut a TL vector into fixed-width trophic-level classes (numeric midpoints)
#'
#' @description
#' Discretizes a numeric vector of trophic levels (TL) into fixed-width classes
#' and returns a **numeric midpoint** (`fTLr`) for each observation.
#' This is a simplified version of `make_fTL()` for standalone TL vectors.
#'
#' @details
#' - Intervals are **[a, b)** by default (`right = FALSE`).
#' - Labels are the **midpoints** of each TL bin.
#' - TL values are rounded to `digits` to avoid floating-point boundary errors.
#' - Values outside `[min_lim, max_lim]` return `NA`.
#' - Attributes `fTL_breaks`, `fTL_labels`, `fTL_step`, `fTL_right`,
#'   and `fTL_bounds` are attached to the numeric result.
#'
#' @param tl Numeric vector of trophic levels.
#' @param step Numeric (> 0). TL class width. Default: `0.1`.
#' @param min_lim,max_lim Numeric. Domain of TL (inclusive). Default: `1`, `5.4`.
#' @param right Logical. If `TRUE`, intervals are (a, b]; else [a, b). Default: `FALSE`.
#' @param digits Integer. Decimals to round before class assignment. Default: `6`.
#'
#' @return A numeric vector `fTLr` (midpoints), same length as `tl`, with `NA` for out-of-range
#' values. Attributes describing the discretization are attached.
#'
#' @examples
#' tl <- c(2.01, 3.47, 2.94, 5.40, 0.95)
#' f  <- cut_fTL_numeric(tl, step = 0.5, min_lim = 1, max_lim = 5.5)
#' f
#' attributes(f)$fTL_breaks
#'
#' @export
cut_fTL <- function(
    tl,
    step = 0.1, min_lim = 1, max_lim = 5.4,
    right = FALSE,
    digits = 6
) {
  if (!is.numeric(tl)) stop("`tl` must be numeric.")
  if (!is.numeric(step) || step <= 0) stop("`step` must be > 0.")
  if (max_lim <= min_lim) stop("`max_lim` must be > `min_lim`.")

  breaks <- seq(min_lim, max_lim, by = step)
  if (!isTRUE(all.equal(tail(breaks, 1), max_lim))) breaks <- c(breaks, max_lim)
  labels <- head(breaks, -1) + step/2

  tl_num <- round(tl, digits = digits)

  fTL <- cut(
    tl_num,
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE,
    right = right
  )

  # convert factor labels → numeric midpoints
  fTLr <- suppressWarnings(as.numeric(as.character(fTL)))

  # attach metadata
  attr(fTLr, "fTL_breaks") <- breaks
  attr(fTLr, "fTL_labels") <- labels
  attr(fTLr, "fTL_step")   <- step
  attr(fTLr, "fTL_right")  <- right
  attr(fTLr, "fTL_bounds") <- c(min = min_lim, max = max_lim)

  fTLr
}

#' Create trophic-level classes (fTL) and numeric midpoints (fTLr)
#'
#' @description
#' Discretizes a continuous trophic level (TL) column into fixed-width classes
#' (`fTL`) labeled by their midpoint, and adds a numeric midpoint version
#' (`fTLr`). Optionally, the function expands the dataset to include **all** TL
#' classes for each combination of specified grouping variables (useful for
#' plotting or cumulative indices), filling a measure column with a default value.
#'
#' @details
#' - By default, TL intervals are **[a, b)** (`right = FALSE`) to avoid ambiguity
#'   for upper bounds; the lowest bound is included (`include.lowest = TRUE`).
#' - Class labels are midpoints of the intervals: `breaks[i] + step/2`.
#' - TL values are rounded to `digits` decimals before cutting to mitigate
#'   floating-point boundary errors.
#' - If `expand = TRUE`, `tidyr::complete()` is applied over all combinations of
#'   `expand_by` × all TL classes; the column `fill_col` is filled with `fill_value`.
#' - The function stores attributes describing breaks and configuration.
#'
#' @param data A data.frame or tibble containing the continuous TL column.
#' @param TLcol Character string. Name of the column containing the continuous TL.
#' @param step Numeric (> 0). TL class width. Default: `0.1`.
#' @param min_lim,max_lim Numeric. TL domain limits. Default: `1`, `5.4`.
#' @param right Logical. If `TRUE`, intervals are (a, b], else [a, b). Default: `FALSE`.
#' @param digits Integer. Number of decimals to round TL before cutting. Default: `6`.
#' @param expand Logical. If `TRUE`, expand to all missing (group × fTL) combinations. Default: `FALSE`.
#' @param expand_by Character vector. Grouping columns used for expansion.
#'   Only used if `expand = TRUE`. Default: `c("period", "area")`.
#' @param fill_col Character. Name of the measure column to fill upon expansion.
#'   Must exist if `expand = TRUE`. Default: `"Biomass"`.
#' @param fill_value Value to insert in `fill_col` for missing combinations. Default: `0`.
#'
#' @return
#' The input dataframe with two additional columns:
#' \itemize{
#' \item `fTL`  (factor) – TL class as midpoint label;
#' \item `fTLr` (numeric) – numeric midpoint.
#' }
#'
#' If `expand = TRUE`, the dataset is completed over all TL classes within each
#' group defined by `expand_by`. The following attributes are also attached:
#' \itemize{
#' \item `fTL_breaks`  – vector of interval breaks;
#' \item `fTL_labels`  – midpoint labels;
#' \item `fTL_step`    – class width;
#' \item `fTL_right`   – interval side convention;
#' \item `fTL_bounds`  – c(min, max) TL limits.
#' }
#'
#' @note
#' TL values outside `[min_lim, max_lim]` result in `NA` class assignment. A
#' coverage check helper can be implemented separately if needed.
#'
#' @examples
#' df <- data.frame(
#'   period  = c("2019","2019","2020"),
#'   area    = c("A","B","A"),
#'   TL      = c(2.01, 3.47, 2.94),
#'   Biomass = c(10, 5, 8)
#' )
#'
#' # Basic class assignment
#' out <- make_fTL(df, TLcol = "TL", step = 0.5, min_lim = 1, max_lim = 5)
#' head(out)
#'
#' # Expansion to all TL classes within each period × area
#' out2 <- make_fTL(
#'   df, TLcol = "TL",
#'   step = 0.5, min_lim = 1, max_lim = 5,
#'   expand = TRUE, expand_by = c("period","area"),
#'   fill_col = "Biomass", fill_value = 0
#' )
#'
#' attr(out2, "fTL_breaks")
#' attr(out2, "fTL_labels")
#'
#' @seealso
#' Cumulative biomass or abundance functions that rely on `fTL`, such as `cumB_*`.
#'
#' @export
#' @importFrom tidyr complete nesting
#' @importFrom rlang syms
make_fTL <- function(
    data, TLcol,
    step = 0.1, min_lim = 1, max_lim = 5.4,
    right = FALSE,
    digits = 6,
    expand = FALSE,
    expand_by = c("period", "area"),
    fill_col = "Biomass",
    fill_value = 0
) {
  # --- checks ---
  if (!TLcol %in% names(data)) stop(sprintf("Column '%s' not found.", TLcol))
  if (!is.numeric(data[[TLcol]])) stop(sprintf("Column '%s' must be numeric.", TLcol))
  if (!is.numeric(step) || step <= 0) stop("`step` must be > 0.")
  if (!is.numeric(min_lim) || !is.numeric(max_lim) || max_lim <= min_lim) {
    stop("`min_lim` < `max_lim` and both numeric are required.")
  }

  # --- breaks & labels aligned to [min, max] ---
  breaks <- seq(min_lim, max_lim, by = step)
  if (!isTRUE(all.equal(tail(breaks, 1), max_lim))) breaks <- c(breaks, max_lim)
  labels <- head(breaks, -1) + step / 2

  TL_num <- round(data[[TLcol]], digits = digits)

  # --- cut to factor classes ---
  fTL <- cut(
    TL_num,
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE,
    right = right
  )

  fTLr <- suppressWarnings(as.numeric(as.character(fTL)))

  data$fTL  <- fTL
  data$fTLr <- fTLr

  # --- optional expansion ---
  if (isTRUE(expand)) {
    missing_cols <- setdiff(expand_by, names(data))
    if (length(missing_cols) > 0) {
      stop(paste("Missing columns for expand:", paste(missing_cols, collapse = ", ")))
    }
    if (!fill_col %in% names(data)) {
      stop(sprintf("Column '%s' not found for fill.", fill_col))
    }

    fTL_all <- factor(labels, levels = labels)

    data <- tidyr::complete(
      data,
      tidyr::nesting(!!!rlang::syms(expand_by)),
      fTL = fTL_all,
      fill = setNames(list(fill_value), fill_col)
    )

    data$fTLr <- suppressWarnings(as.numeric(as.character(data$fTL)))
  }

  # --- metadata ---
  attr(data, "fTL_breaks") <- breaks
  attr(data, "fTL_labels") <- labels
  attr(data, "fTL_step")   <- step
  attr(data, "fTL_right")  <- right
  attr(data, "fTL_bounds") <- c(min = min_lim, max = max_lim)

  data
}

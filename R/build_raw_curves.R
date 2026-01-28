#' Build raw binned and cumulative curves from TL and biomass
#'
#' @description
#' Constructs raw trophic-level cumulative curves by:
#' \enumerate{
#'   \item binning trophic level (TL) values into fixed-width classes
#'   (numeric midpoints, `fTLr`);
#'   \item summing biomass (or abundance) within each TL bin and within each
#'   ecological unit defined by `curve_by`;
#'   \item optionally aggregating replicated curves (e.g. bootstrap samples)
#'   defined by `group`;
#'   \item computing cumulative biomass (`cumB`) and standardized cumulative
#'   biomass (`cumBst`).
#' }
#'
#' The key design principle is the explicit separation between:
#' \itemize{
#'   \item \strong{curve identity} (`curve_by`): defines the ecological unit
#'   (e.g. area × year) for which a curve is constructed;
#'   \item \strong{replicates} (`group`): defines statistical replicates
#'   (e.g. bootstrap, permutation, simulation) that are \emph{not} part of the
#'   ecological aggregation and are combined only after curves are built.
#' }
#'
#' @param data A data.frame or tibble.
#' @param tl_col Character string. Name of the trophic level (TL) column (numeric).
#' @param biomass_col Character string. Name of the biomass or abundance column (numeric).
#' @param curve_by Character vector. Columns defining the ecological identity of
#'   each curve (e.g. `c("area","year")`). Use `character(0)` for a single global curve.
#' @param group Optional character string. Column identifying replicated curves
#'   (e.g. `.boot`). If provided, curves are first built for `curve_by` and then
#'   aggregated across `group`.
#' @param step Numeric (> 0). TL bin width. Default `0.1`.
#' @param min_lim,max_lim Numeric. Lower and upper TL limits.
#' @param right Logical. Passed to `cut()`. Default `FALSE` gives `[a,b)` intervals.
#' @param digits Integer. Number of decimals used to round TL before binning.
#' @param keep_empty_bins Logical. If `TRUE`, all TL bins are kept for each curve,
#'   filling missing bins with `B = 0`. Requires \pkg{tidyr}.
#' @param aggregate_fun Function. Aggregation function used to combine replicated
#'   curves across `group`. Default is `mean`.
#' @param make_gr Logical. If `TRUE`, creates a plotting id column `gr` by combining
#'   `curve_by` and (if present) `group`.
#'
#' @return
#' A tibble with:
#' \itemize{
#'   \item grouping columns (`curve_by` and optionally `group`);
#'   \item `fTLr`: numeric TL bin midpoints;
#'   \item `B`: binned biomass;
#'   \item `cumB`: cumulative biomass;
#'   \item `cumBst`: standardized cumulative biomass (0–1);
#'   \item `gr` (optional): curve identifier for plotting.
#' }
#'
#' Attributes describing the TL binning are attached to the output.
#'
#' @export
#'
build_raw_curves <- function(
    data,
    tl_col,
    biomass_col,
    curve_by = character(0),
    group = NULL,
    step = 0.1,
    min_lim = 1,
    max_lim = 5.4,
    right = FALSE,
    digits = 6,
    keep_empty_bins = FALSE,
    aggregate_fun = mean,
    make_gr = TRUE
) {
  stopifnot(is.data.frame(data))

  if (!tl_col %in% names(data)) stop("tl_col not found: ", tl_col)
  if (!biomass_col %in% names(data)) stop("biomass_col not found: ", biomass_col)
  if (!is.numeric(data[[tl_col]])) stop("TL column must be numeric.")
  if (!is.numeric(data[[biomass_col]])) stop("Biomass column must be numeric.")

  if (length(curve_by) > 0) {
    miss <- setdiff(curve_by, names(data))
    if (length(miss)) stop("curve_by columns not found: ", paste(miss, collapse = ", "))
  } else {
    data[[".curve__"]] <- 1L
    curve_by <- ".curve__"
    on.exit({ data[[".curve__"]] <- NULL }, add = TRUE)
  }

  if (!is.null(group) && !group %in% names(data)) {
    stop("group column not found: ", group)
  }

  # ---- TL binning ----
  breaks <- seq(min_lim, max_lim, by = step)
  if (!isTRUE(all.equal(tail(breaks, 1), max_lim)))
    breaks <- c(breaks, max_lim)

  labels <- head(breaks, -1) + step / 2

  TLr <- round(data[[tl_col]], digits = digits)
  fTL <- cut(TLr, breaks = breaks, labels = labels,
             include.lowest = TRUE, right = right)
  data[["fTLr"]] <- suppressWarnings(as.numeric(as.character(fTL)))

  # drop NA TL or biomass
  data <- data |>
    dplyr::filter(!is.na(.data[["fTLr"]]), !is.na(.data[[biomass_col]]))

  # ---- 1. build elemental curves (ecological level) ----
  elem <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(curve_by, "fTLr")))) |>
    dplyr::summarise(
      B = sum(.data[[biomass_col]], na.rm = TRUE),
      .groups = "drop"
    )

  # ---- optionally keep empty bins ----
  if (isTRUE(keep_empty_bins)) {
    if (!requireNamespace("tidyr", quietly = TRUE)) {
      stop("keep_empty_bins = TRUE requires tidyr.")
    }
    elem <- elem |>
      tidyr::complete(
        dplyr::across(dplyr::all_of(curve_by)),
        fTLr = labels,
        fill = list(B = 0)
      )
  }

  # ---- 2. aggregate replicated curves ----
  if (!is.null(group)) {
    elem <- elem |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(c(curve_by, group, "fTLr")))
      ) |>
      dplyr::summarise(
        B = aggregate_fun(B, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # ---- 3. cumulative curves ----
  grp_cum <- c(curve_by, group)
  grp_cum <- grp_cum[!is.na(grp_cum)]

  out <- elem |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_cum))) |>
    dplyr::arrange(fTLr, .by_group = TRUE) |>
    dplyr::mutate(
      cumB = cumsum(B),
      max_cumB = max(cumB, na.rm = TRUE),
      cumBst = dplyr::if_else(max_cumB > 0, cumB / max_cumB, 0 * cumB)
    ) |>
    dplyr::select(-max_cumB) |>
    dplyr::ungroup()

  # ---- plotting id ----
  if (isTRUE(make_gr)) {
    id_cols <- c(curve_by, group)
    out <- out |>
      dplyr::mutate(
        gr = as.character(interaction(dplyr::pick(dplyr::all_of(id_cols)),
                                      drop = TRUE, lex.order = TRUE))
      )
  }

  # ---- metadata ----
  attr(out, "fTL_breaks") <- breaks
  attr(out, "fTL_labels") <- labels
  attr(out, "fTL_step")   <- step
  attr(out, "fTL_bounds") <- c(min = min_lim, max = max_lim)

  out
}

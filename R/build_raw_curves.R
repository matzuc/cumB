#' Build raw binned and cumulative curves from TL and biomass
#'
#' @description
#' Creates raw curves by binning TL into fixed-width classes (numeric midpoints),
#' summing biomass within each TL bin and within each combination of grouping
#' factors, and then computing cumulative biomass (`cumB`) and standardized
#' cumulative biomass (`cumBst`).
#'
#' @param data A data.frame/tibble.
#' @param tl_col Name of the TL column (numeric).
#' @param biomass_col Name of the biomass/abundance column (numeric).
#' @param group_by Character vector of column names defining curve identity
#'   (e.g. c("area","year","period") or c("area","year",".boot")). Use
#'   character(0) for a single global curve.
#' @param step,min_lim,max_lim TL binning parameters.
#' @param right Logical passed to `cut()`. Default FALSE gives [a,b) intervals.
#' @param digits Integer; TL rounded before cutting to avoid boundary issues.
#' @param keep_empty_bins If TRUE, completes missing TL bins within each group,
#'   filling `B = 0`. Requires tidyr.
#' @param boot_col_name Optional column name for a curve id to help plotting.
#'   If provided, creates `gr` by pasting together `group_by` values.
#'
#' @return A tibble with grouping columns, `fTLr`, `B`, `cumB`, `cumBst`
#'   (and optionally `gr`).
#' @export
#'
build_raw_curves <- function(
  data,
  tl_col,
  biomass_col,
  group_by = character(0),
  step = 0.1,
  min_lim = 1,
  max_lim = 5.4,
  right = FALSE,
  digits = 6,
  keep_empty_bins = FALSE,
  make_gr = TRUE
) {
  stopifnot(is.data.frame(data))
  if (!tl_col %in% names(data)) stop("tl_col not found: ", tl_col)
  if (!biomass_col %in% names(data)) stop("biomass_col not found: ", biomass_col)
  if (!is.numeric(data[[tl_col]])) stop("TL column must be numeric.")
  if (!is.numeric(data[[biomass_col]])) stop("Biomass column must be numeric.")

  if (length(group_by) > 0) {
    missing_g <- setdiff(group_by, names(data))
    if (length(missing_g)) stop("group_by columns not found: ", paste(missing_g, collapse = ", "))
  } else {
    data[[".dummy_group__"]] <- 1L
    group_by <- ".dummy_group__"
    on.exit({ data[[".dummy_group__"]] <- NULL }, add = TRUE)
  }

  # ---- binning TL -> numeric midpoint fTLr ----
  breaks <- seq(min_lim, max_lim, by = step)
  if (!isTRUE(all.equal(tail(breaks, 1), max_lim))) breaks <- c(breaks, max_lim)
  labels <- head(breaks, -1) + step / 2

  TLr <- round(data[[tl_col]], digits = digits)
  fTL <- cut(TLr, breaks = breaks, labels = labels, include.lowest = TRUE, right = right)
  data[["fTLr"]] <- suppressWarnings(as.numeric(as.character(fTL)))

  # drop NA bins / NA biomass
  out <- data |>
    dplyr::filter(!is.na(.data[["fTLr"]]), !is.na(.data[[biomass_col]]))

  # ---- sum biomass per bin within groups ----
  out <- out |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_by, "fTLr")))) |>
    dplyr::summarise(B = sum(.data[[biomass_col]], na.rm = TRUE), .groups = "drop")

  # ---- optionally complete all bins per group ----
  if (isTRUE(keep_empty_bins)) {
    if (!requireNamespace("tidyr", quietly = TRUE)) {
      stop("keep_empty_bins=TRUE requires tidyr.")
    }
    all_bins <- labels
    out <- out |>
      tidyr::complete(
        dplyr::across(dplyr::all_of(group_by)),
        fTLr = all_bins,
        fill = list(B = 0)
      )
  }

  # ---- cumulative curves per group ----
  out <- out |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by))) |>
    dplyr::arrange(fTLr, .by_group = TRUE) |>
    dplyr::mutate(
      cumB = cumsum(B),
      max_cumB = max(cumB, na.rm = TRUE),
      cumBst = dplyr::if_else(max_cumB > 0, cumB / max_cumB, 0 * cumB)
    ) |>
    dplyr::select(-max_cumB) |>
    dplyr::ungroup()

  # ---- plotting id (no across() misuse) ----
  if (isTRUE(make_gr)) {
    out <- out |>
      dplyr::mutate(
        gr = as.character(interaction(dplyr::pick(dplyr::all_of(group_by)),
                                      drop = TRUE, lex.order = TRUE))
      )
  }

  attr(out, "fTL_breaks") <- breaks
  attr(out, "fTL_labels") <- labels
  attr(out, "fTL_step")   <- step
  attr(out, "fTL_right")  <- right
  attr(out, "fTL_bounds") <- c(min = min_lim, max = max_lim)

  out
}

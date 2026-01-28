#' Nonparametric bootstrap within ecological units (row- or cluster-level)
#'
#' @description
#' Generates nonparametric bootstrap samples by resampling observations
#' **within each ecological unit** defined by `curve_by`.
#'
#' If `id_unit` is provided, a **cluster bootstrap** is performed (e.g. stations);
#' otherwise a **row-level bootstrap** is used.
#'
#' Optionally, if `group` is provided (typically a bootstrap identifier such as
#' `.boot`), replicated datasets are aggregated across replicates using
#' `aggregate_fun`, while preserving the ecological structure defined by
#' `curve_by`.
#'
#' @param data A `data.frame` or tibble containing the observations.
#' @param curve_by Character vector defining the ecological units within which
#'   resampling occurs (e.g. `c("area","year")`). Use `character(0)` for a single
#'   global unit.
#' @param id_unit Optional character string identifying clusters
#'   (e.g. station, haul). If `NULL`, row-level bootstrap is used.
#' @param B Integer. Number of bootstrap replicates. Default `999`.
#' @param replace Logical. Resample with replacement. Default `TRUE`.
#' @param seed Optional integer seed for reproducibility.
#' @param boot_col_name Name of the bootstrap replicate column. Default `".boot"`.
#' @param group Optional character string identifying replicated datasets
#'   (typically equal to `boot_col_name`). If provided, bootstrap replicates
#'   are aggregated across this column.
#' @param aggregate_fun Function used to aggregate replicated values across
#'   `group` (default `mean`). Must accept `na.rm`.
#'
#' @return
#' A data.frame containing bootstrap samples. If `group` is provided, bootstrap
#' replicates are aggregated accordingly.
#'
#' Attributes describing the bootstrap design are attached to the output.
#'
#' @export
#'
boot_grouped_samples <- function(
    data,
    curve_by = character(0),
    id_unit = NULL,
    B = 999,
    replace = TRUE,
    seed = NULL,
    boot_col_name = ".boot",
    group = NULL,
    aggregate_fun = mean
) {

  # ---- basic checks ----
  if (!is.data.frame(data))
    stop("`data` must be a data.frame or tibble.")
  if (!is.null(seed))
    set.seed(as.integer(seed))
  if (!is.numeric(B) || B < 1)
    stop("`B` must be a positive integer.")
  if (!is.logical(replace) || length(replace) != 1L)
    stop("`replace` must be a single logical value.")

  if (!is.null(id_unit) && !id_unit %in% names(data))
    stop("`id_unit` not found in data: ", id_unit)

  # ---- curve_by handling ----
  if (length(curve_by) == 0) {
    data[[".curve__"]] <- 1L
    curve_by <- ".curve__"
    on.exit({ data[[".curve__"]] <- NULL }, add = TRUE)
  } else {
    miss <- setdiff(curve_by, names(data))
    if (length(miss))
      stop("curve_by columns not found: ", paste(miss, collapse = ", "))
  }

  # ---- split data by ecological units ----
  key <- if (length(curve_by) == 1L)
    data[[curve_by]]
  else
    interaction(data[curve_by], drop = TRUE, lex.order = TRUE)

  split_idx <- split(seq_len(nrow(data)), key, drop = TRUE)

  # ---- helpers ----
  boot_rows_one_unit <- function(idx) {
    n <- length(idx)
    out <- vector("list", B)
    for (b in seq_len(B)) {
      samp <- sample(idx, size = n, replace = replace)
      df <- data[samp, , drop = FALSE]
      df[[boot_col_name]] <- b
      out[[b]] <- df
    }
    out
  }

  boot_clusters_one_unit <- function(idx) {
    ids <- data[idx, id_unit, drop = TRUE]
    clu <- unique(ids)
    rows_by_cluster <- split(idx, ids, drop = TRUE)
    m <- length(clu)

    out <- vector("list", B)
    for (b in seq_len(B)) {
      samp_ids <- sample(clu, size = m, replace = replace)
      pieces <- lapply(
        samp_ids,
        function(cl) data[rows_by_cluster[[as.character(cl)]], , drop = FALSE]
      )
      df <- do.call(rbind, pieces)
      df[[boot_col_name]] <- b
      out[[b]] <- df
    }
    out
  }

  # ---- bootstrap per ecological unit ----
  res_lists <- lapply(split_idx, function(idx) {
    if (is.null(id_unit)) boot_rows_one_unit(idx)
    else boot_clusters_one_unit(idx)
  })

  # ---- bind per bootstrap replicate ----
  out_by_b <- vector("list", B)
  for (b in seq_len(B)) {
    out_by_b[[b]] <- do.call(rbind, lapply(res_lists, `[[`, b))
  }
  out <- do.call(rbind, out_by_b)

  # ---- optional aggregation across group ----
  if (!is.null(group)) {
    if (!group %in% names(out))
      stop("group column not found in output: ", group)

    num_cols <- names(out)[vapply(out, is.numeric, logical(1))]
    num_cols <- setdiff(num_cols, group)

    out <- out |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(setdiff(names(out), num_cols)))
      ) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(num_cols),
          ~ aggregate_fun(.x, na.rm = TRUE)
        ),
        .groups = "drop"
      )
  }

  # ---- metadata ----
  attr(out, "boot_B")        <- B
  attr(out, "boot_curve_by") <- curve_by
  attr(out, "boot_id_unit")  <- id_unit
  attr(out, "boot_seed")     <- seed

  out
}

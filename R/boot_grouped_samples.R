#' Nonparametric bootstrap within grouping combinations (row- or cluster-level)
#'
#' @description
#' Generates \eqn{B} nonparametric bootstrap samples by resampling **within each
#' combination** of grouping factors. If `id_unit` is provided, performs a
#' **cluster bootstrap** (resampling clusters with replacement and taking all rows
#' of the selected clusters); otherwise performs a **row-level bootstrap**
#' (resampling rows with replacement). The output stacks all replicates and adds
#' a `.boot` column (1..B).
#'
#' @param data A `data.frame` (or tibble) containing the observations.
#' @param group_by Character vector of column names defining the strata within
#' which resampling occurs (e.g., `c("area","year")`). Use `character(0)` for
#' unstratified bootstrap over the whole dataset.
#' @param id_unit Optional single character string naming a column that identifies
#' the **cluster** (e.g., station/transect/haul). If `NULL`, resampling is at the
#' **row** level; if provided, resampling is at the **cluster** level.
#' @param B Integer, number of bootstrap replicates. Default: `999`.
#' @param replace Logical, resample with replacement. Default: `TRUE` (bootstrap).
#' @param seed Optional integer to set the RNG seed for reproducibility. Default: `NULL`.
#' @param boot_col_name Name of the replicate id column in the output. Default: `".boot"`.
#'
#' @returns
#' A `data.frame` containing the concatenation of all bootstrap replicates with an
#' extra integer column `boot_col_name` in `1..B`. Grouping columns are preserved.
#' The function also attaches attributes:
#' - `boot_B` (number of replicates),
#' - `boot_group_by` (grouping columns),
#' - `boot_id_unit` (cluster id column or `NULL`),
#' - `boot_seed` (seed used or `NULL`).
#'
#' @details
#' - **Row bootstrap** (`id_unit = NULL`): within each group `g`, sample `n_g`
#'   row indices with replacement from the rows belonging to `g`.
#' - **Cluster bootstrap** (`id_unit` provided): within each group `g`, find the
#'   unique clusters; sample `m_g` cluster ids with replacement (where `m_g` is
#'   the original number of unique clusters in `g`), and bind all rows belonging
#'   to each sampled cluster (duplicating clusters as needed).
#' - If a group has fewer than 1 row (i.e., empty), an error is thrown.
#' - The **original group sizes** are preserved in expectation per replicate
#'   (row bootstrap: exact size; cluster bootstrap: exact number of clusters,
#'   but the number of rows depends on cluster sizes).
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   area = rep(LETTERS[1:2], each = 6),
#'   year = rep(rep(2019:2020, each = 3), 2),
#'   station = rep(paste0("S", 1:3), 4),
#'   value = rpois(12, lambda = 5)
#' )
#'
#' # Row-level bootstrap within area × year
#' boot_row <- boot_grouped_samples(df, group_by = c("area","year"), B = 10, seed = 42)
#' table(boot_row$.boot)
#'
#' # Cluster-level bootstrap (station) within area × year
#' boot_clu <- boot_grouped_samples(df, group_by = c("area","year"),
#'                                  id_unit = "station", B = 10, seed = 42)
#'
#' @export
boot_grouped_samples <- function(
    data,
    group_by = character(0),
    id_unit = NULL,
    B = 999,
    replace = TRUE,
    seed = NULL,
    boot_col_name = ".boot"
) {
  # --- basic checks ---
  if (!is.data.frame(data)) stop("`data` must be a data.frame or tibble.")
  if (!is.null(seed)) set.seed(as.integer(seed))
  if (!is.null(id_unit) && !is.character(id_unit)) stop("`id_unit` must be a single character string or NULL.")
  if (!is.null(id_unit) && length(id_unit) != 1L) stop("`id_unit` must be length-1 if provided.")
  if (!is.numeric(B) || B < 1) stop("`B` must be a positive integer.")
  B <- as.integer(B)
  if (!is.logical(replace) || length(replace) != 1L) stop("`replace` must be a single logical.")
  if (!is.character(boot_col_name) || length(boot_col_name) != 1L) stop("`boot_col_name` must be a single character string.")

  # grouping checks
  if (length(group_by) > 0L) {
    missing_g <- setdiff(group_by, names(data))
    if (length(missing_g)) stop(sprintf("Grouping columns not found: %s", paste(missing_g, collapse = ", ")))
  } else {
    # add a dummy grouping factor to simplify split
    data[[".dummy_group__"]] <- 1L
    group_by <- ".dummy_group__"
    on.exit({ data[[".dummy_group__"]] <- NULL }, add = TRUE)
  }

  # id_unit check
  if (!is.null(id_unit) && !id_unit %in% names(data)) {
    stop(sprintf("`id_unit` column '%s' not found in `data`.", id_unit))
  }

  # split data by groups (base R)
  key <- if (length(group_by) == 1L) data[[group_by]] else interaction(data[group_by], drop = TRUE, lex.order = TRUE)
  split_idx <- split(seq_len(nrow(data)), key, drop = TRUE)

  # helper: row bootstrap for one group's indices
  boot_rows_one_group <- function(idx, B, replace) {
    n <- length(idx)
    if (n < 1L) stop("Encountered an empty group; cannot bootstrap.")
    # matrix of sampled row indices (n x B)
    # For memory robustness, do one replicate at a time to keep it simple:
    out <- vector("list", B)
    for (b in seq_len(B)) {
      samp <- sample(idx, size = n, replace = replace)
      out[[b]] <- data[samp, , drop = FALSE]
      out[[b]][[boot_col_name]] <- b
    }
    out
  }

  # helper: cluster bootstrap for one group's indices
  boot_clusters_one_group <- function(idx, B, replace, id_col) {
    # unique clusters in this group
    ids <- data[idx, id_col, drop = TRUE]
    clu <- unique(ids)
    m <- length(clu)
    if (m < 1L) stop("Encountered a group with zero clusters; cannot bootstrap.")
    # map cluster -> row indices
    rows_by_cluster <- split(idx, ids, drop = TRUE)
    out <- vector("list", B)
    for (b in seq_len(B)) {
      # sample clusters with replacement; length = m (same as original number of clusters)
      samp_ids <- sample(clu, size = m, replace = replace)
      # bind rows of sampled clusters (duplicating as needed)
      pieces <- lapply(samp_ids, function(cl) data[rows_by_cluster[[as.character(cl)]], , drop = FALSE])
      grp <- do.call(rbind, pieces)
      grp[[boot_col_name]] <- b
      out[[b]] <- grp
    }
    out
  }

  # iterate groups, collect lists of B data.frames
  res_lists <- vector("list", length(split_idx))
  i <- 0L
  for (gname in names(split_idx)) {
    i <- i + 1L
    idx <- split_idx[[gname]]
    if (is.null(id_unit)) {
      res_lists[[i]] <- boot_rows_one_group(idx, B = B, replace = replace)
    } else {
      res_lists[[i]] <- boot_clusters_one_group(idx, B = B, replace = replace, id_col = id_unit)
    }
  }

  # now we need to merge across groups per replicate b
  # res_lists is a list (groups) of lists (B replicates)
  # build per-b replicate by rbind of each group's replicate b
  out_by_b <- vector("list", B)
  for (b in seq_len(B)) {
    parts <- lapply(res_lists, function(L) L[[b]])
    out_by_b[[b]] <- do.call(rbind, parts)
  }

  out <- do.call(rbind, out_by_b)

  # attach metadata
  attr(out, "boot_B")        <- B
  attr(out, "boot_group_by") <- group_by
  attr(out, "boot_id_unit")  <- id_unit
  attr(out, "boot_seed")     <- seed

  out
}

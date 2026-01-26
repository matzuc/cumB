#' Fit cumB curves (data.table)
#'
#' @param dt data.frame/data.table with curves
#' @param x_col name of x column (e.g. "fTLr")
#' @param y_col name of y column (e.g. "cumBst" or "cumB")
#' @param group_by character vector of columns defining one curve (e.g. c("area","year",".boot"))
#' @param standardize logical; TRUE = fit standardized curve (Upper fixed to 1), FALSE = free Upper
#' @param npoints number of points for the smooth fitted curve returned by cumBfit
#' @param TLrange numeric length-2; clamp prediction range (passed to cumBfit)
#' @param grid_x optional numeric vector; if NULL uses fit$curve$x for fitted curves
#' @param keep_obs logical; keep observed points in curves output
#' @param keep_fit logical; add fitted yhat in curves output
#'
#' @return object of class "cumB_fit_dt" with $curves and $params (both data.tables)
#' @importFrom data.table as.data.table rbindlist
#' @importFrom data.table :=
#' @importFrom data.table .SD .SDcols
#' @export
fit_cumB_curves_dt <- function(
    dt,
    x_col,
    y_col,
    group_by,
    standardize = TRUE,
    npoints = 1000,
    TLrange = c(1.6, 5),
    grid_x = NULL,
    keep_obs = TRUE,
    keep_fit = TRUE
) {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table required.")
  DT <- data.table::as.data.table(dt)

  miss <- setdiff(c(x_col, y_col, group_by), names(DT))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))

  DT[, x := get(x_col)]
  DT[, y := get(y_col)]
  DT <- DT[is.finite(x) & is.finite(y)]

  # scegli fct a seconda dello standardize
  drmfun_std  <- drc::baro5(fixed = c(NA, NA, NA, 1,  NA))  # Upper fisso=1
  drmfun_raw  <- drc::baro5(fixed = c(NA, NA, NA, NA, NA))  # Upper libero

  fit_one <- function(x, y) {
    fct <- if (isTRUE(standardize)) drmfun_std else drmfun_raw
    tryCatch(
      cumBfit(x = x, y = y, npoints = npoints, drmfun = fct, TLrange = TLrange),
      error = function(e) structure(list(error = conditionMessage(e)), class = "cumB_fit_error")
    )
  }

  params <- DT[, {
    x0 <- x
    y0 <- y

    ok <- length(x0) >= 4 && length(unique(x0)) >= 4 &&
      all(is.finite(x0)) && all(is.finite(y0))

    if (!ok) {
      list(
        n_pts = length(x0),
        converged = FALSE,
        LowA = NA_real_, Steepness = NA_real_, TLinfl = NA_real_, BIOinfl = NA_real_,
        UpA = NA_real_,
        pseudo_r2 = NA_real_, rmse = NA_real_,
        err = "too_few_points_or_nonfinite"
      )
    } else {
      fit <- fit_one(x0, y0)

      if (inherits(fit, "cumB_fit_error")) {
        list(
          n_pts = length(x0),
          converged = FALSE,
          LowA = NA_real_, Steepness = NA_real_, TLinfl = NA_real_, BIOinfl = NA_real_,
          UpA = NA_real_,
          pseudo_r2 = NA_real_, rmse = NA_real_,
          err = fit$error
        )
      } else {
        p <- fit$parameters
        LowA      <- as.numeric(p$LowA[1])
        Steepness <- as.numeric(p$Steepness[1])
        TLinfl    <- as.numeric(p$TLinfl[1])
        BIOinfl   <- as.numeric(p$BIOinfl[1])
        UpA       <- as.numeric(p$UpperA[1])

        gof <- fit$gof
        pseudo_r2 <- unname(gof["pseudo_r2"])
        rmse      <- unname(gof["rmse"])

        list(
          n_pts = length(x0),
          converged = is.finite(Steepness) && is.finite(TLinfl),
          LowA = LowA, Steepness = Steepness, TLinfl = TLinfl, BIOinfl = BIOinfl,
          UpA = UpA,
          pseudo_r2 = pseudo_r2, rmse = rmse,
          err = NA_character_
        )
      }
    }
  }, by = group_by]

  curves_list <- list()

  if (keep_obs) {
    obs <- DT[, c(group_by, "x", "y"), with = FALSE]
    obs[, type := "obs"]
    obs[, yhat := NA_real_]
    curves_list[["obs"]] <- obs
  }

  if (keep_fit) {
    fit_curves <- DT[, {
      x_obs <- x
      y_obs <- y
      fit <- fit_one(x_obs, y_obs)

      if (inherits(fit, "cumB_fit_error")) {
        xg <- if (is.null(grid_x)) sort(unique(x_obs)) else as.numeric(grid_x)
        list(x = xg, y = NA_real_, yhat = rep(NA_real_, length(xg)))
      } else {
        if (is.null(grid_x)) {
          list(x = fit$curve$x, y = NA_real_, yhat = as.numeric(fit$curve$y))
        } else {
          # se vuoi una griglia personalizzata, interpolo la curva
          xg <- as.numeric(grid_x)
          yhat <- approx(fit$curve$x, fit$curve$y, xout = xg, rule = 2)$y
          list(x = xg, y = NA_real_, yhat = as.numeric(yhat))
        }
      }
    }, by = group_by]

    fit_curves[, type := "fit"]
    curves_list[["fit"]] <- fit_curves
  }

  curves <- data.table::rbindlist(curves_list, use.names = TRUE, fill = TRUE)

  mk_id <- function(D) do.call(paste, c(D[, ..group_by], sep = "_"))
  curves[, curve_id := mk_id(curves)]
  params[, curve_id := mk_id(params)]

  out <- list(curves = curves, params = params)
  class(out) <- "cumB_fit_dt"
  attr(out, "group_by") <- group_by
  attr(out, "x_col") <- x_col
  attr(out, "y_col") <- y_col
  attr(out, "standardize") <- standardize
  attr(out, "npoints") <- npoints
  attr(out, "TLrange") <- TLrange
  out
}

#' Stima dei parametri Tweedie per specie
#'
#' Esegue, per ciascuna specie in un dataset "long",
#' la stima del parametro di potenza \eqn{p} via profiling
#' e poi fitta un GLM Tweedie (link log) per ottenere la dispersione \eqn{\phi}.
#'
#' @param data Data frame in formato lungo.
#' @param response Nome della colonna risposta (numeric), e.g. "biomassa".
#' @param species Nome della colonna specie, e.g. "specie".
#' @param profile_p_range Range per il profiling di \eqn{p} (default c(1.1, 1.9)).
#' @param min_pos Minimo numero di osservazioni positive richieste per stimare \eqn{p}
#'   con profiling; in caso contrario si usa un fallback (default 3).
#' @param fallback_p Valore di fallback di \eqn{p} se il profiling fallisce (default 1.5).
#' @param quiet Se TRUE, riduce il verboso delle funzioni di stima.
#'
#' @return Tibble con una riga per specie: \code{specie, n, n_pos, mean_y, p_hat, phi_hat, aic, method_p}.
#' @details
#' La procedura:
#' \enumerate{
#'   \item Per ogni specie, usa \code{tweedie::tweedie.profile} (intercetta) su \eqn{y>0}
#'         per stimare \eqn{p} nel range specificato.
#'   \item Fitta un GLM Tweedie con \code{statmod::tweedie(var.power = p_hat, link.power = 0)}
#'         su tutti i dati della specie (zeri inclusi) per stimare \eqn{\phi}.
#' }
#' In caso di fallimenti o dati insufficienti, \eqn{p} viene posto a \code{fallback_p}.
#'
#' @examples
#' \dontrun{
#' params <- fit_tweedie_by_species(df, response = "biomassa", species = "specie")
#' head(params)
#' }
#' @export
biomass_fit <- function(
    data,
    response = "B",
    species  = "SP",
    profile_p_range = c(1.1, 1.9),
    min_pos = 3,
    fallback_p = 1.5,
    quiet = TRUE
) {
  stopifnot(is.data.frame(data))
  if (!all(c(response, species) %in% names(data))) {
    stop("Le colonne '", response, "' e/o '", species, "' non esistono in 'data'.")
  }

  # lazy imports per package
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)
  requireNamespace("tweedie", quietly = TRUE)
  requireNamespace("statmod", quietly = TRUE)

  df <- data
  y  <- df[[response]]
  if (!is.numeric(y)) stop("La colonna '", response, "' deve essere numerica.")

  sp_levels <- unique(df[[species]])

  res_list <- purrr::map(sp_levels, function(spi) {
    sub <- df[df[[species]] == spi, , drop = FALSE]
    yi  <- sub[[response]]
    n   <- length(yi)
    n_pos <- sum(yi > 0, na.rm = TRUE)
    mean_y <- mean(yi, na.rm = TRUE)

    # ---- 1) Stima p via profiling su y>0 (se possibile)
    p_hat <- NA_real_; method_p <- NA_character_
    if (is.finite(mean_y) && n_pos >= min_pos && stats::var(yi[yi > 0]) > 0) {
      # usare solo positivi per il profiling (più stabile)
      df_pos <- sub[yi > 0, , drop = FALSE]
      y_pos  <- df_pos[[response]]

      # Profiling con intercetta sola: y ~ 1
      prof_fit <- try(
        tweedie::tweedie.profile(
          formula = stats::as.formula(paste(response, "~ 1")),
          data = df_pos,
          p.vec = seq(profile_p_range[1], profile_p_range[2], length.out = 15),
          do.plot = FALSE,
          method = "series"
        ),
        silent = quiet
      )

      if (!inherits(prof_fit, "try-error")) {
        p_hat <- prof_fit$p.max
        method_p <- "profile"
      }
    }

    if (!is.finite(p_hat)) {
      p_hat <- fallback_p
      method_p <- "fallback"
    }

    # Bound leggeri per sicurezza
    p_hat <- max(min(p_hat, 1.999), 1.001)

    # ---- 2) GLM Tweedie (link log) per stimare phi su tutti i dati (zeri inclusi)
    fam <- statmod::tweedie(var.power = p_hat, link.power = 0) # link log
    glm_fit <- try(stats::glm(
      formula = stats::as.formula(paste(response, "~ 1")),
      family  = fam,
      data    = sub
    ), silent = quiet)

    phi_hat <- NA_real_; aic <- NA_real_
    if (!inherits(glm_fit, "try-error")) {
      sm <- summary(glm_fit)
      # dispersione: per famiglie non gaussiane, summary$dispersion è la stima di phi
      phi_hat <- suppressWarnings(as.numeric(sm$dispersion))
      aic     <- stats::AIC(glm_fit)

      # sanity check
      if (!is.finite(phi_hat) || phi_hat <= 0) phi_hat <- NA_real_
    }

    tibble::tibble(
      !!species := spi,
      n = n,
      n_pos = n_pos,
      mean_y = mean_y,
      p_hat = p_hat,
      phi_hat = phi_hat,
      aic = aic,
      method_p = method_p
    )
  })

  dplyr::bind_rows(res_list)
}

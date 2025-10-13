#' Simula (o replica) biomasse da Tweedie in formato lungo
#'
#' Per ogni gruppo (\code{group_vars} + \code{species}):
#' - se \code{n >= n_min}, simula da Tweedie con \code{mu =} osservazione riga-per-riga,
#'   \code{phi = phi_hat} e \code{power = p_hat} della specie;
#' - se \code{n < n_min}, NON simula ma replica i valori osservati per tutte le \code{nsim} iter.
#'
#' @param data Data frame lungo con le osservazioni (deve contenere \code{response} e \code{species}).
#' @param params Data frame con almeno \code{species}, \code{p_hat}, \code{phi_hat}.
#' @param response Nome della colonna risposta (es. "biomassa" o "B").
#' @param species Nome della colonna specie (es. "species").
#' @param group_vars Variabili di raggruppamento (default: c("area","period","year")).
#' @param nsim Numero di repliche simulate (o replicate) per gruppo.
#' @param n_min Soglia minima di osservazioni per simulare (default 3).
#' @param mu_min Soglia minima per \code{mu} quando si simula (evita 0 esatto), default 1e-8.
#'
#' @return Tibble lungo con le colonne originali + \code{iter} e \code{sim_biomassa}.
#' @export
#' @import dplyr
#' @importFrom purrr map_dfr
#' @importFrom tweedie rtweedie
simulate_biomass_tweedie <- function(
    data,
    params,
    response   = "biomassa",
    species    = "species",
    group_vars = c(NA),
    nsim       = 100,
    n_min      = 3,
    mu_min     = 1e-8
) {
  stopifnot(is.data.frame(data), is.data.frame(params))
  if (!all(c(response, species) %in% names(data))) {
    stop("Mancano '", response, "' e/o '", species, "' in 'data'.")
  }
  if (!all(c(species, "p_hat", "phi_hat") %in% names(params))) {
    stop("Mancano 'species', 'p_hat' e/o 'phi_hat' in 'params'.")
  }
  split_vars <- c(group_vars, species)
  if (!all(split_vars %in% names(data))) {
    stop("Le variabili in 'group_vars' o 'species' non sono presenti in 'data'.")
  }

  df <- dplyr::left_join(
    data,
    params %>% dplyr::select(all_of(c(species, "p_hat", "phi_hat"))),
    by = species
  )

  sim_one_group <- function(gdf) {
    n_obs <- nrow(gdf)

    # caso n < n_min: replichiamo il campione com'è per nsim iter
    if (n_obs < n_min) {
      return(
        purrr::map_dfr(seq_len(nsim), function(it) {
          dplyr::mutate(gdf, iter = it, sim_biomassa = .data[[response]])
        })
      )
    }

    # parametri specie; se mancanti/invalidi, fallback: replica osservati
    p  <- unique(gdf$p_hat); ph <- unique(gdf$phi_hat)
    invalid_par <- length(p) != 1 || length(ph) != 1 ||
      !is.finite(p) || !is.finite(ph) || p <= 1 || p >= 2 || ph <= 0
    if (invalid_par) {
      return(
        purrr::map_dfr(seq_len(nsim), function(it) {
          dplyr::mutate(gdf, iter = it, sim_biomassa = .data[[response]])
        })
      )
    }

    # mu = osservazione per riga; clamp solo per la simulazione
    mu_vec <- as.numeric(gdf[[response]])
    mu_sim <- mu_vec
    mu_sim[!is.finite(mu_sim) | mu_sim < mu_min] <- mu_min

    purrr::map_dfr(seq_len(nsim), function(it) {
      ysim <- tweedie::rtweedie(n = n_obs, mu = mu_sim, phi = ph, power = p)
      dplyr::mutate(gdf, iter = it, sim_biomassa = ysim)
    })
  }

  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(split_vars))) %>%
    dplyr::group_modify(~ sim_one_group(.x)) %>%
    dplyr::ungroup()
}

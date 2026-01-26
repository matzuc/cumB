#' cumBfit
#'
#' Descrizione dettagliata della funzione.
#'
#' @param x Descrizione del parametro x.
#' @param y Descrizione del parametro y.
#' @param drmfun paramenters for the DRC curve fitting
#' @return Descrizione di ciò che viene restituito.
#' @export
#' @import drc


cumBfit <- function(x, y, npoints = 2000, drmfun =  baro5(fixed = c(NA, NA, NA, 1, NA)), TLrange = c(1.6, 5)){

  # fit the model
  r <- drm(y ~ x, fct = drmfun)

  # use the fitted curve to predict the expected values (these are used for plotting and estimating the parameters)


  minX <- min(min(x), TLrange[1])
  maxX <- min(max(x), TLrange[2])


  # xx are the TLs
  xx <- NA;
  xx <- seq(minX, maxX,length.out = npoints)
  pr <- NA; length(pr) <- npoints# vector for prediction
  pr <- predict(r, newdata = data.frame(x = xx))

  # parameters

  dpr<-diff(pr,1)/diff(xx)

  # TL @ inflection point
  TLinfl <- xx[which.max(dpr)-1]
  # maximum steepness
  Steepness <- max(dpr)
  # Biomass @ inflection
  BIOinfl<- predict(r, newdata=data.frame(x = TLinfl))
  # Lower asymptote
  LowA <- predict(r, newdata=data.frame(x=1))
  UpperA <- predict(r, newdata=data.frame(x=max(xx)))



  # metriche sui punti osservati
  yhat_obs <- as.numeric(stats::predict(r, newdata = data.frame(x = x)))
  resid    <- y - yhat_obs
  sse <- sum(resid^2, na.rm = TRUE)
  sst <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
  pseudo_r2 <- if (sst > 0) 1 - sse/sst else NA_real_
  rmse <- sqrt(mean(resid^2, na.rm = TRUE))




  # Crea una lista con tutti i risultati
  results <- list(
    curve = data.frame(x = xx, y = pr),
    parameters = data.frame(LowA = LowA, Steepness = Steepness, TLinfl = TLinfl, BIOinfl = BIOinfl, UpperA = UpperA),
    model = r,
    predictions = pr,
    gof = c(pseudo_r2 = pseudo_r2, rmse = rmse)
  )

  # Imposta la classe dell'oggetto risultante
  class(results) <- "cumB_curve"

  return(results)
}

# Method for printing the object
print.cumB_curve <- function(x, ...) {
  cat("My Curve Object\n")
  cat("Parameters:\n")
  print(x$parameters)
  # Puoi aggiungere ulteriori dettagli se lo desideri
}

# Method for the summary of the object
summary.cumB_curve <- function(object, ...) {
  # Ad esempio, qui potresti fornire statistiche descrittive o misure di bontà di adattamento
  cat("Summary of My Curve Object\n")
  # ... [il tuo codice per creare un riassunto] ...
}

# Metodo plot
plot.cumB_curve <- function(obj, ...) {

  plot(obj$curve$x, obj$curve$y, ...)
  # Puoi personalizzare ulteriormente il grafico se lo desideri
}


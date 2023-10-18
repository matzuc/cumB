#' cumBfit
#'
#' Descrizione dettagliata della funzione.
#'
#' @param x Descrizione del parametro x.
#' @param y Descrizione del parametro y.
#' @return Descrizione di ciò che viene restituito.
#' @export
#' @import drc

cumBfit <- function(x, y, npoints = 5000){

  # fit the model
  r <- drm(y ~ x, fct = baro5(fixed = c(NA, NA, NA, 1, NA)))

  # use the fitted curve to predict the expected values (these are used fot plotting and estimating the parameters)

  # xx are the TLs
  xx <- NA;
  xx <- seq(1.6, 5,length.out = npoints)
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




  # Crea una lista con tutti i risultati
  results <- list(
    curve = data.frame(x = xx, y = pr),
    parameters = data.frame(LowA = LowA, Steepness = Steepness, TLinfl = TLinfl, BIOinfl = BIOinfl),
    model = r,
    predictions = pr
  )

  # Imposta la classe dell'oggetto risultante
  class(results) <- "cumB_curve"

  return(results)
}

# Metodo print
print.cumB_curve <- function(x, ...) {
  cat("My Curve Object\n")
  cat("Parameters:\n")
  print(x$parameters)
  # Puoi aggiungere ulteriori dettagli se lo desideri
}

# Metodo summary
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


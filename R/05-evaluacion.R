# OPTIMIZACIÓN DE CONSTANTES Y VENTANAS

optimizar <- function(y, metodo, rejilla, s = NULL) {
  
  if (!is.numeric(y)) {
    stop("La serie debe ser numérica.")
  }
  
  if (!is.character(metodo)) {
    stop("El método debe ser un texto.")
  }
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  if (length(y) < 2) {
    stop("Se necesitan al menos 2 observaciones.")
  }
  
  
  # MEDIA MÓVIL
  
  if (metodo == "mm") {
    
    resultados <- data.frame(
      k = rejilla,
      MSE = NA_real_
    )
    
    for (i in seq_along(rejilla)) {
      
      k <- rejilla[i]
      
      if (k < length(y)) {
        
        modelo <- ajustar_mm(y, k)
        
        posiciones <- which(!is.na(modelo$yhat))
        errores <- y[posiciones] - modelo$yhat[posiciones]
        
        resultados$MSE[i] <- mean(errores^2)
      }
    }
  }
  
  
  # DOBLE MEDIA MÓVIL
  
  else if (metodo == "dmm") {
    
    resultados <- data.frame(
      k = rejilla,
      MSE = NA_real_
    )
    
    for (i in seq_along(rejilla)) {
      
      k <- rejilla[i]
      
      if (2 * k - 1 < length(y)) {
        
        modelo <- ajustar_dmm(y, k)
        
        posiciones <- which(!is.na(modelo$yhat))
        errores <- y[posiciones] - modelo$yhat[posiciones]
        
        resultados$MSE[i] <- mean(errores^2)
      }
    }
  }
  
  
  # SUAVIZAMIENTO EXPONENCIAL SIMPLE
  
  else if (metodo == "ses") {
    
    resultados <- data.frame(
      alpha = rejilla,
      MSE = NA_real_
    )
    
    for (i in seq_along(rejilla)) {
      
      alpha <- rejilla[i]
      
      modelo <- ajustar_ses(y, alpha)
      
      posiciones <- which(!is.na(modelo$yhat))
      errores <- y[posiciones] - modelo$yhat[posiciones]
      
      resultados$MSE[i] <- mean(errores^2)
    }
  }
  
  
  # HOLT LINEAL
  
  else if (metodo == "holt") {
    
    resultados <- expand.grid(
      alpha = rejilla,
      beta = rejilla
    )
    
    resultados$MSE <- NA_real_
    
    for (i in 1:nrow(resultados)) {
      
      modelo <- ajustar_holt(
        y,
        resultados$alpha[i],
        resultados$beta[i]
      )
      
      posiciones <- which(!is.na(modelo$yhat))
      errores <- y[posiciones] - modelo$yhat[posiciones]
      
      resultados$MSE[i] <- mean(errores^2)
    }
  }
  
  
  # HOLT-WINTERS ADITIVO
  
  else if (metodo == "hw") {
    
    if (is.null(s)) {
      stop("Se debe indicar s para Holt-Winters.")
    }
    
    resultados <- expand.grid(
      alpha = rejilla,
      beta = rejilla,
      gamma = rejilla
    )
    
    resultados$MSE <- NA_real_
    
    for (i in 1:nrow(resultados)) {
      
      modelo <- ajustar_hw(
        y,
        s,
        resultados$alpha[i],
        resultados$beta[i],
        resultados$gamma[i]
      )
      
      posiciones <- which(!is.na(modelo$yhat))
      errores <- y[posiciones] - modelo$yhat[posiciones]
      
      resultados$MSE[i] <- mean(errores^2)
    }
  }
  
  
  # HOLT-WINTERS MULTIPLICATIVO
  
  else if (metodo == "hw_mult") {
    
    if (is.null(s)) {
      stop("Se debe indicar s para Holt-Winters.")
    }
    
    resultados <- expand.grid(
      alpha = rejilla,
      beta = rejilla,
      gamma = rejilla
    )
    
    resultados$MSE <- NA_real_
    
    for (i in 1:nrow(resultados)) {
      
      modelo <- ajustar_hw_mult(
        y,
        s,
        resultados$alpha[i],
        resultados$beta[i],
        resultados$gamma[i]
      )
      
      posiciones <- which(!is.na(modelo$yhat))
      errores <- y[posiciones] - modelo$yhat[posiciones]
      
      resultados$MSE[i] <- mean(errores^2)
    }
  }
  
  
  else {
    stop("Método no reconocido.")
  }
  
  if (all(is.na(resultados$MSE))) {
    stop("Ningún parámetro de la rejilla es válido para esta serie.")
  }
  
  posicion <- which.min(resultados$MSE)
  
  optimo <- resultados[posicion, ]
  
  
  return(
    list(
      metodo = metodo,
      rejilla = resultados,
      optimo = optimo
    )
  )
}


# ORIGEN MOVIL
origen_movil <- function(y, ajustar, H) {
  
  if (!is.numeric(y) || any(is.na(y))) {
    stop("y debe ser numérica y no contener valores faltantes.")
  }
  
  T <- length(y)
  n0 <- floor(0.8 * T)
  
  if (H < 1 || H != as.integer(H) || is.na(H)) {
    stop("H debe ser un entero mayor o igual que 1.")
  }
  
  if (n0 > T - H) {
    stop("La serie es demasiado corta para evaluar este horizonte.")
  }
  
  evaluar_horizonte <- function(h) {
    
    origen <- n0:(T - h)
    
    observado <- numeric(length(origen))
    pronostico <- numeric(length(origen))
    
    for (i in seq_along(origen)) {
      o <- origen[i]
      
      modelo <- ajustar(y[1:o])
      pron <- modelo$pronosticar(h)
      
      observado[i] <- y[o + h]
      pronostico[i] <- pron[h]
    }
    
    data.frame(
      origen = origen,
      h = h,
      observado = observado,
      pronostico = pronostico,
      error = observado - pronostico
    )
  }
  
  return(
    list(
      h1 = evaluar_horizonte(1),
      hH = evaluar_horizonte(H),
      todos = do.call(rbind, lapply(1:H, evaluar_horizonte))
    )
  )
}




# MEDIDAS DE ERROR
medidas <- function(y, yhat, y_estimacion, s = 1) {
  
  stopifnot(is.numeric(y))
  stopifnot(is.numeric(yhat))
  stopifnot(is.numeric(y_estimacion))
  
  if (length(s) != 1 || is.na(s) ||
      s < 1 || s != as.integer(s)) {
    stop("s debe ser un entero mayor o igual que 1.")
  }
  
  if (any(is.na(y)) || any(is.na(yhat)) ||
      any(is.na(y_estimacion))) {
    stop("No se permiten valores faltantes.")
  }
  
  if (length(y) != length(yhat)) {
    stop("y y yhat deben tener la misma longitud.")
  }
  
  if (length(y) < 1) {
    stop("Se necesita al menos una observación para evaluar.")
  }
  
  T_est <- length(y_estimacion)
  
  if (s >= T_est) {
    stop("s debe ser menor que la longitud del tramo de estimación.")
  }
  
  # Error de pronóstico
  e <- y - yhat
  
  # MSE
  MSE <- mean(e^2)
  
  # MAD
  MAD <- mean(abs(e))
  
  # MAPE
  if (any(y == 0)) {
    MAPE <- NA_real_
  } else {
    MAPE <- 100 * mean(abs(e / y))
  }
  
  # MASE: escala calculada en el tramo de estimación
  error_ingenuo <- abs(
    y_estimacion[(s + 1):T_est] -
      y_estimacion[1:(T_est - s)]
  )
  
  escala <- mean(error_ingenuo)
  
  if (escala == 0) {
    MASE <- NA_real_
  } else {
    MASE <- MAD / escala
  }
  
  return(
    list(
      MSE = MSE,
      MAD = MAD,
      MAPE = MAPE,
      MASE = MASE
    )
  )
}



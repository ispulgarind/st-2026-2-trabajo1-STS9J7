# MEDIA SIMPLE

ajustar_media <- function(y) {
  
  stopifnot(is.numeric(y))
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  if (length(y) < 2) {
    stop("Se necesitan al menos 2 observaciones.")
  }
  
  T <- length(y)
  
  yhat <- rep(NA_real_, T)
  
  yhat[2] <- y[1]
  
  suma <- y[1]
  
  for (t in 2:(T - 1)) {
    
    suma <- suma + y[t]
    
    yhat[t + 1] <- suma / t
  }
  
  pronosticar <- function(h) {
    
    if (length(h) != 1 || is.na(h) ||
        h < 1 || h != as.integer(h)) {
      stop("h debe ser un entero mayor o igual que 1.")
    }
    
    rep(mean(y), h)
  }
  
  return(
    list(
      yhat = yhat,
      pronosticar = pronosticar,
      parametros = list()
    )
  )
}



# INGENUO
ajustar_ingenuo <- function(y) {
  
  if (!is.numeric(y)) {
    stop("La serie debe ser numérica.")
  }
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  if (length(y) < 2) {
    stop("Se necesitan al menos 2 observaciones.")
  }
  
  T <- length(y)
  
  yhat <- rep(NA_real_, T)
  
  for (t in 2:T) {
    yhat[t] <- y[t - 1]
  }
  
  pronosticar <- function(h) {
    
    if (length(h) != 1 || is.na(h) ||
        h < 1 || h != as.integer(h)) {
      stop("h debe ser un entero mayor o igual que 1.")
    }
    
    rep(y[T], h)
  }
  
  return(
    list(
      yhat = yhat,
      pronosticar = pronosticar,
      parametros = list()
    )
  )
}


# INGENUO ESTACIONAL
ajustar_ingenuo_estacional <- function(y, s) {
  
  if (!is.numeric(y)) {
    stop("La serie debe ser numérica.")
  }
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  if (length(y) < 2) {
    stop("Se necesitan al menos 2 observaciones.")
  }
  
  if (length(s) != 1 || is.na(s) ||
      s < 1 || s != as.integer(s)) {
    stop("s debe ser un entero mayor o igual que 1.")
  }
  
  T <- length(y)
  
  if (T <= s) {
    stop("Se necesitan más observaciones que el período estacional s.")
  }
  
  yhat <- rep(NA_real_, T)
  
  for (t in (s + 1):T) {
    yhat[t] <- y[t - s]
  }
  
  pronosticar <- function(h) {
    
    if (length(h) != 1 || is.na(h) ||
        h < 1 || h != as.integer(h)) {
      stop("h debe ser un entero mayor o igual que 1.")
    }
    
    pronosticos <- numeric(h)
    
    for (j in 1:h) {
      indice <- T + j - s
      
      while (indice > T) {
        indice <- indice - s
      }
      
      pronosticos[j] <- y[indice]
    }
    
    return(pronosticos)
  }
  
  return(
    list(
      yhat = yhat,
      pronosticar = pronosticar,
      parametros = list(s = s)
    )
  )
}


# MEDIA MÓVIL
ajustar_mm <- function(y, k) {
  
  stopifnot(is.numeric(y))
  
  if (length(k) != 1 || is.na(k) ||
      k < 1 || k != as.integer(k)) {
    stop("k debe ser un entero mayor o igual que 1.")
  }
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  T <- length(y)
  
  if (k > T) {
    stop("La ventana k no puede ser mayor que la longitud de la serie.")
  }
  
  yhat <- rep(NA_real_, T)
  
  suma <- sum(y[1:k])
  
  if (k < T) {
    yhat[k + 1] <- suma / k
  }
  
  if (T > k + 1) {
    
    for (t in (k + 1):(T - 1)) {
      
      suma <- suma - y[t - k]
      suma <- suma + y[t]
      
      yhat[t + 1] <- suma / k
    }
  }
  
  # Pronósticos fuera de la muestra
  pronosticar <- function(h) {
    
    if (length(h) != 1 || is.na(h) ||
        h < 1 || h != as.integer(h)) {
      stop("h debe ser un entero mayor o igual que 1.")
    }
    
    ultimo_mm <- mean(y[(T - k + 1):T])
    
    rep(ultimo_mm, h)
  }
  
  return(
    list(
      yhat = yhat,
      pronosticar = pronosticar,
      parametros = list(k = k)
    )
  )
}


# SUAVIZAMIENTO EXPONENCIAL SIMPLE
ajustar_ses <- function(y, alpha) {
  
  stopifnot(is.numeric(y))
  
  if (length(alpha) != 1 || is.na(alpha) ||
      alpha <= 0 || alpha > 1) {
    stop("alpha debe estar en el intervalo (0, 1].")
  }
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  if (length(y) < 2) {
    stop("Se necesitan al menos 2 observaciones.")
  }
  
  T <- length(y)
  
  yhat <- rep(NA_real_, T)
  
  yhat[2] <- y[1]
  
  if (T > 2) {
    
    for (t in 2:(T - 1)) {
      
      yhat[t + 1] <- alpha * y[t] +
        (1 - alpha) * yhat[t]
    }
  }
  
  pronosticar <- function(h) {
    
    if (length(h) != 1 || is.na(h) ||
        h < 1 || h != as.integer(h)) {
      stop("h debe ser un entero mayor o igual que 1.")
    }
    
    ultimo_pronostico <- alpha * y[T] +
      (1 - alpha) * yhat[T]
    
    rep(ultimo_pronostico, h)
  }
  
  return(
    list(
      yhat = yhat,
      pronosticar = pronosticar,
      parametros = list(alpha = alpha)
    )
  )
}


# DOBLE MEDIA MÓVIL

ajustar_dmm <- function(y, k) {
  
  stopifnot(is.numeric(y))
  stopifnot(length(k) == 1, k >= 2, k == as.integer(k))
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  T <- length(y)
  
  if (k > T) {
    stop("La ventana k no puede ser mayor que la longitud de la serie.")
  }
  
  if (T < 2 * k - 1) {
    stop("Se necesitan al menos 2k - 1 observaciones para calcular la primera doble media móvil.")
  }
  
  if (T < 2 * k) {
    stop("Se necesitan al menos 2k observaciones para calcular el primer pronóstico yhat.")
  }
  
  # CALCULAR LA PRIMERA MEDIA MÓVIL
  
  MM <- rep(NA_real_, T)
  
  suma <- sum(y[1:k])
  MM[k] <- suma / k
  
  if (T > k) {
    for (t in (k + 1):T) {
      suma <- suma - y[t - k]
      suma <- suma + y[t]
      MM[t] <- suma / k
    }
  }
  
  # CALCULAR LA SEGUNDA MEDIA MÓVIL
  
  DMM <- rep(NA_real_, T)
  
  suma_mm <- sum(MM[k:(2 * k - 1)])
  DMM[2 * k - 1] <- suma_mm / k
  
  if (T > 2 * k - 1) {
    for (t in (2 * k):T) {
      suma_mm <- suma_mm - MM[t - k]
      suma_mm <- suma_mm + MM[t]
      DMM[t] <- suma_mm / k
    }
  }
  
  # CALCULAR NIVEL Y TENDENCIA
  
  E <- rep(NA_real_, T)
  beta1 <- rep(NA_real_, T)
  
  for (t in (2 * k - 1):T) {
    E[t] <- 2 * MM[t] - DMM[t]
    
    beta1[t] <- 2 / (k - 1) *
      (MM[t] - DMM[t])
  }
  
  # PRONÓSTICOS DE UN PASO
  
  yhat <- rep(NA_real_, T)
  
  for (t in (2 * k - 1):(T - 1)) {
    yhat[t + 1] <- E[t] + beta1[t]
  }
  
  pronosticar <- function(h) {
    
    if (length(h) != 1 || is.na(h) ||
        h < 1 || h != as.integer(h)) {
      stop("h debe ser un entero mayor o igual que 1.")
    }
    
    pronosticos <- numeric(h)
    
    for (j in 1:h) {
      pronosticos[j] <- E[T] + beta1[T] * j
    }
    
    return(pronosticos)
  }
  
  return(
    list(
      yhat = yhat,
      pronosticar = pronosticar,
      parametros = list(
        k = k,
        E = E[T],
        beta1 = beta1[T],
        trayectoria_E = E,
        trayectoria_beta1 = beta1
      )
    )
  )
}


# Función auxiliar para calcular la información estadística de una regresión por mínimos cuadrados
resumen_regresion <- function(X, y, residuos, nombres = NULL) {
  
  if (!is.matrix(X) || !is.numeric(X)) {
    stop("X debe ser una matriz numérica.")
  }
  
  if (!is.numeric(y) || !is.numeric(residuos)) {
    stop("y y residuos deben ser numéricos.")
  }
  
  if (any(is.na(X)) || any(is.na(y)) || any(is.na(residuos))) {
    stop("No se permiten valores faltantes.")
  }
  
  T <- length(y)
  p <- ncol(X)
  
  if (nrow(X) != T || length(residuos) != T) {
    stop("Las dimensiones de X, y y residuos no coinciden.")
  }
  
  if (T <= p) {
    stop("Se necesitan más observaciones que coeficientes.")
  }
  
  gl <- T - p
  
  XtX <- crossprod(X)
  XtX_inv <- solve(XtX)
  
  sigma2 <- sum(residuos^2) / gl
  
  # Matriz de varianza ordinaria de los coeficientes
  var_ordinaria <- sigma2 * XtX_inv
  
  # Error estándar ordinario
  se_ordinario <- sqrt(diag(var_ordinaria))
  
  # ERROR ESTÁNDAR ROBUSTO HAC
  
  # Número de rezagos para el núcleo de Bartlett
  L <- floor(4 * (T / 100)^(2 / 9))
  
  S <- matrix(0, nrow = p, ncol = p)
  
  # Rezago 0
  for (t in 1:T) {
    xt <- X[t, ]
    S <- S + residuos[t]^2 * tcrossprod(xt)
  }
  
  # Demás rezagos
  if (L > 0) {
    for (h in 1:L) {
      
      # Peso del núcleo de Bartlett
      peso <- 1 - h / (L + 1)
      
      for (t in (h + 1):T) {
        xt <- X[t, ]
        xh <- X[t - h, ]
        
        S <- S +
          peso * residuos[t] * residuos[t - h] *
          (tcrossprod(xt, xh) + tcrossprod(xh, xt))
      }
    }
  }
  
  # Matriz de varianza robusta
  var_robusta <- XtX_inv %*% S %*% XtX_inv
  
  # Errores estándar robustos
  se_robusto <- sqrt(diag(var_robusta))
  
  # Estadístico t y valor p
  beta <- solve(XtX, crossprod(X, y))
  
  t_ordinario <- beta / se_ordinario
  t_robusto <- beta / se_robusto
  
  p_robusto <- 2 * pt(
    -abs(t_robusto),
    df = gl
  )
  
  # R cuadrado
  y_ajustado <- X %*% beta
  
  suma_total <- sum((y - mean(y))^2)
  suma_residuos <- sum(residuos^2)
  
  R2 <- 1 - suma_residuos / suma_total
  
  # Durbin-Watson
  DW <- sum((residuos[-1] - residuos[-T])^2) /
    sum(residuos^2)
  
  # Nombres de los coeficientes
  if (is.null(nombres)) {
    nombres <- paste0("beta_", 0:(p - 1))
  }
  
  # Tabla de coeficientes
  tabla <- data.frame(
    coeficiente = nombres,
    estimacion = as.numeric(beta),
    se_ordinario = se_ordinario,
    se_robusto = se_robusto,
    t = as.numeric(t_robusto),
    valor_p = as.numeric(p_robusto)
  )
  
  rownames(tabla) <- NULL
  
  return(
    list(
      beta = as.numeric(beta),
      tabla = tabla,
      R2 = R2,
      sigma2 = sigma2,
      DW = DW,
      rezagos_HAC = L,
      residuos = residuos,
      y_ajustado = as.numeric(y_ajustado),
      gl = gl
    )
  )
}


# AJUSTAR TENDENCIA
ajustar_tendencia <- function(y, tipo, corregir_sesgo = FALSE) {
  
  stopifnot(is.numeric(y))
  
  if (length(tipo) != 1 || is.na(tipo)) {
    stop("Debes indicar un tipo de tendencia.")
  }
  
  if (length(corregir_sesgo) != 1 ||
      is.na(corregir_sesgo) ||
      !is.logical(corregir_sesgo)) {
    stop("corregir_sesgo debe ser TRUE o FALSE.")
  }
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  if (!tipo %in% c("lineal", "cuadratica", "exponencial")) {
    stop("El tipo debe ser 'lineal', 'cuadratica' o 'exponencial'.")
  }
  
  T <- length(y)
  
  minimo <- if (tipo == "cuadratica") 5 else 4
  
  if (T < minimo) {
    stop("Se necesitan al menos ", minimo, " observaciones.")
  }
  
  
  # TENDENCIA LINEAL
  
  if (tipo == "lineal") {
    
    t <- 1:T
    X <- cbind(1, t)
    
    beta <- solve(
      crossprod(X),
      crossprod(X, y)
    )
    
    ajustados <- as.numeric(X %*% beta)
    residuos <- y - ajustados
    
    resumen <- resumen_regresion(
      X, y, residuos,
      nombres = c("beta_0", "beta_1")
    )
    
    # Pronósticos recursivos de un paso
    yhat <- rep(NA_real_, T)
    
    for (tt in 4:T) {
      
      t_anterior <- 1:(tt - 1)
      X_anterior <- cbind(1, t_anterior)
      
      beta_anterior <- solve(
        crossprod(X_anterior),
        crossprod(X_anterior, y[1:(tt - 1)])
      )
      
      yhat[tt] <- beta_anterior[1] +
        beta_anterior[2] * tt
    }
    
    pronosticar <- function(h) {
      
      if (length(h) != 1 || is.na(h) ||
          h < 1 || h != as.integer(h)) {
        stop("h debe ser un entero mayor o igual que 1.")
      }
      
      t_futuro <- T + (1:h)
      
      as.numeric(beta[1] + beta[2] * t_futuro)
    }
    
    return(
      list(
        yhat = yhat,
        pronosticar = pronosticar,
        parametros = list(
          tipo = "lineal",
          coeficientes = resumen$tabla,
          ajustados = ajustados,
          R2 = resumen$R2,
          sigma2 = resumen$sigma2,
          DW = resumen$DW,
          rezagos_HAC = resumen$rezagos_HAC,
          L_bartlett = resumen$rezagos_HAC,
          grados_libertad = T - 2,
          distribucion_t_robusto = "t con n - 2 grados de libertad",
          residuos = resumen$residuos,
          y_ajustado = resumen$y_ajustado
        )
      )
    )
  }
  
  
  # TENDENCIA CUADRÁTICA
  
  if (tipo == "cuadratica") {
    
    t <- 1:T
    X <- cbind(1, t, t^2)
    
    beta <- solve(
      crossprod(X),
      crossprod(X, y)
    )
    
    ajustados <- as.numeric(X %*% beta)
    residuos <- y - ajustados
    
    resumen <- resumen_regresion(
      X, y, residuos,
      nombres = c("beta_0", "beta_1", "beta_2")
    )
    
    # Pronósticos recursivos de un paso
    yhat <- rep(NA_real_, T)
    
    for (tt in 5:T) {
      
      t_anterior <- 1:(tt - 1)
      
      X_anterior <- cbind(
        1,
        t_anterior,
        t_anterior^2
      )
      
      beta_anterior <- solve(
        crossprod(X_anterior),
        crossprod(X_anterior, y[1:(tt - 1)])
      )
      
      yhat[tt] <- beta_anterior[1] +
        beta_anterior[2] * tt +
        beta_anterior[3] * tt^2
    }
    
    pronosticar <- function(h) {
      
      if (length(h) != 1 || is.na(h) ||
          h < 1 || h != as.integer(h)) {
        stop("h debe ser un entero mayor o igual que 1.")
      }
      
      t_futuro <- T + (1:h)
      
      as.numeric(
        beta[1] +
          beta[2] * t_futuro +
          beta[3] * t_futuro^2
      )
    }
    
    return(
      list(
        yhat = yhat,
        pronosticar = pronosticar,
        parametros = list(
          tipo = "cuadratica",
          coeficientes = resumen$tabla,
          ajustados = ajustados,
          R2 = resumen$R2,
          sigma2 = resumen$sigma2,
          DW = resumen$DW,
          rezagos_HAC = resumen$rezagos_HAC,
          L_bartlett = resumen$rezagos_HAC,
          grados_libertad = T - 3,
          distribucion_t_robusto = "t con n - 3 grados de libertad",
          residuos = resumen$residuos,
          y_ajustado = resumen$y_ajustado
        )
      )
    )
  }
  
  
  # TENDENCIA EXPONENCIAL
  
  if (tipo == "exponencial") {
    
    if (any(y <= 0)) {
      stop("La tendencia exponencial requiere que todos los valores de y sean mayores que 0.")
    }
    
    t <- 1:T
    log_y <- log(y)
    X <- cbind(1, t)
    
    beta_log <- solve(
      crossprod(X),
      crossprod(X, log_y)
    )
    
    log_y_ajustado <- as.numeric(X %*% beta_log)
    ajustados <- exp(log_y_ajustado)
    residuos <- log_y - log_y_ajustado
    
    resumen <- resumen_regresion(
      X, log_y, residuos,
      nombres = c("a_log", "theta")
    )
    
    # Pronósticos recursivos de un paso
    yhat <- rep(NA_real_, T)
    
    for (tt in 4:T) {
      
      t_anterior <- 1:(tt - 1)
      X_anterior <- cbind(1, t_anterior)
      
      beta_anterior <- solve(
        crossprod(X_anterior),
        crossprod(X_anterior, log_y[1:(tt - 1)])
      )
      
      yhat[tt] <- exp(
        beta_anterior[1] +
          beta_anterior[2] * tt
      )
    }
    
    pronosticar <- function(h) {
      
      if (length(h) != 1 || is.na(h) ||
          h < 1 || h != as.integer(h)) {
        stop("h debe ser un entero mayor o igual que 1.")
      }
      
      t_futuro <- T + (1:h)
      
      pronosticos <- exp(
        beta_log[1] + beta_log[2] * t_futuro
      )
      
      if (corregir_sesgo) {
        pronosticos <- pronosticos *
          exp(resumen$sigma2 / 2)
      }
      
      as.numeric(pronosticos)
    }
    
    return(
      list(
        yhat = yhat,
        pronosticar = pronosticar,
        parametros = list(
          tipo = "exponencial",
          coeficientes = resumen$tabla,
          beta_0 = exp(beta_log[1]),
          beta_1 = exp(beta_log[2]),
          ajustados = ajustados,
          R2 = resumen$R2,
          sigma2 = resumen$sigma2,
          DW = resumen$DW,
          rezagos_HAC = resumen$rezagos_HAC,
          L_bartlett = resumen$rezagos_HAC,
          grados_libertad = T - 2,
          distribucion_t_robusto = "t con n - 2 grados de libertad",
          tipo_pronostico = if (corregir_sesgo) {
            "media con correccion exp(sigma2/2)"
          } else {
            "mediana mediante exp(a_log + theta*t)"
          },
          residuos_escala = "log",
          metricas_escala = "original",
          residuos = resumen$residuos,
          y_ajustado = resumen$y_ajustado
        )
      )
    )
  }
}


# TENDENCIA CON FICTICIAS DE SUMA CERO
ajustar_tendencia_estacional <- function(y, s) {
      
    if (!is.numeric(y)) stop("La serie debe ser numérica.")
    if (any(is.na(y))) stop("La serie contiene valores faltantes.")
    if (length(y) < 3) stop("Se necesitan al menos 3 observaciones.")
      
    if (length(s) != 1 || s < 2 || s != as.integer(s)) {
      stop("s debe ser un entero mayor o igual que 2.")
      }
      
    if (length(y) < s + 3) {
      stop("Se necesitan al menos s + 3 observaciones.")
    }
      
    T <- length(y)
    t <- 1:T
      
    temporada <- (t - 1) %% s + 1
      
    X <- cbind(1, t)
      
    for (j in 1:(s - 1)) {
      ficticia <- ifelse(temporada == j, 1, 0) -
        ifelse(temporada == s, 1, 0)
        
      X <- cbind(X, ficticia)
      }
      
    # Ajuste con toda la información
    beta <- solve(
      crossprod(X),
      crossprod(X, y)
      )
      
    ajustados <- as.numeric(X %*% beta)
    residuos <- y - ajustados
      
    nombres <- c("beta_0", "beta_1")
      
    for (j in 1:(s - 1)) {
      nombres <- c(nombres, paste0("gamma_", j))
      }
      
    resumen <- resumen_regresion(
      X,
      y,
      residuos,
      nombres = nombres
      )
      
    # Pronósticos recursivos de un paso
    yhat <- rep(NA_real_, T)
      
    inicio <- s + 3
      
    if (T >= inicio) {
        
      for (tt in inicio:T) {
          
        t_anterior <- 1:(tt - 1)
          
        temporada_anterior <- (t_anterior - 1) %% s + 1
          
        X_anterior <- cbind(1, t_anterior)
          
        for (j in 1:(s - 1)) {
          ficticia <- ifelse(temporada_anterior == j, 1, 0) -
            ifelse(temporada_anterior == s, 1, 0)
            
          X_anterior <- cbind(X_anterior, ficticia)
        }
          
        beta_anterior <- solve(
          crossprod(X_anterior),
          crossprod(X_anterior, y[1:(tt - 1)])
        )
          
        # Estación de la observación que se quiere pronosticar
        temporada_tt <- (tt - 1) %% s + 1
          
        X_tt <- c(1, tt)
          
        for (j in 1:(s - 1)) {
          ficticia <- ifelse(temporada_tt == j, 1, 0) -
            ifelse(temporada_tt == s, 1, 0)
            
          X_tt <- c(X_tt, ficticia)
        }
          
        yhat[tt] <- sum(X_tt * beta_anterior)
      }
    }
      
    pronosticar <- function(h) {
        
      if (length(h) != 1 ||
          h < 1 ||
          h != as.integer(h)) {
        stop("h debe ser un entero mayor o igual que 1.")
      }
        
      t_futuro <- T + (1:h)
      temporada_futura <- (t_futuro - 1) %% s + 1
        
      X_futuro <- cbind(1, t_futuro)
        
      for (j in 1:(s - 1)) {
        ficticia <- ifelse(temporada_futura == j, 1, 0) -
          ifelse(temporada_futura == s, 1, 0)
          
        X_futuro <- cbind(X_futuro, ficticia)
      }
        
      pronosticos <- X_futuro %*% beta
        
      return(as.numeric(pronosticos))
    }
      
    return(
      list(
        yhat = yhat,
        pronosticar = pronosticar,
          
        parametros = list(
          s = s,
          coeficientes = resumen$tabla,
          ajustados = ajustados,
          R2 = resumen$R2,
          sigma2 = resumen$sigma2,
          DW = resumen$DW,
          rezagos_HAC = resumen$rezagos_HAC,
          L_bartlett = resumen$rezagos_HAC,
          grados_libertad = T - (s + 1),
          gamma_s_reportado = FALSE,
          residuos = resumen$residuos,
          y_ajustado = resumen$y_ajustado
        )
      )
    )
  }

# HOLT LINEAL
ajustar_holt <- function(y, alpha, beta) {
  
  stopifnot(is.numeric(y))
  
  if (length(alpha) != 1 || is.na(alpha) ||
      alpha <= 0 || alpha > 1) {
    stop("alpha debe estar en el intervalo (0, 1].")
  }
  
  if (length(beta) != 1 || is.na(beta) ||
      beta <= 0 || beta > 1) {
    stop("beta debe estar en el intervalo (0, 1].")
  }
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  if (length(y) < 2) {
    stop("Se necesitan al menos 2 observaciones.")
  }
  
  T <- length(y)
  
  # Nivel inicial
  L <- rep(NA_real_, T)
  
  # Pendiente inicial
  b <- rep(NA_real_, T)
  
  L[1] <- y[1]
  b[1] <- 0
  
  yhat <- rep(NA_real_, T)
  
  for (t in 2:T) {
    
    yhat[t] <- L[t - 1] + b[t - 1]
    
    L[t] <- alpha * y[t] +
      (1 - alpha) * yhat[t]
    
    b[t] <- beta * (L[t] - L[t - 1]) +
      (1 - beta) * b[t - 1]
  }
  
  pronosticar <- function(h) {
    
    if (length(h) != 1 || is.na(h) ||
        h < 1 || h != as.integer(h)) {
      stop("h debe ser un entero mayor o igual que 1.")
    }
    
    pronosticos <- numeric(h)
    
    for (j in 1:h) {
      pronosticos[j] <- L[T] + b[T] * j
    }
    
    return(pronosticos)
  }
  
  return(
    list(
      yhat = yhat,
      pronosticar = pronosticar,
      parametros = list(
        alpha = alpha,
        beta = beta,
        L = L,
        b = b,
        nivel_final = L[T],
        pendiente_final = b[T]
      )
    )
  )
}


# HOLT WINTERS ADITIVO
ajustar_hw <- function(y, s, alpha, beta, gamma) {
  
  if (!is.numeric(y)) stop("La serie debe ser numérica.")
  if (any(is.na(y))) stop("La serie contiene valores faltantes.")
  
  if (length(s) != 1 || s < 2 || s != as.integer(s)) {
    stop("s debe ser un entero mayor o igual que 2.")
  }
  
  if (length(y) < 2 * s) {
    stop("Se necesitan al menos dos temporadas completas (2s observaciones).")
  }
  
  if (length(alpha) != 1 || alpha <= 0 || alpha > 1) {
    stop("alpha debe estar entre 0 y 1.")
  }
  
  if (length(beta) != 1 || beta <= 0 || beta > 1) {
    stop("beta debe estar entre 0 y 1.")
  }
  
  if (length(gamma) != 1 || gamma <= 0 || gamma > 1) {
    stop("gamma debe estar entre 0 y 1.")
  }
  
  T <- length(y)
  
  L <- rep(NA_real_, T)
  b <- rep(NA_real_, T)
  S <- rep(NA_real_, T + s)
  
  # Inicialización con las dos primeras temporadas
  L[s] <- mean(y[1:s])
  
  b[s] <- (
    mean(y[(s + 1):(2 * s)]) -
      mean(y[1:s])
  ) / s
  
  for (j in 1:s) {
    S[j] <- y[j] - L[s]
  }
  
  yhat <- rep(NA_real_, T)
  
  # Actualizar nivel, pendiente y estacionalidad
  for (t in (s + 1):T) {
    
    if (t > 2 * s) {
      yhat[t] <- L[t - 1] +
        b[t - 1] +
        S[t - s]
    }
    
    L[t] <- alpha *
      (y[t] - S[t - s]) +
      (1 - alpha) *
      (L[t - 1] + b[t - 1])
    
    b[t] <- beta *
      (L[t] - L[t - 1]) +
      (1 - beta) *
      b[t - 1]
    
    S[t] <- gamma *
      (y[t] - L[t]) +
      (1 - gamma) *
      S[t - s]
  }
  
  pronosticar <- function(h) {
    
    if (length(h) != 1 ||
        h < 1 ||
        h != as.integer(h)) {
      stop("h debe ser un entero mayor o igual que 1.")
    }
    
    pronosticos <- numeric(h)
    
    for (j in 1:h) {
      indice_estacional <- T - s + 1 +
        ((j - 1) %% s)
      
      pronosticos[j] <- L[T] +
        b[T] * j +
        S[indice_estacional]
    }
    
    return(pronosticos)
  }
  
  return(
    list(
      yhat = yhat,
      pronosticar = pronosticar,
      parametros = list(
        s = s,
        alpha = alpha,
        beta = beta,
        gamma = gamma,
        L = L,
        b = b,
        S = S[1:T],
        nivel_final = L[T],
        pendiente_final = b[T]
      )
    )
  )
}


# HOLT-WINTERS MULTIPLICATIVO
ajustar_hw_mult <- function(y, s, alpha, beta, gamma) {
  
  if (!is.numeric(y)) stop("La serie debe ser numérica.")
  if (any(is.na(y))) stop("La serie contiene valores faltantes.")
  if (any(y <= 0)) {
    stop("El método multiplicativo requiere que todos los valores de y sean mayores que 0.")
  }
  
  if (length(s) != 1 || s < 2 || s != as.integer(s)) {
    stop("s debe ser un entero mayor o igual que 2.")
  }
  
  if (length(y) < 2 * s) {
    stop("Se necesitan al menos dos temporadas completas (2s observaciones).")
  }
  
  if (length(alpha) != 1 || alpha <= 0 || alpha > 1) {
    stop("alpha debe estar entre 0 y 1.")
  }
  
  if (length(beta) != 1 || beta <= 0 || beta > 1) {
    stop("beta debe estar entre 0 y 1.")
  }
  
  if (length(gamma) != 1 || gamma <= 0 || gamma > 1) {
    stop("gamma debe estar entre 0 y 1.")
  }
  
  T <- length(y)
  
  L <- rep(NA_real_, T)
  b <- rep(NA_real_, T)
  S <- rep(NA_real_, T + s)
  
  # Inicialización con las dos primeras temporadas
  L[s] <- mean(y[1:s])
  
  b[s] <- (
    mean(y[(s + 1):(2 * s)]) -
      mean(y[1:s])
  ) / s
  
  for (j in 1:s) {
    S[j] <- y[j] / L[s]
  }
  
  # Pronósticos de un paso
  yhat <- rep(NA_real_, T)
  
  # Actualizar nivel, pendiente y estacionalidad
  for (t in (s + 1):T) {
    
    if (t > 2 * s) {
      yhat[t] <- (L[t - 1] + b[t - 1]) * S[t - s]
    }
    
    L[t] <- alpha *
      (y[t] / S[t - s]) +
      (1 - alpha) *
      (L[t - 1] + b[t - 1])
    
    b[t] <- beta *
      (L[t] - L[t - 1]) +
      (1 - beta) *
      b[t - 1]
    
    S[t] <- gamma *
      (y[t] / L[t]) +
      (1 - gamma) *
      S[t - s]
  }
  
  pronosticar <- function(h) {
    
    if (length(h) != 1 ||
        h < 1 ||
        h != as.integer(h)) {
      stop("h debe ser un entero mayor o igual que 1.")
    }
    
    pronosticos <- numeric(h)
    
    for (j in 1:h) {
      indice_estacional <- T - s + 1 +
        ((j - 1) %% s)
      
      pronosticos[j] <- (L[T] + b[T] * j) *
        S[indice_estacional]
    }
    
    return(pronosticos)
  }
  
  return(
    list(
      yhat = yhat,
      pronosticar = pronosticar,
      parametros = list(
        s = s,
        alpha = alpha,
        beta = beta,
        gamma = gamma,
        L = L,
        b = b,
        S = S[1:T],
        nivel_final = L[T],
        pendiente_final = b[T]
      )
    )
  )
}




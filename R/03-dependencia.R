#ACF MUESTRAL
acf_muestral <- function(y, m = NULL) {
  
  if (!is.numeric(y)) {
    stop("La serie debe ser numérica.")
  }
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  n <- length(y)
  
  if (n < 2) {
    stop("La serie debe tener al menos dos observaciones.")
  }
  
  if (is.null(m)) {
    m <- min(floor(n / 4), 24)
  }
  
  if (m < 1 || m >= n) {
    stop("El número de rezagos debe ser mayor que 0 y menor que n.")
  }
  
  media <- mean(y)
  
  divisor <- sum((y - media)^2)
  
  r <- numeric(m)
  
  for (h in 1:m) {
    
    numerador <- sum(
      (y[(h + 1):n] - media) *
        (y[1:(n - h)] - media)
    )
    
    r[h] <- numerador / divisor
  }
  
  return(r)
}


#FUNCION pacf_dl
pacf_dl <- function(r) {
  
  if (!is.numeric(r)) {
    stop("Las autocorrelaciones deben ser numéricas.")
  }
  
  if (length(r) < 1) {
    stop("Debe existir al menos una autocorrelación.")
  }
  
  m <- length(r)
  
  # Matriz para guardar los coeficientes
  phi <- matrix(0, m, m)
  
  # Primer coeficiente
  phi[1, 1] <- r[1]
  
  # Recursión de Durbin-Levinson
  if (m >= 2) {
    
    for (k in 2:m) {
      
      numerador <- r[k] - sum(
        phi[k - 1, 1:(k - 1)] * r[(k - 1):1]
      )
      
      denominador <- 1 - sum(
        phi[k - 1, 1:(k - 1)] * r[1:(k - 1)]
      )
      
      phi[k, k] <- numerador / denominador
      
      for (j in 1:(k - 1)) {
        phi[k, j] <- phi[k - 1, j] -
          phi[k, k] * phi[k - 1, k - j]
      }
    }
  }
  
  # La diagonal contiene la PACF
  pacf <- diag(phi)
  
  return(pacf)
}


#PRUEBA LJUNG BOX
ljung_box <- function(r, T, m, p = 0) {
  
  if (!is.numeric(r)) {
    stop("Las autocorrelaciones deben ser numéricas.")
  }
  
  if (T <= 0) {
    stop("T debe ser mayor que 0.")
  }
  
  if (m < 1 || m >= T) {
    stop("m debe ser mayor que 0 y menor que T.")
  }
  
  if (length(r) < m) {
    stop("r no contiene suficientes autocorrelaciones.")
  }
  
  if (p < 0 || p >= m) {
    stop("p debe ser mayor o igual que 0 y menor que m.")
  }
  
  if (any(is.na(r[1:m]))) {
    stop("La ACF contiene valores faltantes.")
  }
  
  Q <- T * (T + 2) *
    sum(r[1:m]^2 / (T - (1:m)))
  
  gl <- m - p
  
  valor_critico <- stats::qchisq(0.95, df = gl)
  
  valor_p <- stats::pchisq(
    Q,
    df = gl,
    lower.tail = FALSE
  )
  
  return(
    list(
      m = m,
      p = p,
      estadistico = Q,
      grados_libertad = gl,
      valor_critico = valor_critico,
      valor_p = valor_p
    )
  )
}


#VARIANZA LARGO PLAZO
varianza_largo_plazo <- function(y, L) {
  
  if (!is.numeric(y)) {
    stop("La serie debe ser numérica.")
  }
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  n <- length(y)
  
  if (n < 2) {
    stop("La serie debe tener al menos dos observaciones.")
  }
  
  if (L < 1 || L >= n) {
    stop("L debe ser mayor que 0 y menor que n.")
  }
  
  media <- mean(y)
  
  # Calcular la varianza de largo plazo
  resultado <- sum((y - media)^2) / n
  
  for (k in 1:L) {
    
    covarianza <- sum(
      (y[(k + 1):n] - media) *
        (y[1:(n - k)] - media)
    ) / n
    
    # Peso del núcleo de Bartlett
    peso <- 1 - k / (L + 1)
    
    resultado <- resultado + 2 * peso * covarianza
  }
  
  return(resultado)
}



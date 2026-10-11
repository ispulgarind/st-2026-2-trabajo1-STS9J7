# MEDIA MÓVIL CENTRADA
media_movil_centrada <- function(y, k) {
  
  stopifnot(is.numeric(y))
  stopifnot(length(k) == 1, k >= 2, k == as.integer(k))
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  T <- length(y)
  
  if (k >= T) {
    stop("La ventana k debe ser menor que la longitud de la serie.")
  }
  
  resultado <- rep(NA_real_, T)
  
  # Si k es impar
  if (k %% 2 == 1) {
    
    mitad <- (k - 1) / 2
    
    for (t in (mitad + 1):(T - mitad)) {
      resultado[t] <- mean(
        y[(t - mitad):(t + mitad)]
      )
    }
    
  } else {
    
    # Si k es par
    mm <- rep(NA_real_, T)
    
    for (t in k:T) {
      mm[t] <- mean(y[(t - k + 1):t])
    }
    
    for (t in (k / 2 + 1):(T - k / 2)) {
      resultado[t] <- mean(
        mm[t - k / 2 + 1],
        mm[t - k / 2 + 2]
      )
    }
  }
  
  return(resultado)
}


#DESCOMPOSICION CLASICA
descomposicion_clasica <- function(y, periodo, tipo = "aditiva") {
  
  if (!is.numeric(y)) {
    stop("La serie debe ser numérica.")
  }
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  if (periodo < 2 || periodo >= length(y)) {
    stop("El período no es válido.")
  }
  
  if (!tipo %in% c("aditiva", "multiplicativa")) {
    stop("El tipo debe ser 'aditiva' o 'multiplicativa'.")
  }
  
  tendencia <- media_movil_centrada(y, periodo)
  
  posicion <- (1:length(y) - 1) %% periodo + 1
  
  # Quitar la tendencia
  if (tipo == "aditiva") {
    valores <- y - tendencia
  } else {
    valores <- y / tendencia
  }
  
  # Calcular los índices estacionales
  indices <- numeric(periodo)
  
  for (j in 1:periodo) {
    
    valores_j <- valores[posicion == j]
    
    indices[j] <- mean(
      valores_j[!is.na(valores_j)]
    )
  }
  
  # Normalizar los índices
  if (tipo == "aditiva") {
    indices <- indices - mean(indices)
  } else {
    indices <- indices / mean(indices)
  }
  
  # Asignar el índice estacional a cada observación
  estacional <- indices[posicion]
  
  # Calcular los residuos
  if (tipo == "aditiva") {
    residuo <- y - tendencia - estacional
  } else {
    residuo <- y / (tendencia * estacional)
  }
  
  return(
    list(
      tendencia = tendencia,
      indices_estacionales = indices,
      estacional = estacional,
      residuo = residuo
    )
  )
}


#FUNCION DIFERENCIAR
diferenciar <- function(y, d = 0, D = 0, s = 1) {
  
  if (!is.numeric(y)) {
    stop("La serie debe ser numérica.")
  }
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  if (d < 0 || d != as.integer(d)) {
    stop("d debe ser un entero mayor o igual a 0.")
  }
  
  if (D < 0 || D != as.integer(D)) {
    stop("D debe ser un entero mayor o igual a 0.")
  }
  
  if (s < 1 || s != as.integer(s)) {
    stop("s debe ser un entero mayor o igual a 1.")
  }
  
  resultado <- y
  
  # Diferencias ordinarias
  if (d > 0) {
    for (i in 1:d) {
      resultado <- resultado[2:length(resultado)] -
        resultado[1:(length(resultado) - 1)]
    }
  }
  
  # Diferencias estacionales
  if (D > 0) {
    for (i in 1:D) {
      
      if (length(resultado) <= s) {
        stop("No hay suficientes observaciones para la diferencia estacional.")
      }
      
      resultado <- resultado[(s + 1):length(resultado)] -
        resultado[1:(length(resultado) - s)]
    }
  }
  
  return(resultado)
}


#POLINOMIO DE REZAGO
polinomio_rezago <- function(coef) {
  
  if (!is.numeric(coef)) {
    stop("Los coeficientes deben ser numéricos.")
  }
  
  if (length(coef) < 1) {
    stop("Debe existir al menos un coeficiente.")
  }
  
  polinomio <- c(1, coef)
  
  # Calcular las raíces
  raices <- polyroot(polinomio)
  
  modulo <- Mod(raices)
  
  resultado <- data.frame(
    raiz = raices,
    modulo = modulo
  )
  
  return(resultado)
}








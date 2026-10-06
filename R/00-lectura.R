leer_serie <- function(x, fuente, unidad) {
  
  datos <- read.csv(x)
  
  if (!all(c("fecha", "valor") %in% names(datos))) {
    stop("El CSV debe tener las columnas 'fecha' y 'valor'.")
  }
  
  fecha <- as.Date(datos$fecha)
  y <- as.numeric(datos$valor)
  
  #Verificaciones
  if (any(is.na(fecha))) {
    stop("Hay fechas inválidas en el CSV.")
  }
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes o no numéricos.")
  }
  
  if (length(y) < 2) {
    stop("Se necesitan al menos dos observaciones.")
  }
  
  if (any(diff(fecha) <= 0)) {
    stop("Las fechas deben estar en orden creciente y sin repetir.")
  }
  
  diferencias <- as.integer(diff(fecha))
  
  if (length(unique(diferencias)) != 1) {
    stop("Las fechas no tienen una frecuencia constante.")
  }
  
  # Identificar la frecuencia
  if (diferencias[1] == 1) {
    frecuencia <- "diaria"
  } else if (diferencias[1] == 7) {
    frecuencia <- "semanal"
  } else {
    frecuencia <- "otra"
  }
  
  resultado <- tibble::tibble(
    t = 1:length(y),
    fecha = fecha,
    y = y
  )
  
  # Guardar información adicional
  attr(resultado, "frecuencia") <- frecuencia
  attr(resultado, "fuente") <- fuente
  attr(resultado, "unidad") <- unidad
  
  return(resultado)
}
# FUNCIÓN PARA GRAFICAR UNA SERIE DE TIEMPO
graficar_serie <- function(datos, titulo) {
  
  ggplot2::ggplot(datos, ggplot2::aes(x = fecha, y = y)) +
    
    ggplot2::geom_line(color = "purple") +
    
    ggplot2::labs(
      title = titulo,
      x = "Fecha",
      y = paste0("Valor (", attr(datos, "unidad"), ")"),
      caption = paste0(
        "Fuente: ", attr(datos, "fuente"),
        " | Observaciones: ", length(datos$y)
      )
    ) +
    
    ggplot2::theme_minimal()
}


# FUNCIÓN PARA GRAFICAR LA ESTACIONALIDAD
grafico_estacional <- function(datos, periodo) {
  
  # Identificar el ciclo y el período dentro del ciclo
  datos$ciclo <- ceiling(datos$t / periodo)
  datos$periodo <- (datos$t - 1) %% periodo + 1
  
  ggplot2::ggplot(
    datos,
    ggplot2::aes(x = periodo, y = y, group = ciclo, color = factor(ciclo))
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = "Gráfico estacional",
      x = "Período",
      y = paste0("Valor (", attr(datos, "unidad"), ")"),
      color = "Ciclo"
    ) +
    ggplot2::theme_minimal()
}


# FUNCIÓN PARA GRAFICAR LOS REZAGOS
grafico_rezagos <- function(datos, rezago) {
  
  # Crear los valores actuales y los valores rezagados
  x <- datos$y[(rezago + 1):length(datos$y)]
  y <- datos$y[1:(length(datos$y) - rezago)]
  
  datos_rezago <- data.frame(
    rezagado = y,
    actual = x
  )
  
  ggplot2::ggplot(
    datos_rezago,
    ggplot2::aes(x = rezagado, y = actual)
  ) +
    ggplot2::geom_point(color = "purple") +
    ggplot2::labs(
      title = paste("Gráfico de rezago", rezago),
      x = paste("Y(t-", rezago, ")"),
      y = "Y(t)"
    ) +
    ggplot2::theme_minimal()
}


# FUNCIÓN PARA CALCULAR Y GRAFICAR EL CORRELOGRAMA
correlograma <- function(datos, m = NULL) {
  
  stopifnot(is.numeric(datos))
  
  if (any(is.na(datos))) {
    stop("La serie contiene valores faltantes.")
  }
  
  T <- length(datos)
  
  if (T < 2) {
    stop("La serie debe tener al menos dos observaciones.")
  }
  
  if (is.null(m)) {
    m <- min(floor(T / 4), 24)
  }
  
  if (m < 1 || m >= T) {
    stop("El número de rezagos debe ser mayor que 0 y menor que T.")
  }
  
  media <- mean(datos)
  
  # Calcular la ACF
  divisor <- sum((datos - media)^2)
  
  acf_valores <- numeric(m)
  
  for (h in 1:m) {
    
    numerador <- sum(
      (datos[(h + 1):T] - media) *
        (datos[1:(T - h)] - media)
    )
    
    acf_valores[h] <- numerador / divisor
  }
  
  # Calcular la PACF
  pacf_valores <- numeric(m)
  
  for (h in 1:m) {
    
    matriz <- matrix(0, h, h)
    
    for (i in 1:h) {
      for (j in 1:h) {
        matriz[i, j] <- ifelse(i == j, 1, acf_valores[abs(i - j)])
      }
    }
    
    vector <- acf_valores[1:h]
    
    pacf_valores[h] <- solve(matriz, vector)[h]
  }
  
  # Banda de confianza
  banda <- 1.96 / sqrt(T)
  
  # Datos para los gráficos
  datos_acf <- data.frame(
    rezago = 1:m,
    valor = acf_valores
  )
  
  datos_pacf <- data.frame(
    rezago = 1:m,
    valor = pacf_valores
  )
  
  # Gráfico ACF
  grafico_acf <- ggplot2::ggplot(
    datos_acf,
    ggplot2::aes(x = rezago, y = valor)
  ) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_hline(
      yintercept = c(-banda, banda),
      linetype = "dashed"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        xend = rezago,
        y = 0,
        yend = valor
      ),
      color = "purple"
    ) +
    ggplot2::labs(
      title = "ACF",
      x = "Rezago",
      y = "Autocorrelación"
    ) +
    ggplot2::theme_minimal()
  
  # Gráfico PACF
  grafico_pacf <- ggplot2::ggplot(
    datos_pacf,
    ggplot2::aes(x = rezago, y = valor)
  ) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_hline(
      yintercept = c(-banda, banda),
      linetype = "dashed"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        xend = rezago,
        y = 0,
        yend = valor
      ),
      color = "purple"
    ) +
    ggplot2::labs(
      title = "PACF",
      x = "Rezago",
      y = "Autocorrelación parcial"
    ) +
    ggplot2::theme_minimal()
  
  # Unir los gráficos
  panel <- patchwork::wrap_plots(
    grafico_acf,
    grafico_pacf,
    ncol = 1
  )
  
  return(
    list(
      acf = acf_valores,
      pacf = pacf_valores,
      banda = banda,
      grafico = panel
    )
  )
}

# UNIR SERIE, ACF Y PACF
panel_diagnostico <- function(grafico_serie, resultado_cor) {
  
  grafico_acf <- resultado_cor$grafico[[1]]
  grafico_pacf <- resultado_cor$grafico[[2]]
  
  panel <- (
    grafico_serie |
      (grafico_acf / grafico_pacf)
  )
  
  return(panel)
}
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


#FUNCION CORRELOGRAMA Y PACF
correlograma <- function(datos, m = NULL) {
  
  if (!is.numeric(datos)) {
    stop("La serie debe ser numérica.")
  }
  
  if (any(is.na(datos))) {
    stop("La serie contiene valores faltantes.")
  }
  
  n <- length(datos)
  
  if (n < 2) {
    stop("La serie debe tener al menos dos observaciones.")
  }
  
  if (is.null(m)) {
    m <- min(floor(n / 4), 24)
  }
  
  if (m < 1 || m >= n) {
    stop("El número de rezagos debe ser mayor que 0 y menor que n.")
  }
  
  r <- acf_muestral(datos, m)
  
  # Calcular la PACF con Durbin-Levinson
  pacf_valores <- pacf_dl(r)
  
  banda_ruido <- 1.96 / sqrt(n)
  
  banda_bartlett <- numeric(m)
  
  for (h in 1:m) {
    
    if (h == 1) {
      suma <- 0
    } else {
      suma <- sum(r[1:(h - 1)]^2)
    }
    
    banda_bartlett[h] <- 1.96 *
      sqrt((1 + 2 * suma) / n)
  }
  
  # Datos para el gráfico ACF
  datos_acf <- data.frame(
    rezago = 1:m,
    valor = r,
    bartlett = banda_bartlett
  )
  
  # Datos para el gráfico PACF
  datos_pacf <- data.frame(
    rezago = 1:m,
    valor = pacf_valores
  )
  
  grafico_acf <- ggplot2::ggplot(
    datos_acf,
    ggplot2::aes(x = rezago, y = valor)
  ) +
    ggplot2::geom_hline(yintercept = 0) +
    
    ggplot2::geom_hline(
      yintercept = c(-banda_ruido, banda_ruido),
      linetype = "dashed",
      color = "red"
    ) +
    
    ggplot2::geom_line(
      ggplot2::aes(
        x = rezago,
        y = bartlett
      ),
      linetype = "dotted",
      color = "blue"
    ) +
    
    ggplot2::geom_line(
      ggplot2::aes(
        x = rezago,
        y = -bartlett
      ),
      linetype = "dotted",
      color = "blue"
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
      y = "Autocorrelación",
      caption = "Rojo discontinuo: banda de ruido blanco ±1,96/√n. Azul punteado: banda de Bartlett."
    ) +
    
    ggplot2::theme_minimal()
  
  # Gráfico PACF
  grafico_pacf <- ggplot2::ggplot(
    datos_pacf,
    ggplot2::aes(x = rezago, y = valor)
  ) +
    ggplot2::geom_hline(yintercept = 0) +
    
    ggplot2::geom_hline(
      yintercept = c(-banda_ruido, banda_ruido),
      linetype = "dashed",
      color = "red"
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
      acf = r,
      pacf = pacf_valores,
      banda_ruido = banda_ruido,
      banda_bartlett = banda_bartlett,
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


# GRÁFICA DE OPTIMIZACIÓN

graficar_optimizacion <- function(resultado, parametro, titulo) {
  
  grafico <- ggplot2::ggplot(
    resultado$rejilla,
    ggplot2::aes(
      x = .data[[parametro]],
      y = MSE
    )
  ) +
    ggplot2::geom_line(
      color = "purple"
    ) +
    ggplot2::geom_point(
      color = "purple"
    ) +
    ggplot2::geom_point(
      data = resultado$optimo,
      ggplot2::aes(
        x = .data[[parametro]],
        y = MSE
      ),
      color = "red",
      size = 4
    ) +
    ggplot2::labs(
      title = titulo,
      x = parametro,
      y = "MSE"
    ) +
    ggplot2::theme_minimal()
  
  return(grafico)
}


# MAPA DE OPTIMIZACIÓN DE HOLT

graficar_holt <- function(resultado) {
  
  grafico <- ggplot2::ggplot(
    resultado$rejilla,
    ggplot2::aes(
      x = alpha,
      y = beta,
      fill = MSE
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(
      low = "lavender",
      high = "purple"
    ) +
    ggplot2::geom_point(
      data = resultado$optimo,
      ggplot2::aes(
        x = alpha,
        y = beta
      ),
      color = "red",
      size = 4
    ) +
    ggplot2::labs(
      title = "MSE según alpha y beta - Holt",
      x = "alpha",
      y = "beta",
      fill = "MSE"
    ) +
    ggplot2::theme_minimal()
  
  return(grafico)
}


# MAPA DE OPTIMIZACIÓN DE HOLT-WINTERS
graficar_hw <- function(resultado, titulo) {
  
  gamma_optimo <- resultado$optimo$gamma
  
  datos <- resultado$rejilla[
    resultado$rejilla$gamma == gamma_optimo,
  ]
  
  grafico <- ggplot2::ggplot(
    datos,
    ggplot2::aes(
      x = alpha,
      y = beta,
      fill = MSE
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(
      low = "lavender",
      high = "purple"
    ) +
    ggplot2::geom_point(
      data = resultado$optimo,
      ggplot2::aes(
        x = alpha,
        y = beta
      ),
      color = "red",
      size = 4
    ) +
    ggplot2::labs(
      title = titulo,
      subtitle = paste("gamma óptimo =", gamma_optimo),
      x = "alpha",
      y = "beta",
      fill = "MSE"
    ) +
    ggplot2::theme_minimal()
  
  return(grafico)
}
# VERIFICACION diferenciar()
y <- 1:10

resultado_propio <- diferenciar(y, d = 1)
resultado_R <- diff(y)

all.equal(resultado_propio, resultado_R)


# Verificación de diferencia estacional

y <- 1:15

resultado_propio <- diferenciar(y, D = 1, s = 3)
resultado_R <- y[4:15] - y[1:12]

all.equal(resultado_propio, resultado_R)


#VERIFICACION pacf_dl
y_prueba <- c(
  10, 12, 13, 15, 14, 16, 18, 17, 19, 21,
  20, 22, 24, 23, 25, 27, 26, 28, 30, 29
)

m <- 8

r <- acf(y_prueba, lag.max = m, plot = FALSE)$acf[-1]

pacf_propia <- pacf_dl(r)

pacf_R <- pacf(y_prueba, lag.max = m, plot = FALSE)$acf

pacf_propia
pacf_R

# Máxima diferencia absoluta
max(abs(pacf_propia - pacf_R))


#VERIFIACIONES DE LOS MÉTODOS

#MEDIA SIMPLE
y <- c(10, 12, 14, 16, 18)
modelo <- ajustar_media(y)
modelo$yhat
modelo$pronosticar(3)

# INGENUO
y <- c(10, 12, 14, 16, 18)
modelo <- ajustar_ingenuo(y)
modelo$yhat
modelo$pronosticar(3)

# INGENUO ESTACIONAL
y <- c(10, 20, 30, 40,
       12, 22, 32, 42)
modelo <- ajustar_ingenuo_estacional(y, s = 4)
modelo$yhat
modelo$pronosticar(4)

# MEDIA MÓVIL
y <- c(18.43, 19.98, 19.51, 20.63,
       19.78, 21.25, 21.28, 22.14)
resultado <- ajustar_mm(y, k = 3)
resultado$yhat
resultado$pronosticar(3)

# SUAVIZAMIENTO EXPONENCIAL SIMPLE
y <- c(10, 12, 13, 15, 16, 18, 20, 21)
resultado <- ajustar_ses(y, alpha = 0.3)
resultado$yhat
resultado$pronosticar(3)

# DOBLE MEDIA MÓVIL
y <- c(133, 155, 165, 171, 194, 231, 274, 312, 313, 333, 343)
resultado <- ajustar_dmm(y, k = 3)
resultado$yhat
resultado$pronosticar(3)

# TENDENCIA LINEAL
y <- c(10, 12, 13, 15, 16, 18, 20, 21)
resultado <- ajustar_tendencia(y, tipo = "lineal")
resultado$yhat
resultado$pronosticar
resultado$parametros$coeficientes

# Comparación con lm()
t <- 1:length(y)
modelo_lm <- lm(y ~ t)

coef(modelo_lm)

# TENDENCIA CUADRÁTICA
y <- c(10, 12, 13, 15, 16, 18, 20, 21)
resultado <- ajustar_tendencia(y, tipo = "cuadratica")
resultado$yhat
resultado$pronosticar(3)
resultado$parametros$coeficientes

# Comparación con lm()
t <- 1:length(y)
modelo_lm <- lm(y ~ t + I(t^2))
coef(modelo_lm)

# TENDENCIA EXPONENCIAL
y <- c(10, 12, 13, 15, 16, 18, 20, 21)
resultado <- ajustar_tendencia(y, tipo = "exponencial")
resultado$yhat
resultado$pronosticar(3)
resultado$parametros$coeficientes

# Comparación con lm()
t <- 1:length(y)
modelo_lm <- lm(log(y) ~ t)
coef(modelo_lm)

# TENDENCIA CON FICTICIAS
y <- c(10, 14, 12, 16,
       12, 16, 14, 18)
resultado <- ajustar_tendencia_estacional(y, s = 4)
resultado$yhat
resultado$pronosticar(4)
resultado$parametros$coeficientes

# Comparación con lm()
t <- 1:length(y)
temporada <- (t - 1) %% 4 + 1

f1 <- ifelse(temporada == 1, 1, 0) -
  ifelse(temporada == 4, 1, 0)

f2 <- ifelse(temporada == 2, 1, 0) -
  ifelse(temporada == 4, 1, 0)

f3 <- ifelse(temporada == 3, 1, 0) -
  ifelse(temporada == 4, 1, 0)

modelo_lm <- lm(y ~ t + f1 + f2 + f3)

coef(modelo_lm)

# HOLT
y <- c(10, 12, 15, 14, 18, 20)
resultado <- ajustar_holt(y, alpha = 0.3, beta = 0.2)
resultado$yhat
resultado$pronosticar(3)

# HOLT-WINTERS ADITIVO
y <- c(10, 12, 14, 16,
       12, 14, 16, 18)
resultado <- ajustar_hw(y, s = 4,
                        alpha = 0.3,
                        beta = 0.2,
                        gamma = 0.4)
resultado$yhat
resultado$pronosticar(4)

# HOLT-WINTERS MULTIPLICATIVO
y <- c(10, 12, 14, 16,
       12, 14, 16, 18)

resultado <- ajustar_hw_mult(y, s = 4,
                             alpha = 0.3,
                             beta = 0.2,
                             gamma = 0.4)
resultado$yhat
resultado$pronosticar(4)












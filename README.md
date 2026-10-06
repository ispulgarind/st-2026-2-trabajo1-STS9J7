https://github.com/ispulgarind/st-2026-2-trabajo1-STS9J7

### Archivos reutilizados y modificaciones

**R/00-lectura.R — `leer_serie()`**
**Responsable: Isabela Pulgarín**

Se reutilizó la función `leer_serie()` desarrollada en la Tarea 1 y se adaptó al Trabajo 1.

Cambios realizados:

* Se eliminó el manejo de objetos `ts`, ya que en este trabajo las series se entregan directamente en archivos CSV.
* Se adaptó la función para reconocer las frecuencias de las series entregadas: diaria y semanal.
* Se agregó la verificación de que las fechas tengan una frecuencia constante y no presenten huecos.
* Se mantuvo la validación de las columnas `fecha` y `valor`.
* Se mantuvo la verificación de fechas inválidas, valores faltantes, orden de las fechas y número mínimo de observaciones.
* Se conservaron los atributos `fuente` y `unidad`.
* Se mantuvo la salida como una tabla con las columnas `t`, `fecha` y `y`.

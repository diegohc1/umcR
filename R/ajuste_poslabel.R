#' @name ajuste_poslabel
#'
#' @title Ajusta las posiciones de las etiquetas para un gráfico de barras
#'
#' @description Con el objetivo de graficar barras apiladas, usualmente se calcula un vector con las posiciones de las etiquetas. Es posible que las etiquetas estes muy pegadas o se solapen, si es asi, la función agrega unos puntos a la posición de la etiqueta.
#'
#' @param data Tabla con los datos para un gráfico de barras
#' @param posl Vector con la posición de las etiquetas en las barras apiladas
#' @param freq Valores por graficar (porcentajes, usualmente)
#' @param xxn Opciones o categorias asociados a los porcentajes
#'
#' @return Agrega una columna 'poslabel_aj' a la tabla de datos
#' @export
#' @importFrom magrittr "%>%"
#' @import dplyr
NULL
#'
#' @examples
#' opcion <- c("a", "b", "c", "d", "e")
#' freq = c(7, 46.7, 0.5, 1.4, 44.4)
#' pos_label = c(3.5, 30.35, 53.95, 54.9, 77.8)
#' bd <- data.frame(opcion = opcion, freq = freq, pos_label = pos_label)
#' bd1 <- ajuste_poslabel(bd, pos_label, freq, opcion)
#'
#'
ajuste_poslabel <- function(data, posl, freq, xxn){
  posl <- enquo(posl) #posicion actual de la etiqueta
  freq <- enquo(freq) #porcentajes
  xxn <- enquo(xxn) #opcion de respuesta

  data %>%
    mutate(
      dif = !!posl - lag(!!posl, order_by = !!xxn), #diferencia entre las posiciones de las etiquetas contiguas
      poslabel_aj = (case_when( #si dif es pequeño, sumarle unos puntos
        dif < 3 & !!freq >= 3 ~ !!posl + 3, #ajustar
        dif < 3 & !!freq < 3 ~ !!posl + 2,
        TRUE ~ !!posl)),
      poslabel_aj = ifelse(is.na(poslabel_aj), !!posl, poslabel_aj)) #por si acaso queda un NA por el lag
}

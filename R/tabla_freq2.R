#' Tabla de frecuencias para varias columnas
#'
#' Tabla de frecuencias para varias columnas que empiezan con el mismo nombre y tienen el mismo contenido. Útil para un grupo de ítems de una escala, por ejemplo.
#'
#' @param data Base de datos
#' @param x Caracter con el que empieza el grupo de columnas
#'
#' @return DF
#' @export
#' @importFrom magrittr "%>%"
#' @import dplyr purrr
NULL
#'
#' @examples
#' tabla_freq2(data_ejemplo, "p02")
#'

tabla_freq2 <- function(data, x){

  data %>%
    select(starts_with(x)) %>%
    map(tabla_freq1) %>%
    pega_lista("var")

}

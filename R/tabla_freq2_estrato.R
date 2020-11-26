#' Tabla de frecuencias para varias columnas, según estrato
#'
#' Tabla de frecuencias para varias columnas que empiezan con el mismo nombre y tienen el mismo contenido, agrupando por característica o estrato.
#'
#' @param data Base de datos
#' @param x Caracter con el que empieza el grupo de columnas
#' @param estrato Nombre de la columna que contiene los estratos o grupos
#'
#' @return
#' @export
#' @importFrom magrittr "%>%"
#' @import dplyr purrr
NULL
#'
#' @examples
#' tabla_freq2_estrato(data_ejemplo, "p02", "gestion")

tabla_freq2_estrato <- function(data, x, estrato){

  split(data, data[estrato]) %>%
    map(~select(.x, starts_with(x))) %>%
    map(~map(.x, ~as.data.frame(round(prop.table(table(.x))*100, 1)))) %>%
    map(~pega_lista(.x, "var")) %>%
    pega_lista("Estrato") %>%
    rename(x = 1)

}

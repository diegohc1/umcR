#' Title Tabla de frecuencias para varias columnas que empiezan con el mismo nombre y tienen el mismo contenido
#'
#' @param data Base de datos
#' @param x Caracter con el que empieza el grupo de columnas
#'
#' @return DF
#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples
#' tabla_freq2(data_ejemplo, "p02")
#'

tabla_freq2 <- function(data, x){

  data %>%
    dplyr::select(dplyr::starts_with(x)) %>%
    purrr::map(., tabla_freq1) %>%
    pega_lista(., "var")

}

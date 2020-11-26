#' Une dataframes que estan en una lista
#'
#' En una lista de dataframes con la misma estructura, agrega una columna con el nombre de la columna y las une. Útil cuando cuando se hizo un 'split' por algún estrato.
#'
#'
#' @param data Lista con dataframes
#' @param nc Asigna el nombre de la columna con los nombres de las listas
#'
#' @return Dataframe
#' @export
#' @importFrom magrittr "%>%"
#' @import dplyr purrr
NULL
#' @encoding UTF-8
#'
#' @examples
#' ejem <- list(hom = data.frame(v = 1:3), muj = data.frame(v = 4:6))
#' pega_lista(ejem, "nombre")
#'
#'
pega_lista <- function(data, nc){

  if(!is.list(data))
    stop("Data debe ser una lista")

  imap(data, mutate(.x, !!nc := .y)) %>%
    bind_rows() #para bind listas
}

#' Igual que table(), pero en dataframe y porcentajes redondeados. Regresa un dataframe.
#'
#' @param x Vector para aplicar tabla de frecuencias
#'
#' @return df
#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples
#' tabla_freq1(iris$Species)

tabla_freq1 <- function(x){

  as.data.frame(round(prop.table(table(x))*100, 1))

}

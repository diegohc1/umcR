#' @name contar_nse_segregacion
#'
#' @title Cuenta nse del estudiante según escuelas y clasifica segregación
#'
#' @description Se debe tener una base de datos con el nivel socioeconómico (NSE) de los estudiantes, el código modular y el area de la escuela. La función cuenta a los estudiantes en la escuela según su NSE (agrupando a los de NSE bajo y muy bajo) y despues asigna un nivel de segregación según los cortes c1 y c2. Finalmente, categoriza a las escuelas rurales.
#'
#' @param data Base de datos
#' @param nse Variable que contiene el nivel socioeconómico, debe estar en números. Por ejemplo, 'nse alto' = 1, 'nse medio' = 2,...
#' @param codmod Variable que agrupa a los estudiantes, en principio la escuela (codmod7)
#' @param area Variable con el area de la escuela, debe contener 'Rural' y 'Urbano'
#' @param c1 Corte para asignar segregación media
#' @param c2 Corte para asignar segregación alta
#'
#' @return
#' @export
#' @importFrom magrittr "%>%"
#' @import dplyr
NULL
#' @encoding UTF-8
#' @examples
#' contar_nse_segregacion(data_ejemplo, nse, codmod7, area, 50, 90)
#'
contar_nse_segregacion <- function(data, nse, codmod, area, c1, c2){

  if(!is.numeric(data$nse))
    stop("'Variable de nse' debe ser númerico")

  if(!(unique(data$area) %in%  c("Urbana", "Rural"))[1])
    stop("'Variable de area' debe contener 'Rural' y 'Urbana'")

  nse <- enquo(nse); codmod <- enquo(codmod); area <- enquo(area)

  data1 <- data %>%
    filter(!is.na(!!nse)) %>%
    mutate(totalto = ifelse(!!nse == 1, 1, 0), #nse en numeros!
           totmedio = ifelse(!!nse == 2, 1, 0),
           totbajo = ifelse(!!nse == 3, 1, 0),
           totmbajo = ifelse(!!nse == 4, 1, 0),
           toalum = 1) %>%
    group_by(!!codmod) %>%
    summarise(area = first(!!area), across(starts_with("to"), ~sum(.))) %>%
    mutate(across(starts_with("tot"), ~round((./toalum)*100, 2)),
           totbajo2 = totbajo + totmbajo)

  data2 <-
    data1 %>% mutate(segreg2 = (
      case_when(
        totalto >= c1 & totalto < c2 ~ "media NSE alto",
        totmedio >= c1 & totmedio < c2 ~ "media NSE medio",
        totbajo2 >= c1 & totbajo2 < c2 ~ "media NSE bajo",
        totalto >= c2 ~ "alta NSE alto",
        totmedio >= c2 ~ "alta NSE medio",
        totbajo2 >= c2 ~ "alta NSE bajo",
        TRUE ~ "No segregado"))) %>%
    mutate(segreg2 = ifelse(area == "Rural", "Rural", segreg2)) %>%
    mutate(segreg1 = ifelse(stringr::str_detect(segreg2, "alta"), "Segregación alta",
                            ifelse(stringr::str_detect(segreg2, "media"), "Segregación media", segreg2)))

  return(data2)

}

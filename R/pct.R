#'Cambio porcentual
#'
#'Funcion sencilla para calcular el cambio porcentual entre dos periodos
#'
#'@param ayer
#'Valor inicial
#'
#'@param hoy
#'Valor final
cp <- function(ayer, hoy){

  resultado <- round((hoy-ayer)/ayer*100, 2)
  return(resultado)

}



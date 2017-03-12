setClassUnion('characterOrNULL', c('character', 'NULL'))
#' Clase S4 BestTSPredParam para los parámetros del método BestTSPred
#'
#' Definición de la clase S4 \linkS4class{BestTSPredParam} para los parámetros que
#' hay que pasar como argumentos a la función \code{BestTSPred}.
#'
#' La estructura de la clase S4 \code{BestTSPredParam} consta de:
#' \itemize{
#' \item el slot \code{TSPred.list} de clase \code{\link{list}} con los argumentos
#' necesarios para llamar a las funciones de los distintos modelos de series
#' temporales que generan las predicciones. Se trata de listas cuya primera componente
#' es el nombre de la función y el resto son los argumentos que requieren dichas
#' funciones. El output de cada unos de estos modelos debe cumplir con los requisitos
#' de diseño de las funciones en el paquete \code{TSPred};
#' \item un slot opcional de tipo \code{character} con los nombres de las variables
#' a predecir;
#'
#' @slot TSPred.list Objeto de clase \code{\link{list}} cuyos componentes contienen
#' los elementos de cada llamada a las funciones que calculan las predicciones de
#' acuerdo a los distintos modelos.
#'
#' @slot VarNames \code{Vector} de tipo \code{character} con los nombres de las
#' variables a las que corresponden las predicciones.
#'
#' @examples
#' # Un prototipo vacío
#' new(Class = 'BestTSPredParam')
#'
#' @export
setClass(
  Class = 'BestTSPredParam',
  slots = c(TSPred.list = "list",
            VarNames = "characterOrNULL"),
  validity = function(object){
    if (length(object@TSPred.list) == 0) stop('[BestTSPred::BestTSPredParam validity] The slot TSPred.list must have at least one component.')

    Forwards <- as.integer(unlist(lapply(object@TSPred.list, function(List){List[['forward']]})))

    if (length(unique(Forwards)) != 1) stop('[BestTSPredParam: validation] Todos los parámetros forward en el slot TSPred.list deben ser el mismo.')
    return(TRUE)
  }
)

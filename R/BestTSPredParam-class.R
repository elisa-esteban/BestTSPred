setClassUnion('characterOrNULL', c('character', 'NULL'))
#' Clase S4 BestTSPredParam para los parametros del metodo BestTSPred
#'
#' Definicion de la clase S4 \linkS4class{BestTSPredParam} que contiene los parametros que utiliza
#' el metodo \code{BestTSPred}.
#'
#' @slot TSPred.list Objeto de clase \code{\link{list}} cuyos componentes son listas que contienen
#' el nombre y los argumentos de las funciones con las que se calcularan las predicciones de acuerdo
#' a los distintos modelos de series temporales.
#'
#' @slot VarNames \code{Vector} de tipo \code{character} con los nombres de las variables para las
#' que se calcularan las predicciones.
#'
#' @examples
#' # Un prototipo vacío
#' new(Class = 'BestTSPredParam')
#'
#' TS.list <- list(Reg = list('RegDiffTSPred', forward = 2L),
#'                 Stat = list('StatDiffTSPred', forward = 2L),
#'                 StatReg = list('StatRegDiffTSPred', forward = 2L),
#'                 Arima = list('AutoArimaTSPred', forward = 2L))
#' VarNames <- c('CifraNeg_13.___', 'Personal_07.__1._1._')
#' BestTSPredParam <- new(Class='BestTSPredParam', TSPred.list = TS.list, VarNames = VarNames)
#'
#'
#' @export
setClass(
  Class = 'BestTSPredParam',
  slots = c(TSPred.list = "list",
            VarNames = "characterOrNULL"),
  validity = function(object){

    if (length(object@TSPred.list) == 0) stop('[BestTSPred::BestTSPredParam validity] El slot TSPred.list debe tener al menos una componente.')

    TSPredMethods <- unlist(lapply(object@TSPred.list, function(List){List[[1]]}))
    Forwards <- unique(as.integer(unlist(lapply(object@TSPred.list, function(List){List[['forward']]}))))

    if (!('FixedTSPred' %in% TSPredMethods) & length(Forwards) > 1) stop('[BestTSPred::BestTSPredParam validity] El valor del parametro forward debe ser el mismo en todas las componentes del slot TSPred.list.')
    return(TRUE)
  }
)

#' Inicializador de objetos de clase \linkS4class{BestTSPredParam}.
#'
#' Crea un objeto de clase \linkS4class{BestTSPredParam} con los parámetros que
#' utiliza como argumentos el método \linkS4class{BestTSPred}.
#'
#' Esta función construye un objeto de clase \code{BestTSPredParam} a partir
#' de una lista que contiene los nombres y los parámetros de los distintos
#' modelos de predicción de series temporales con los que se obtienen predicciones,
#' los nombres de las variables a predecir y los nombres de las variables que se
#' utilizarán como variables clave.
#'
#' @param .Object Parámetro obligatorio para todos los inicializadores.
#'
#' @param TSPred.list Objeto de clase \code{list} cuyas componentes contienen los
#' nombres y los parámetros de los modelos con los que se generan las
#' distintas predicciones de los datos.
#'
#' @param VarNames \code{Vector} de tipo \code{character} con los nombres de las
#' variables a predecir. Por defecto, toma el valor \code{NULL}
#'
#' @return Objeto de clase \linkS4class{BestTSPredParam}.
#'
#' @examples
#' TS.list <- list(Reg = list('RegDiffTSPred', forward = 2L),
#'                 Stat = list('StatDiffTSPred', forward = 2L),
#'                 StatReg = list('StatRegDiffTSPred', forward = 2L))
#' BestTSPredParam1 <- new(Class='BestTSPredParam',
#'                        TSPred.list = TS.list)
#' str(BestTSPredParam1)
#'
#' BestTSPredParam2 <- new(Class='BestTSPredParam',
#'                        TSPred.list = TS.list[1:2])
#' str(BestTSPredParam2)
#'
#' @include BestTSPredParam-class.R
#'
#' @export
setMethod(
  f = "initialize",
  signature = "BestTSPredParam",
  definition = function(.Object,
                        TSPred.list = stop('[BestTSPredParam initialize] Es necesario especificar una lista de modelos de predicción.'),
                        VarNames = NULL
  ){

    .Object@TSPred.list <- TSPred.list
    .Object@VarNames <- VarNames

    validObject(.Object)

    return(.Object)
  }
)

#' Selección de la mejor predicción de un conjunto de modelos de series temporales.
#'
#' \code{BestTSPred} Selecciona la mejor predicción para una serie de tiempo de
#' entre las proporcionadas por un conjunto de modelos.
#'
#' Este método toma como argumento de entrada el objeto \code{x} que contiene las
#' variables que queremos predecir y selecciona la mejor predicción de entre un
#' conjunto de predicciones especificadas como input en el objeto de clase
#' \linkS4class{BestTSPredParam}. Este parámetro contiene los diferentes modelos
#' de predicción de series temporales que generan las distintas predicciones.
#'
#'
#' @param x \code{vector}, \code{matrix} u objeto de clase \code{StQList}, con las variables y los
#' valores para los que obtenemos las predicciones.
#'
#' @param BestTSPredParam Objeto de clase \linkS4class{BestTSPredParam} con los parámetros
#' de los distintos modelos de predicción de series temporales con los que se generan
#' las diferentes predicciones y las variables sobre las que se calculan.
#'
#' @return Lista con componentes \code{Pred} y \code{STD} que contienen
#' la mejor predicción de entre las especificadas en el objeto de entrada
#' \code{BestTSPredParam}, y sus correspondientes desviaciones típicas estimadas,
#' respectivamente.
#'
#' \itemize{
#'  \item For input class vector, it returns numeric vectors.
#'  \item For input class matrix, it returns matrices.
#'  \item For input class StQList, it returns list whose components are
#'   data.tables.
#' }
#'
#' @examples
#'
#' # Predicting one and two months ahead in time
#' data(Example1.TS)
#' TS.list <- list(Reg = list('RegDiffTSPred', forward = 2L),
#'                 Stat = list('StatDiffTSPred', forward = 2L),
#'                 StatReg = list('StatRegDiffTSPred', forward = 2L))
#' BestTSPredParam <- new(Class='BestTSPredParam', TSPred.list = TS.list)
#' BestTSPred(Example1.TS, BestTSPredParam)
#'
#' \dontrun{
#' # With an object of class StQList
#' data(StQListExample)
#' VarNames <- c('ActivEcono_35._6._2.1.4._0', 'GeoLoc_35._6._2.1._1.2.5.')
#' StQList <- readRDS('../E30183.FF.StQList.rds')
#' Units <- StQ::getUnits(StQList[['MM102016']])
#' Units <- Units[sample(1:(dim(Units)[1]), 1000)]
#' data.table::setkeyv(Units, 'NOrden')
#' StQ::setUnits(StQList) <- Units
#' VarNames <- c('CifraNeg_13.___', 'Personal_07.__1._1._')
#' TS.list <- list(Reg = list('RegDiffTSPred', forward = 2L),
#'                 Stat = list('StatDiffTSPred', forward = 2L),
#'                 StatReg = list('StatRegDiffTSPred', forward = 2L),
#'                 Arima = list('AutoArimaTSPred', forward = 2L))
#' BestTSPredParam <- new(Class='BestTSPredParam', TSPred.list = TS.list, VarNames = VarNames)
#' BestTSPred(StQListExample, BestTSPredParam)
#' }
#'
#' @import data.table RepoTime  StQ TSPred parallel
#'
#' @include BestTSPredParam-class.R
#'
#' @export
setGeneric("BestTSPred", function(x, BestTSPredParam){standardGeneric("BestTSPred")})

#' @rdname BestTSPred
#'
#' @export
setGeneric("BestTSPred", function(x, BestTSPredParam){standardGeneric("BestTSPred")})

#' @rdname BestTSPred
#'
#' @export
setMethod(
  f = "BestTSPred",
  signature = c("vector"),
  function(x, BestTSPredParam){

    n_cores <- max(1, detectCores() - 1)
    clust <- makeCluster(n_cores)

    clusterExport(clust, c('x', 'BestTSPredParam'), envir = environment())
    clusterEvalQ(clust, library(data.table))
    clusterEvalQ(clust, library(TSPred))
    clusterEvalQ(clust, library(BestTSPred))

    Results.List <- parLapply(clust, seq(along = BestTSPredParam@TSPred.list), function(TSPred){

      Function <- BestTSPredParam@TSPred.list[[TSPred]][[1L]]
      Param.List <- list()
      Param.List[['x']] <- x
      if (length(BestTSPredParam@TSPred.list[[TSPred]]) >= 2) Param.List <- c(Param.List, BestTSPredParam@TSPred.list[[TSPred]][-1])
      out <- do.call(Function, Param.List)
      return(out)

    })

    stopCluster(clust)

    STD <- rbindlist(Results.List)[['STD']]


    if (all(is.na(STD))) return(list(Pred = as.numeric(NA), STD = as.numeric(NA)))
    MinSTD.index <- which.min(STD)

    output <- Results.List[[MinSTD.index]]

    return(output)
  }
)
#' @rdname BestTSPred
#'
#' @export
setMethod(
  f = "BestTSPred",
  signature = c("StQList"),
  function(x, BestTSPredParam){

    VarNames <- BestTSPredParam@VarNames

    if (length(VarNames) == 0) {

      stop('[BestTSPred::BestTSPred StQList] Slot VarNames in the parameter BestTSPredParam must be specified.')
    }

    x_StQ <- StQListToStQ(x)
    DT <- dcast_StQ(x_StQ, ExtractNames(VarNames))
    IDQuals <- setdiff(names(DT), c(VarNames, 'Period'))
    DT[, orderPeriod := orderRepoTime(Period), by = IDQuals]
    setkeyv(DT, c(IDQuals, 'orderPeriod'))

    if (length(VarNames) == 1) {

      output <- DT[ ,BestTSPred(get(VarNames), BestTSPredParam = BestTSPredParam),
                    by = IDQuals]
      setnames(output, c('Pred', 'STD'), paste0(c('Pred', 'STD'), VarNames))


    } else {


      output <- lapply(seq(along = BestTSPredParam@TSPred.list), function(TSPred){

        Function <- BestTSPredParam@TSPred.list[[TSPred]][[1L]]
        Param.List <- list()
        Param.List[['x']] <- x
        Param.List[['VarNames']] <- BestTSPredParam@VarNames
        if (length(BestTSPredParam@TSPred.list[[TSPred]]) >= 2) Param.List <- c(Param.List, BestTSPredParam@TSPred.list[[TSPred]][-1])
        out <- do.call(Function, Param.List)
        rm(Param.List)
        gc()
        return(out)

      })


      names(output) <- names(BestTSPredParam_logit@TSPred.list)
      lapply(names(output), function(name){output[[name]][, PredMethod := name]})
      output <- rbindlist(output)
      setkeyv(output, c(IDQuals, 'PredMethod'))
      output <- melt.data.table(output, id.vars = c(IDQuals, 'PredMethod'), measure.vars = setdiff(names(output), c(IDQuals, 'PredMethod')))
      output[, VarAnalisis := ifelse(substr(variable, 1, 3) == 'STD', substr(variable, 4, length(variable)), substr(variable, 5, length(variable)))]
      outSTD <- output[grep('STD', variable)]
      setorderv(outSTD, c('NOrden', 'VarAnalisis', 'value'), na.last = TRUE)
      out_STS_min <- outSTD[!duplicated(outSTD, by = c(IDQuals, 'VarAnalisis'))]
      out_STS_min <- out_STS_min[, c('variable', 'value') := NULL]
      output <- merge(output, out_STS_min, by = c(IDQuals, 'VarAnalisis'))
      output <- subset(output, PredMethod.x == PredMethod.y)[, c('VarAnalisis', 'PredMethod.x', 'PredMethod.y') := NULL]
      Form <- paste0(IDQuals, ' ~ variable')
      output <- dcast(output, as.formula(Form), value.var = 'value')


    }

    return(output)

  }
)

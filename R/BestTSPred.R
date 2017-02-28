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
#' @include BestTSPredParam-class.R
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
#' data(StQList_Example)
#' VarNames <- c('ActivEcono_35._6._2.1.4._0', 'GeoLoc_35._6._2.1._1.2.5.')
#' TS.list <- list(Reg = list('RegDiffTSPred', forward = 2L),
#'                 Stat = list('StatDiffTSPred', forward = 2L),
#'                 StatReg = list('StatRegDiffTSPred', forward = 2L),
#'                 Arima = list('AutoArimaTSPred', forward = 2L))
#' BestTSPredParam <- new(Class='BestTSPredParam', TSPred.list = TS.list, VarNames = VarNames)
#' BestTSPred(StQList_Example, BestTSPredParam)
#' }
#' @export
setGeneric("BestTSPred", function(x, BestTSPredParam){standardGeneric("BestTSPred")})
#' @rdname BestTSPred
#'
#' @import TSPred
#'
#' @include BestTSPredParam-class.R
#'
#' @export
setMethod(
  f = "BestTSPred",
  signature = c("vector"),
  function(x, BestTSPredParam){

    Results.List <- list()
    STD <- c()
    for (TSPred in seq(along = BestTSPredParam@TSPred.list)){

      Function <- BestTSPredParam@TSPred.list[[TSPred]][[1L]]
      Param.List <- list()
      Param.List[['x']] <- x
      if (length(BestTSPredParam@TSPred.list[[TSPred]]) >= 2) Param.List <- c(Param.List, BestTSPredParam@TSPred.list[[TSPred]][-1])
      Results.List[[TSPred]] <- do.call(Function, Param.List)
      STD <- c(STD, Results.List[[TSPred]][['STD']])

    }

    if (all(is.na(STD))) return(list(Pred = as.numeric(NA), STD = as.numeric(NA)))
    MinSTD.index <- which.min(STD)

    output <- Results.List[[MinSTD.index]]

    return(output)
  }
)
#' @rdname BestTSPred
#'
#' @import TSPred
#'
#' @include BestTSPredParam-class.R
#'
#' @export
setMethod(
  f = "BestTSPred",
  signature = c("matrix"),
  function(x, BestTSPredParam){

    output <- apply(x, 1, BestTSPred, BestTSPredParam)
    output <- Reduce(rbind, output)
    dimnames(output)[[1]] <- dimnames(x)[[1]]
    return(output)

  }
)
#' @rdname BestTSPred
#'
#' @import data.table StQ TSPred
#'
#' @include BestTSPredParam-class.R
#'
#' @export
setMethod(
  f = "BestTSPred",
  signature = c("StQList"),
  function(x, BestTSPredParam){

    VarNames <- BestTSPredParam@VarNames
    if (length(VarNames) == 0){

      stop('[BestTSPred StQList] El slot VarNames del parámetro BestTSPredParam no puede estar vacío.')
    }

    QualsVal <- strsplit(VarNames, '_')
    QualsVal <- lapply(QualsVal, function(Values){Values[2:length(Values)]})

    OrigVarNames <- VarNames
    VarNames <- ExtractNames(VarNames)
    Data.list <- getData(x, VarNames)
    IDQuals <- unlist(lapply(Data.list, getIDQual))

    DD <- getDD(Data.list[[length(Data.list)]])
    Data.list <- lapply(Data.list, getData)

    slotsNames <- names(getSlots('DD'))
    slotsNames <- slotsNames[slotsNames != 'VarNameCorresp']
    slotsDD <- lapply(slotsNames, function(x){slot(DD,x)})
    DD <- DatadtToDT(Reduce('+', slotsDD))

    keyVar <- vector('list', length(VarNames))

    for (i in 1:length(VarNames)){

      key <- DD[Variable == VarNames[i]]
      Quals <- setdiff(names(key), c('Variable', 'Sort', 'Class', 'Length', 'ValueRegExp'))
      key <- transpose(key[, Quals, with = FALSE])[['V1']]
      key <- key[key != '']
      keyVar[[i]] <- setdiff(key, IDQuals)

    }

    Data.list <- lapply(seq(along = Data.list), function(index){

      Data <- DatadtToDT(Data.list[[index]])
      Data_Var <- lapply(seq(along = VarNames), function(var){

        Quals <- QualsVal[[var]]
        keys <- keyVar[[var]]

        if (length(Quals) == length(keys)){

          for (i in seq(along = keys)){

            col <- keys[i]
            Data <- Data[, aux := Data[[col]] == Quals[i]]
            Data <- Data[aux == TRUE]
            Data[, aux := NULL]
          }

        } else {

          for (i in 1:(length(keys) - 1)){

            col <- keys[i]
            Data <- Data[, aux := Data[[col]] == Quals[i]]
            Data <- Data[aux == TRUE]
            Data[, aux := NULL]
          }

          Data <- Data[, aux := Data[[keys[length(keys)]]] == '']
          Data <- Data[aux == TRUE]
          Data[, aux := NULL]
        }

        return(Data)
      })

      out <- rbindlist(Data_Var, fill = TRUE)
      return(out)
    })

    UnitQuals <- names(getUnits(x[[length(x)]]))
    keyVarTot <- unique(c(UnitQuals, unlist(keyVar)))
    ValidUnits <- Data.list[[length(Data.list)]][, keyVarTot, with = F]
    setkeyv(ValidUnits, keyVarTot)
    ValidUnits <- ValidUnits[!duplicated(ValidUnits)]

    Data.list <- lapply(Data.list, function(Data){

      Data <- Data[, c(keyVarTot, 'IDDD', 'Value'), with = F]
      setkeyv(Data, keyVarTot)
      out <- Data[ValidUnits]
      setkeyv(out, 'IDDD')
      out <- out[VarNames]
      return(out)

    })

    Data.list <- rbindlist(Data.list)
    setkeyv(Data.list, c(keyVarTot, 'IDDD'))
    Data.list[, Value := ifelse(Value == '', NA_real_, Value)]

    output.DT <- Data.list[, lapply(.SD, BestTSPred, BestTSPredParam = BestTSPredParam),
                           .SDcols = 'Value',
                           by = setdiff(names(Data.list), 'Value')]

    output.Pred <- output.DT[seq(1, dim(output.DT)[[1]], by = 2), c(UnitQuals, 'IDDD', 'Value'), with = F]
    formulaPred <- as.formula(paste0(paste0(UnitQuals, collapse = ' + '), ' ~ IDDD'))
    output.Pred <- dcast.data.table(output.Pred, formulaPred, value.var = 'Value')
    setnames(output.Pred, VarNames, OrigVarNames)

    output.STD <- output.DT[seq(2, dim(output.DT)[[1]], by = 2), c(UnitQuals, 'IDDD', 'Value'), with = F]
    formulaSTD <- as.formula(paste0(paste0(UnitQuals, collapse = ' + '), ' ~ IDDD'))
    output.STD <- dcast.data.table(output.STD, formulaSTD, value.var = 'Value')
    setnames(output.STD, VarNames, OrigVarNames)

    output <- list(Pred = output.Pred, STD = output.STD)

    return(output)

  }
)

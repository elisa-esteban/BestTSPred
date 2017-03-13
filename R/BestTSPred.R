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
#' StQList <- readRDS('../E30183.FF.StQList.rds')
#' VarNames <- c('CifraNeg_13.___', 'Personal_07.__1._1._')
#' TS.list <- list(Reg = list('RegDiffTSPred', forward = 2L),
#'                 Stat = list('StatDiffTSPred', forward = 2L),
#'                 StatReg = list('StatRegDiffTSPred', forward = 2L),
#'                 Arima = list('AutoArimaTSPred', forward = 2L))
#' BestTSPredParam <- new(Class='BestTSPredParam', TSPred.list = TS.list, VarNames = VarNames)
#' BestTSPred(StQList, BestTSPredParam)
#' }
#'
#'
#' @include BestTSPredParam-class.R
#'
#' @import data.table StQ TSPred
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
#' @include BestTSPredParam-class.R
#'
#' @export
setMethod(
  f = "BestTSPred",
  signature = c("StQList"),
  function(x, BestTSPredParam){

    VarNames <- BestTSPredParam@VarNames

    if (length(VarNames) == 1){

      DT <- getValues(x, VarNames)
      IDQuals <- setdiff(names(DT), c(VarNames, 'Period'))
      DT[, orderPeriod := RepoTime::orderRepoTime(Period), by = IDQuals]
      setkeyv(DT, c(IDQuals, 'orderPeriod'))
      output <- DT[, BestTSPred(get(VarNames), BestTSPredParam = BestTSPredParam), by = IDQuals]
      setnames(output, c('Pred', 'STD'), paste0(c('Pred', 'STD'), VarNames))
      return(output)

    } else {

      DT.list <- lapply(VarNames, function(Var){

        LocalOutput <- getValues(x, Var)
        setnames(LocalOutput, Var, 'Value')
        LocalOutput[, Variable := Var]
        return(LocalOutput)
      })

      DT <- rbindlist(DT.list)
      IDQuals <- setdiff(names(DT), c('Variable', 'Period', 'Value'))
      DT[, orderPeriod := orderRepoTime(Period), by = IDQuals]
      setkeyv(DT, c(IDQuals, 'Variable', 'orderPeriod'))
      output <- DT[, BestTSPred(Value, BestTSPredParam =  BestTSPredParam), by = c(IDQuals, 'Variable')]
      Form <- paste0(IDQuals, ' ~ Variable')
      output.Pred <- dcast(output, as.formula(Form), value.var = 'Pred')
      setnames(output.Pred, VarNames, paste0('Pred', VarNames))
      output.STD <- dcast(output, as.formula(Form), value.var = 'STD')
      setnames(output.STD, VarNames, paste0('STD', VarNames))
      output <- merge(output.Pred, output.STD, by = IDQuals, all = TRUE)
      return(output)
    }



    for (TSPred in seq(along = BestTSPredParam@TSPred.list)){

      Function <- BestTSPredParam@TSPred.list[[TSPred]][[1L]]
      Param.List <- list()
      Param.List[['x']] <- x
      if (length(BestTSPredParam@TSPred.list[[TSPred]]) >= 2) Param.List <- c(Param.List, BestTSPredParam@TSPred.list[[TSPred]][-1])
      Results.List[[TSPred]] <- do.call(Function, Param.List)
      STD <- c(STD, Results.List[[TSPred]][['STD']])

    }
return('ok')
    VarNames <- BestTSPredParam@VarNames
    if (length(VarNames) == 0){

      stop('[BestTSPred StQList] El slot VarNames del parámetro BestTSPredParam no puede estar vacío.')
    }

    QualsVal <- strsplit(VarNames, '_')
    QualsVal <- lapply(QualsVal, function(Values){Values[2:length(Values)]})

    OrigVarNames <- VarNames
    VarNames <- ExtractNames(VarNames)
    Data.list <- getData(x, VarNames)
return(Data.list)
    IDQuals <- unlist(lapply(Data.list, getIDQual))

    DD <- getDD(Data.list[[length(Data.list)]])
    Data.list <- lapply(Data.list, getData)
return('ok')
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

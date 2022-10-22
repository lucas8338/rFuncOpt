# this file contains the main function and methods will be called by the user

#' @title rFuncOpt
#' @description does a optimization of function parameters through searching.
#' @param defaultFunction a function that takes a lot of arguments ( should to use an elipsis "..." )
#' and return a list or vector containing the metrics you want.
#' @param params contains a list containing the name of parameter and its values.
#' @param a function that takes the params and return a list of combinations. you can to use
#' any the functions from comb.*
#' @param combinationRuleFunction is a function to run for each combination in a list
#' remember 'the combination is a list' so each value of the list will be passed to this
#' function through of a loop' this function should to return the combination if wants to keep
#' or return NULL if the combination is unwanted.
#' it is usefull to remove unwanted combination for for example when a parameter
#' cant run ( is incompatible ) with another one.
#' this will run through foreach.
#' @import dplyr, foreach
#' @export
rFuncOpt<- function(defaultFunction,
                    params,
                    combinationFunction=comb.expandGrid,
                    combinationRuleFunction=NULL,
                    thread.num=parallel::detectCores(),
                    thread.type=ifelse(Sys.info()[['sysname']]=='Windows', 'PSOCK','SOCK')
){
  # declare cores
  cl<- parallel::makeCluster(spec=thread.num,type=thread.type)
  doSNOW::registerDoSNOW(cl)
  on.exit(parallel::stopCluster(cl))

  stopifnot("'defaultFunction' needs to be a function"=is.function(defaultFunction))
  stopifnot("'params' need to be a list"=is.list(params))
  stopifnot("'combinationFunction' need to be a function"=is.function(combinationFunction))

  combinations<- do.call(combinationFunction,list(params))

  # if there a rule function will run the rule function for each combination in parallel.
  if ( !is.null(combinationRuleFunction) ){
    logger::log_info("running combinationRuleFunction for each combination...")
    pg<- libGetDataR::util.generateForeachProgressBar(length(combinations))
    newCombinations<- foreach::foreach( i=1:(length(combinations)),.options.snow=pg ) %dopar% {
      do.call( combinationRuleFunction,list(combinations[[i]]) )
    }
    combinations <- newCombinations[sapply(newCombinations,function(e)!is.null(e))]
  }

  # a function to handle when happen an error during defaultFunction execution.
  runError<- function(e){
    warning(glue::glue(cat("was there an error during the execution of the 'defaultFunction' at iteration=={i}
    with the parameter=={combination}
    the errow was:
    {e}")))

    return('error')
  }

  logger::log_info(glue::glue("will run optimization for {length(combinations)} iterations
  using {thread.num} thread for each execution."))
  pg<- libGetDataR::util.generateForeachProgressBar(length(combinations))
  runs<- foreach::foreach( i=1:(length(combinations)),.options.snow=pg ) %dopar% {
    combination<- combinations[[i]]
    runTime<- system.time( result<- tryCatch( do.call( defaultFunction,list(combination) ),error = runError ))[['elapsed']]
    resultList<- list(iteration=i,parameters=combination,runTime=runTime,result=result)
  }

  class(runs)<- 'rFuncOpt.result'

  runs

}

#' @title rFuncOpt_asDataFrame
#' @description convert the result of rFuncOpt to a data.frame
#' @return a data.frame
#' @export
rFuncOpt_asDataFrame<- function(data){
  stopifnot("class of 'data' is not 'rFuncOpt.result'"=class(data)=='rFuncOpt.result')

  sht.parameters<- data[[1]][['parameters']]

  sht.result<- data[[1]][['result']]

  row.iterations<- 1:(length(data))

  row.runTime<- c()

  for ( i in data ){
    row.runTime<- append(row.runTime,i[['runTime']])
  }

  col.parameters<- names( sht.parameters )

  for ( i in 1:(length(col.parameters)) ){
    col.parameters[[i]]<- glue::glue("parameter.{col.parameters[[i]]}")
  }

  int.nmetrics<- length( sht.result )

  int.nmetricsNullName<- 0

  for ( name in names(sht.result) ){
    if ( is.null(name) || name=="" ){
      int.nmetricsNullName<- int.nmetricsNullName+1
    }
  }

  int.possibleMissingNames<- c()

  if ( int.nmetricsNullName>0 ){
    for ( i in 1:int.nmetricsNullName ){
      int.possibleMissingNames<- append(int.possibleMissingNames,glue::glue("metric{i}"))
    }
  }

  for ( i in 1:(length(names(sht.result))) ){
    name<- names(sht.result)[[i]]
    if ( is.null(name) || name=="" ){
      names(sht.result)[[i]]<- int.possibleMissingNames[[1]]
      int.possibleMissingNames<- int.possibleMissingNames[-1]
    }
  }

  cols<- list('iteration')

  cols<- append(cols,col.parameters)

  cols<- append(cols,'runTime')

  cols<- append(cols,names(sht.result))

  result<- data.frame(matrix(nrow = 0,ncol = length(cols)))

  colnames(result)<- cols

  for ( i in 1:(length(data)) ){
    result[i,'iteration']<- data[[i]][['iteration']]
    for ( cname in colnames(result)[2:(length(colnames(result)))] ){
      #parameters
      if ( stringr::str_starts(cname,'parameter.') ){
        text<- stringr::str_split(cname,'[.]')[[1]]
        text<- capture.output(cat(text[2:(length(text))],sep=""))
        result[i,cname]<- capture.output(cat(as.character(data[[i]][['parameters']][[text]])))
      }
      result[i,'runTime']<- data[[i]][['runTime']]

      startmetrics<- match('runTime',colnames(result))+1
      startmetrics<- colnames(result)[startmetrics:length(colnames(result))]
      newNmetrics<- 1:int.nmetrics
      for ( metric in startmetrics ){
        if ( data[[i]][['result']][[1]]=='error' ){
          result[i,metric]<- 'error'
        }else{
          result[i,metric]<- data[[i]][['result']][[newNmetrics[[1]]]]
          newNmetrics<- newNmetrics[-1]
        }
      }
    }
  }

  result

}
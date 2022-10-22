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

  # run the combination function on 'params'.
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

  # here is the main loop to train the 'defaultFunction' with params.
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
#' @param data takes the return of function 'rFuncOpt'
#' @return a data.frame
#' @export
rFuncOpt_asDataFrame<- function(data){
  stopifnot("class of 'data' is not 'rFuncOpt.result'"=class(data)=='rFuncOpt.result')
  stopifnot("the first execution of data there 'error' the first execution cant have error,
   cause this function use them to generate the colnames of the data.frame"=data[[1]][['result']][[1]]!='error')

  # is a shortcut the parameters value 'sht' mean 'SHortcuT'.
  sht.parameters<- data[[1]][['parameters']]
  # shortcut for result
  sht.result<- data[[1]][['result']]

  # a variable to preprocess the name of parameters columns cause each paramater will takes a
  # prefic 'parameter.'
  col.parameters<- names( sht.parameters )

  # add the prefix to parameters
  for ( i in 1:(length(col.parameters)) ){
    col.parameters[[i]]<- glue::glue("parameter.{col.parameters[[i]]}")
  }

  # store how much metrics there in the data
  int.nmetrics<- length( sht.result )

  # to preprocess metrics that dont have name
  int.nmetricsNullName<- 0

  # count how much variable therent name
  for ( name in names(sht.result) ){
    if ( is.null(name) || name=="" ){
      int.nmetricsNullName<- int.nmetricsNullName+1
    }
  }

  # a empty vector to take the preprocessed name of the metrics that dont have name
  # i'll use the default 'metric{numMetric}' to rename the metric.
  int.possibleMissingNames<- c()

  # do the preprocessing of missing name metrics.
  if ( int.nmetricsNullName>0 ){
    for ( i in 1:int.nmetricsNullName ){
      int.possibleMissingNames<- append(int.possibleMissingNames,glue::glue("metric{i}"))
    }
  }

  # apply the name of metric to the shortcut, so now all metrics in the variable
  # 'sht.result' there names.
  for ( i in 1:(length(names(sht.result))) ){
    name<- names(sht.result)[[i]]
    if ( is.null(name) || name=="" ){
      names(sht.result)[[i]]<- int.possibleMissingNames[[1]]
      int.possibleMissingNames<- int.possibleMissingNames[-1]
    }
  }

  # declare the 'cols' variable this store the name of colnames of the result, this start with the column 'iteration'
  # cause is obvious will ever there this column.
  cols<- list('iteration')

  # add the parameters columns to the 'cols'
  cols<- append(cols,col.parameters)

  # add the 'runTime' column to 'cols'
  cols<- append(cols,'runTime')

  # add the names of metrics to 'cols'
  cols<- append(cols,names(sht.result))

  # set a empty data.frame. The thing abount of using a matrix right there is cause you cant
  # declare columns names after creating the data.frame, cause you got an error about the size of data.frame
  # is zero, so you need to 'say' how much rows and how much cols the data.frame will have, the number of rows
  # is not needed for this the parameter 'nrow=0', but the 'ncol' need to be set.
  result<- data.frame(matrix(nrow = 0,ncol = length(cols)))

  # set the colnames to result
  colnames(result)<- cols

  # the main loop will add the results to the data.frame, this will iterate of each result of rFuncOpt
  # caus is the easiest way.
  for ( i in 1:(length(data)) ){
    result[i,'iteration']<- data[[i]][['iteration']]
    # bellow will set the parameters for the parameters columns
    # cname takes each columns name starting from the position 2 cause the position one is the 'iteration' column
    for ( cname in colnames(result)[2:(length(colnames(result)))] ){
      # only will run if the name of the column there the prefix 'parameter.' indicating is a column for parameter
      if ( stringr::str_starts(cname,'parameter.') ){
        # as str_split uses reges to split by '.' (dot) is needed to use '[]' with dot inside
        # this split will be used to get the data to the column
        text<- stringr::str_split(cname,'[.]')[[1]]
        # the function 'capture.output' is used to capture the output cause the 'cat' function return NULL at end
        text<- capture.output(cat(text[2:(length(text))],sep=""))
        # set the value
        result[i,cname]<- capture.output(cat(as.character(data[[i]][['parameters']][[text]])))
      }
      # set the value of the column 'runTime'
      result[i,'runTime']<- data[[i]][['runTime']]

      # this is interesting, how the metrics cant have names generated by me ( in the case of NULL (error of the
      # defaultFunction ) ) i'll use the 'runTime' column as divisor to know when the metrics starts
      startmetrics<- match('runTime',colnames(result))+1
      startmetrics<- colnames(result)[startmetrics:length(colnames(result))]
      # 'newNmetrics' contains a iteratior from 1:nmetrics cause the metrics will not to be set by the name of
      # variable, but by their index, cause the variable can take dynamic name in case of error (seen before).
      newNmetrics<- 1:int.nmetrics
      for ( metric in startmetrics ){
        # if the name of the result is 'error' so the result there error so this world will be set as value
        # for the metric
        if ( data[[i]][['result']][[1]]=='error' ){
          result[i,metric]<- 'error'
        }else{
          # here will set the metric by an index and will remove the index from the 'newNmetrics', so for example,
          # on the first iteration the index selected is 1, that value (1) will be removed, so the next value
          # will be 2...
          result[i,metric]<- data[[i]][['result']][[newNmetrics[[1]]]]
          newNmetrics<- newNmetrics[-1]
        }
      }
    }
  }

  result

}
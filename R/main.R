# this file contains the main function and methods will be called by the user

#' @title rFuncOpt
#' @description does a optimization of function parameters through searching.
#' @param defaultFunction a function that takes a lot of arguments ( should to use an elipsis "..." )
#' and return the metric you want
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
                    combinationFunction,
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
    result<- tryCatch( do.call( defaultFunction,list(combination) ),error = runError )
    resultList<- list(iteration=i,parameters=combination,result=result)
  }

  runs

}

#' @title rFuncOpt_asDataFrame
#' @description convert the result of rFuncOpt to a data.frame
#' @return a data.frame
#' @export
rFuncOpt_asDataFrame<- function(data){
  iteration<- 1:(length(data))

  parameters
}
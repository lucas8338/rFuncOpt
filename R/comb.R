# this file contains functions to do combinations of params

#' @title comb.expandGrid
#' @description uses the 'expand.grid' function to generate combinations
#' @param data a list with items.
#' @return a list containing the combinations of values of the data.
#' @export
comb.expandGrid<- function(data){
  stopifnot("'data' needs to be a list"=is.list(data))

  comb<- expand.grid(data)

  comb.list<- list()

  for ( rown in 1:(nrow(comb)) ){
    comb.list<- append(comb.list,list(as.list(comb[rown,])))
  }

  comb.list
}
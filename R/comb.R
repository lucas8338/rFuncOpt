# this file contains functions to do combinations of params

#' @title comb.expandGrid
#' @description uses the 'expand.grid' function to generate combinations
#' @param data a list with items.
#' @return a data.frame containing the combinations of values of the data.
#' @export
comb.expandGrid<- function(data){
  stopifnot("'data' needs to be a list"=is.list(data))

  comb<- expand.grid(data)

  comb
}


#' @title comb.randomExpandGrid
#' @description uses the 'expand.grid' then will use the 'sample' function to randomise the order
#' to generate combinations
#' @param data a list with items.
#' @return a data.frame containing the combinations of values of the data.
#' @export
comb.randomExpandGrid<- function(data){
  stopifnot("'data' needs to be a list"=is.list(data))

  comb<- expand.grid(data)

  comb<- comb[sample(1:(nrow(comb))),]

  # fix the rownames
  rownames(comb) <- 1:(nrow(comb))

  comb
}
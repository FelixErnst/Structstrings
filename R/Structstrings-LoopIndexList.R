#' @include Structstrings.R

#' @name LoopIndexList
#' 
#' @title LoopIndexList: base pairing information as a list of integer values
#' 
#' @description 
#' With loop indeces base pairing information can be represented by giving each base
#' pair a number and increasing/decreasing it with each opened/closed base
#' pair. This information can be used for further analysis of the represented 
#' structure.
#' 
#' @param ... the \code{integer} input vectors.
#' 
#' @return a \code{LoopIndexList} object.
#' 
#' @examples 
#' # if the object is create manually make sure it is a valid structure
#' # information. Otherwise an error is thrown.
#' lil <- LoopIndexList(list(c(1L,2L,3L,3L,3L,2L,1L,0L,5L,6L,6L,5L),
#'                        c(1L,2L,2L,2L,2L,2L,1L,0L,5L,6L,6L,5L)))
NULL

#' @rdname LoopIndexList
#' @export
setClass(Class = "LoopIndexList",
         contains = c("CompressedIntegerList"))

#' @rdname LoopIndexList
#' @export
LoopIndexList <- function(...){
  as(IRanges::IntegerList(...),"LoopIndexList")
}

.valid.LoopIndexList <- function(x){
  # all elements must be contained in even numbers?!
  # unique values must be constantly increasing
  # browser()
  NULL
}
S4Vectors:::setValidity2("LoopIndexList",
                         .valid.LoopIndexList)

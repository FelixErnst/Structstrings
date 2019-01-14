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
  y <- IntegerList(
    mapply(function(i,j){
      c(i,j)
    },
    IntegerList(list(0,0,0,0)),
    x,
    SIMPLIFY = FALSE))
  z <- IntegerList(
    mapply(function(i,j){
      c(i,j)
    },
    x,
    IntegerList(list(0,0,0,0)),
    SIMPLIFY = FALSE))
  # First check reveals error faster, but is leaky
  message <- "Unmatched positions."
  z <- y - z
  if(sum(sum(z)) != 0L){
    return(message)
  } else {
    if(sum(sum(unique(z))) != 0L){
      return(message)
    }
  }
  NULL
}

S4Vectors:::setValidity2("LoopIndexList",
                         .valid.LoopIndexList)

#' @name LoopIndexList
#' @export
setAs("IntegerList", "LoopIndexList",
      function(from) {
        from <- IRanges::CompressedIntegerList(from)
        class(from) <- "LoopIndexList"
        validObject(from)
        from
      })
#' @name LoopIndexList
#' @export
setAs("CompressedIntegerList", "LoopIndexList",
      function(from) {
        class(from) <- "LoopIndexList"
        validObject(from)
        from
      })
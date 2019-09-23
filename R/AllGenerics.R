#' @include Structstrings.R
NULL

#' @rdname getBasePairing
#' @export
setGeneric(name = "getBasePairing",
           def = function(x, compress = TRUE, return.sequence = FALSE)
             standardGeneric("getBasePairing"))
#' @rdname getBasePairing
#' @export
setGeneric(name = "getDotBracket",
           def = function(x, force = FALSE) standardGeneric("getDotBracket"))
#' @rdname getBasePairing
#' @export
setGeneric(name = "getLoopIndices",
           def = function(x, bracket.type, warn.type.drops = TRUE)
             standardGeneric("getLoopIndices"))
#' @rdname convertAnnotation
#' @export
setGeneric(name = "convertAnnotation",
           def = function(x, from, to) standardGeneric("convertAnnotation"))

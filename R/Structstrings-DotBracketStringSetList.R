#' @include Structstrings.R
NULL

#' @rdname DotBracketString
#' @export
setClass("DotBracketStringSetList",
         contains="BStringSetList",
         representation(
           unlistData="DotBracketStringSet"
         ),
         prototype(
           elementType="DotBracketStringSet"
         )
)

setMethod("relistToClass", "DotBracketStringSet",
          function(x) "DotBracketStringSetList"
)

#' @rdname DotBracketString
#' @export
DotBracketStringSetList <- function(..., use.names = TRUE)
{
  .XStringSetList("DotBracket", ..., use.names = use.names)
}
  
#' @rdname DotBracketString
#' @export
DBSL <- function(..., use.names = TRUE) DotBracketStringSetList(...,use.names)

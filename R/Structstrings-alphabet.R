#' @include Structstrings.R
NULL

#' @rdname DotBracketString
#' @export
setMethod("alphabet", "DotBracketString",
          function(x){
            DOTBRACKET_ALPHABET
          })
#' @rdname DotBracketString
#' @export
setMethod("encoding", "DotBracketString",
          function(x){
            ans <- DOTBRACKET_CHAR_VALUES
            names(DOTBRACKET_ALPHABET)
            ans
          })
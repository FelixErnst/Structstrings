#' @include Structstrings.R
NULL

#' @name DotBracketString
#' @aliases DotBracketString DotBracketStringSet DotBracketStringSetList
#' 
#' @title The DotBracketString, DotBracketStringSet and DotBracketStringSetList
#' classes
#' 
#' @description 
#' The \code{DotBracketString} extends the 
#' \code{\link[Biostrings:XStringSet-class]{BString}} class. The 
#' \code{DotBracketStringSet} and \code{DotBracketStringSetList} classes are 
#' implemented accordingly.
#' 
#' The alphabet consists of the letters
#' \code{"(",")",".","<",">","[","]","\{","\}"}, which describes base pairing
#' between positions. The \code{.} letter describes an unpaired position. The
#' number of opening and closing letters need to be equal within a
#' \code{DotBracketString} to be a valid dot bracket annotation. This is checked
#' upon creation and modificiation of the object. 
#' 
#' The objects can also be created using the shorter function names \code{DB},
#' \code{DBS} and \code{DBSL}.
#' 
#' Currently, there is no distinction in base pairing strength between the
#' different bracket types.
#' 
#' @param x \code{DotBracketString},\code{DotBracketStringSet}: the input, which
#' is tried to be convert into a \code{DotBracketString*}.
#' @param start \code{DotBracketString}: starting position for creating the 
#' object from the character input.
#' @param nchar \code{DotBracketString}: number of letters are read from the 
#' input character
#' @param ... \code{DotBracketStringSetList}: the input, which converted into a
#' list. Each element is tried to be converted into a 
#' \code{DotBracketStringSet}.
#' @param use.names \code{DotBracketStringSetList}: Should names of the input be
#' preserved.
#' 
#' @return a \code{DotBracketString*} object.
#' 
#' @examples 
#' str <- "((.))..[[..]]...{{..}}..<<..>>"
#' db <- DotBracketString(str)
#' dbs <- DotBracketStringSet(c("structure1" = str, "structure2" = str))
#' dbsl <- DotBracketStringSetList(list(first = dbs, second = dbs))
NULL

#' @rdname DotBracketString
#' @export
setClass("DotBracketString", contains = "BString")

setMethod("seqtype", "DotBracketString", function(x) "DotBracket")

# modified version, since the results might be invalid
setMethod("windows", "DotBracketString",
          function(x, start = NA, end = NA, width = NA){
            x <- callNextMethod()
            .subsetting_validity_check(as(x,"DotBracketStringSet"))
            x
          }
)
setReplaceMethod("subseq", "DotBracketString",
                 function(x, start = NA, end = NA, width = NA, value){
                   x <- callNextMethod()
                   .subsetting_validity_check(as(x,"DotBracketStringSet"))
                   x
                 }
)

#' @rdname DotBracketString
#' @export
DotBracketString <- function(x = character(), start = 1, nchar = NA)
{
  as(BString(x, start, nchar), "DotBracketString")
}
#' @rdname DotBracketString
#' @export
DB <- function(x = character(), start = 1, nchar = NA)
{
  DotBracketString(x = x, start = start, nchar = nchar)
}

.valid.DotBracketString <- function(x)
{
  return(.valid.DotBracketStringSet(list(x)))
}

S4Vectors:::setValidity2("DotBracketString", .valid.DotBracketString)

# constructor ------------------------------------------------------------------

.check_for_invalid_db_letters <- function(string, alphabet)
{
  if(length(string) > 0L){
    if(is.list(string)){
      string <- unlist(string)
    }
    letters_in_string <- unique(unlist(strsplit(string, "")))
    if(any(!(letters_in_string %in% alphabet))){
      message(paste(
        letters_in_string[!(letters_in_string %in% alphabet)],
        collapse = ""))
      stop("Invalid character(s) - see above",
           call. = FALSE)
    }
  }
}
.check_for_invalid_db_values <- function(values, alphabet_values)
{
  if(any(!(values %in% alphabet_values))){
    print(paste(
      values[!(values %in% alphabet_values)],
      collapse = ""))
    stop("Invalid values(s) - see above",
         call. = FALSE)
  }
}

.BStringToDotBracketString <- function(from)
{
  string <- .norm_letters(as.character(from))
  .check_for_invalid_db_letters(string,DOTBRACKET_ALPHABET)
  from <- BString(string)
  class(from) <- "DotBracketString"
  validObject(from)
  from
}
.integerToDotBracketString <- function(from)
{
  if (length(from) == 0){
    ans <- DotBracketString()
  } else {
    .check_for_invalid_db_values(from, DOTBRACKET_CHAR_VALUES)
    ans <- DotBracketString(rawToChar(as.raw(from)))
  }
  validObject(ans)
  ans
}

#' @name DotBracketString
#' @export
setAs("character", "DotBracketString",
      function(from) DotBracketString(from))
#' @name DotBracketString
#' @export
setAs("BString", "DotBracketString",
      function(from) .BStringToDotBracketString(from))
#' @name DotBracketString
#' @export
setAs("integer", "DotBracketString",
      function(from) .integerToDotBracketString(from))

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
#' \code{(}, \code{)}, \code{.}, \code{<}, \code{>}, \code{[}, \code{]},
#' \code{\{} and \code{\}}, which describes base pairing between positions. The
#' \code{.} letter describes an unpaired position. The number of opening and
#' closing letters need to be equal within a \code{DotBracketString} to be a
#' valid dot bracket annotation. This is checked upon creation and modificiation
#' of the object.
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
setMethod("subseq", "DotBracketString",
          function(x, start = NA, end = NA, width = NA){
            x <- callNextMethod()
            validObject(x)
            x
          }
)
setReplaceMethod("subseq", "DotBracketString",
                 function(x, start = NA, end = NA, width = NA, value){
                   x <- callNextMethod()
                   validObject(x)
                   x
                 }
)

#' @rdname DotBracketString
#' @export
DotBracketString <- function(x = "", start = 1, nchar = NA)
{
  as(BString(x, start = start, nchar = nchar), "DotBracketString")
}

#' @rdname DotBracketString
#' @export
DB <- function(x = character(), start = 1, nchar = NA)
{
  DotBracketString(x = x, start = start, nchar = nchar)
}

.valid.DotBracketString <- function(object)
{
  return(.valid.DotBracketStringSet(list(object)))
}

setValidity("DotBracketString", .valid.DotBracketString)


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


# Show 

### Placeholder, initialized in .onLoad()
DOTBRACKET_COLORED_LETTERS <- NULL

### Called in .onLoad() to initialize DOTBRACKET_COLORED_LETTERS
#' @importFrom crayon make_style inverse
#' @importFrom grDevices rgb
make_DOTBRACKET_COLORED_LETTERS <- function()
{
  # base colours
  ans <- c()
  ans["("] <- make_style("greenyellow", bg=TRUE)(make_style("black")("("))
  ans["["] <- make_style("green2", bg=TRUE)(make_style("black")("["))
  ans["<"] <- make_style("green3", bg=TRUE)(make_style("black")("<"))
  ans["{"] <- make_style("limegreen", bg=TRUE)(make_style("black")("{"))
  ans["."] <- make_style(rgb(0.2,0.2,0.2), bg=TRUE)(make_style("white")("."))
  ans[")"] <- make_style("orangered", bg=TRUE)(make_style("black")(")"))
  ans["]"] <- make_style("red2", bg=TRUE)(make_style("black")("]"))
  ans[">"] <- make_style("red3", bg=TRUE)(make_style("black")(">"))
  ans["}"] <- make_style("red4", bg=TRUE)(make_style("black")("}"))
  ans
}

.add_DotBracket_colors <- function(x)
{
  ans <- vapply(x,
                function(xi){
                  xi <- strsplit(xi,"")[[1L]]
                  m <- match(xi, names(DOTBRACKET_COLORED_LETTERS))
                  match_idx <- which(!is.na(m))
                  xi[match_idx] <- DOTBRACKET_COLORED_LETTERS[m[match_idx]]
                  paste0(xi, collapse="")
                },
                character(1),
                USE.NAMES=FALSE
  )
  x_names <- names(x)
  if (!is.null(x_names))
    names(ans) <- x_names
  ans
}

add_colors <- function(x) UseMethod("add_colors")
add_colors.default <- identity
add_colors.DotBracket <- .add_DotBracket_colors

setMethod("show", "DotBracketString",
          function(object)
          {
            object_len <- object@length
            cat(object_len, "-letter ", class(object), " object\n", sep="")
            snippet <- .toSeqSnippet(object, getOption("width") - 5L)
            cat("seq: ", add_colors(snippet), "\n", sep="")
          }
)
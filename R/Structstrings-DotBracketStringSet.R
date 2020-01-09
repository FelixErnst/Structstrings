#' @include Structstrings.R
#' @include Structstrings-DotBracketString.R
NULL

#' @rdname DotBracketString
#' @importFrom XVector xvcopy
#' @export
setClass("DotBracketStringSet",
         contains = "BStringSet",
         representation(),
         prototype(elementType = "DotBracketString"))

setReplaceMethod(
  "seqtype", "DotBracketStringSet",
  function(x, value)
  {
    ans_class <- paste(value, "StringSet", sep = "")
    lkup <- get_seqtype_conversion_lookup(seqtype(x), value)
    if (!is.null(lkup)){
      x <- XVector::xvcopy(x, lkup = lkup)
    }
    new2(ans_class,
         pool = x@pool,
         ranges = x@ranges,
         check = FALSE)
  }
)

# modified version, since the results might be invalid

.subsetting_validity_check <- function(x)
{
  validMatches <- .check_matched_postions(x)
  if(!is.null(validMatches) &&
     is.list(validMatches) &&
     any(lengths(validMatches) > 0L)){
    stop("Subsetting produced invalid structure information.",
         call. = FALSE)
  }
}

setMethod(
  "windows", "DotBracketStringSet",
  function(x, start = NA, end = NA, width = NA)
  {
    x <- callNextMethod()
    .subsetting_validity_check(x)
    x
  }
)

setMethod(
  "threebands", "DotBracketStringSet",
  function(x, start = NA, end = NA, width = NA)
  {
    x <- callNextMethod()
    lapply(x, .subsetting_validity_check)
    x
  }
)

setReplaceMethod(
  "subseq", "DotBracketStringSet",
  function(x, start = NA, end = NA, width = NA, value)
  {
    x <- callNextMethod()
    .subsetting_validity_check(x)
    x
  }
)

# constructor and coercion -----------------------------------------------------

.BStringToDotBracketStringSet <- function(from)
{
  from <- .norm_letters(as.character(from))
  .check_for_invalid_db_letters(from, DOTBRACKET_ALPHABET)
  ans <- as(BStringSet(from), "DotBracketStringSet")
  validObject(ans)
  ans
}
.BStringSetToDotBracketStringSet <- function(from)
{
  from <- .norm_letters(as.character(from))
  .check_for_invalid_db_letters(from, DOTBRACKET_ALPHABET)
  from <- BStringSet(from)
  ans <- new2("DotBracketStringSet",
              pool = from@pool,
              ranges = from@ranges,
              check = FALSE)
  names(ans) <- names(from)
  validObject(ans)
  ans
}
.integerToDotBracketStringSet <- function(from)
{
  if (length(from) == 0){
    ans <- DotBracketStringSet()
  } else {
    .check_for_invalid_db_values(from, DOTBRACKET_CHAR_VALUES)
    ans <- DotBracketStringSet(rawToChar(as.raw(from)))
  }
  ans
}
.IntegerListToDotBracketStringSet <- function(from)
{
  ans <- unlist(from, use.names = FALSE)
  .check_for_invalid_db_values(ans, DOTBRACKET_CHAR_VALUES)
  as(relist(as(ans, "DotBracketStringSet")[[1L]], from),
     "DotBracketStringSet")
}
.DotBracketStringSetToIntegerList <- function(from)
{
  if (length(from) == 0){
    ans <- integer(0)
  } else {
    ans <- lapply(lapply(as.character(from),charToRaw),as.integer)
  }
  as(ans,"CompressedIntegerList")
}

#' @name DotBracketString
#' @export
setAs("character", "DotBracketStringSet",
      function(from) .BStringToDotBracketStringSet(as(from,"BStringSet")))
#' @name DotBracketString
#' @export
setAs("list", "DotBracketStringSet",
      function(from) .BStringSetToDotBracketStringSet(as(from,"BStringSet")))
# the worst case thing, but need for many internal Biostrings functions
#' @name DotBracketString
#' @export
setAs("ANY", "DotBracketStringSet",
      function(from) .BStringToDotBracketStringSet(as(from,"BStringSet")))
#' @name DotBracketString
#' @export
setAs("BString", "DotBracketStringSet",
      function(from) .BStringToDotBracketStringSet(from))
#' @name DotBracketString
#' @export
setAs("BStringSet", "DotBracketStringSet",
      function(from) .BStringSetToDotBracketStringSet(from))
#' @name DotBracketString
#' @export
setAs("integer", "DotBracketStringSet",
      function(from) .integerToDotBracketStringSet(from))
#' @name DotBracketString
#' @export
setAs("IntegerList", "DotBracketStringSet",
      function(from) .IntegerListToDotBracketStringSet(from))
#' @name DotBracketString
#' @export
setAs("DotBracketStringSet", "IntegerList",
      function(from) .DotBracketStringSetToIntegerList(from))

#' @rdname DotBracketString
#' @export
DotBracketStringSet <- function(x = character()) as(x, "DotBracketStringSet")
#' @rdname DotBracketString
#' @export
DBS <- function(x = character()) as(x, "DotBracketStringSet")

setAs("ANY", "DotBracketStringSet", function(from) DotBracketStringSet(from))

# DotBracketStringSet validity -------------------------------------------------

.get_db_types <- function(x)
{
  chr <- unique(unlist(strsplit(as.character(x),"")))
  f1 <- which(gsub("\\\\","",STRUCTURE_OPEN_CHR) %in% chr)
  f2 <- which(gsub("\\\\","",STRUCTURE_CLOSE_CHR) %in% chr)
  if(any(f1 != f2)){
    stop("Invalid input. Soemthing went wrong.")
  }
  return(as.integer(f1))
}

#' @importFrom stringr str_locate
.needs_letters_normalized <- function(x)
{
  f <- stringr::str_locate(x, ">")[,"start"] < 
    stringr::str_locate(x, "<")[,"start"]
  ff <- vapply(f,is.na,logical(1))
  f[ff] <- FALSE
  if(!any(f)) {
    return(FALSE)
  }
  ans <- TRUE
  attr(ans,"vector") <- f
  ans
}
.norm_letters <- function(x)
{
  if(!is.character(x)){
    x <- as.character(x)
  }
  # special case for <>. This is the usually used orientation (eg. by ViennaRNA)
  # , but tRNAscan files use a different orientation. If the first occurance is 
  # < switch out the orientation.
  f <- .needs_letters_normalized(x)
  if(!f){
    return(x)
  }
  f <- attr(f,"vector")
  tmp <- gsub("<","a",x[f])
  tmp <- gsub(">","b",tmp)
  tmp <- gsub("a",">",tmp)
  tmp <- gsub("b","<",tmp)
  x[f] <- tmp
  x
}

#' @importFrom stringr str_locate_all
.pos_letters <- function(x,chrs)
{
  lapply(chrs,
         function(chr){
           stringr::str_locate_all(x, chr)
         })
}

.check_matched_postions <- function(x)
{
  x <- lapply(x,.norm_letters)
  open <- .pos_letters(x,STRUCTURE_OPEN_CHR)
  close <- .pos_letters(x,STRUCTURE_CLOSE_CHR)
  lengthOpen <- lapply(open,function(z){lengths(z)})
  lengthClose <- lapply(close,function(z){lengths(z)})
  lengthMatch <- lapply(
    seq_along(lengthOpen),
    function(i){
      which(unlist(lengthOpen[[i]]) != unlist(lengthClose[[i]]))
    })
  # check for unmatched positions
  if(any(unlist(lengths(lengthMatch)) != 0)){
    return(lengthMatch)
  }
  NULL
}
.valid.DotBracketStringSet <- function(object)
{
  validMatches <- .check_matched_postions(object)
  if(!is.null(validMatches) &&
     is.list(validMatches) &&
     any(lengths(validMatches) > 0L)){
    return(paste0("\nFollowing structures are invalid:\n'",
                  paste(unique(unlist(validMatches)),
                        collapse = "', '"),
                  "'.\nThey contain unmatched positions."))
  }
  NULL
}

setValidity("DotBracketStringSet",.valid.DotBracketStringSet)

# Show 

.DotBracketStringSet.show_frame_line <- function(x, i, iW, widthW)
{
  width <- nchar(x)[i]
  snippet_width <- getOption("width") - 2L - iW - widthW
  if (!is.null(names(x)))
    snippet_width <- snippet_width - .namesW - 1L
  snippet <- .toSeqSnippet(x[[i]], snippet_width)
  if (!is.null(names(x))) {
    snippet_class <- class(snippet)
    snippet <- format(snippet, width=snippet_width)
    class(snippet) <- snippet_class
  }
  cat(format(paste("[", i,"]", sep=""), width=iW, justify="right"), " ",
      format(width, width=widthW, justify="right"), " ",
      add_colors(snippet),
      sep="")
  if (!is.null(names(x))) {
    snippet_name <- names(x)[i]
    if (is.na(snippet_name))
      snippet_name <- "<NA>"
    else if (nchar(snippet_name) > .namesW)
      snippet_name <- paste0(substr(snippet_name, 1L, .namesW - 1L),
                             .compact_ellipsis)
    cat(" ", snippet_name, sep="")
  }
  cat("\n")
}

### 'half_nrow' must be >= 1
.DotBracketStringSet.show_frame <- function(x, half_nrow=5L)
{
  if (is.null(head_nrow <- getOption("showHeadLines")))
    head_nrow <- half_nrow 
  if (is.null(tail_nrow <- getOption("showTailLines")))
    tail_nrow <- half_nrow
  
  lx <- length(x)
  iW <- nchar(as.character(lx)) + 2 # 2 for the brackets
  ncharMax <- max(nchar(x))
  widthW <- max(nchar(ncharMax), nchar("width"))
  Biostrings:::.XStringSet.show_frame_header(iW, widthW, !is.null(names(x)))
  if (lx < (2*half_nrow+1L) | (lx < (head_nrow+tail_nrow+1L))) {
    for (i in seq_len(lx))
      .DotBracketStringSet.show_frame_line(x, i, iW, widthW)
  } else {
    if (head_nrow > 0)
      for (i in 1:head_nrow)
        .DotBracketStringSet.show_frame_line(x, i, iW, widthW)
    cat(format("...", width=iW, justify="right"),
        format("...", width=widthW, justify="right"),
        "...\n")
    if (tail_nrow > 0)
      for (i in (lx-tail_nrow+1L):lx)
        .DotBracketStringSet.show_frame_line(x, i, iW, widthW)
  }
}

setMethod("show", "DotBracketStringSet",
          function(object)
          {
            object_len <- length(object)
            cat(class(object), " object of length ", length(object), sep="")
            if (object_len != 0L)
              cat(":")
            cat("\n")
            if (object_len != 0L)
              .DotBracketStringSet.show_frame(object)
          }
)
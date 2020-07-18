#' @include Structstrings.R
#' @include Structstrings-DotBracketDataFrame.R
#' @include Structstrings-DotBracketStringSet.R
#' @include Structstrings-LoopIndexList.R
NULL

#' @name getBasePairing
#' @aliases getBasePairing getLoopIndices
#'
#' @title Accessing Dot Bracket annotation
#'
#' @description
#' \code{getBasePairing} converts a dot bracket annotation from a 
#' \code{\link{DotBracketString}} into a base pair table as 
#' \code{\link{DotBracketDataFrame}}. Base pairing is indicated by corresponding
#' numbers in the forward and reverse columns.
#'
#' \code{getDotBracket} converts the dot bracket annotation from a
#' \code{\link{DotBracketDataFrame}} into a \code{\link{DotBracketString}}. If
#' the \code{character} colums is populated, the information from this column
#' will be used. If this is not desired set \code{force = TRUE}. However ,
#' beaware that this will result in a dot bracket annotation, which does not
#' necessarilly  matches the original dot bracket string it may have been
#' created from. It is rather the dot bracket string with the lowest number of
#' different loops and it will use the different dot bracket annotations one
#' after another. Example: "(((<<<>>>)))" will be returned as
#' \code{(((((())))))}. \code{(((<<<)))>>>} will be returned as
#' \code{(((<<<)))>>>}, \code{((([[[)))]]]} will be eturned as
#' \code{(((<<<)))>>>}.
#' 
#' \code{getLoopIndices} converts the dot bracket annotation from a
#' \code{\link{DotBracketString}} or \code{DotBracketDataFrame} into a
#' \code{\link{LoopIndexList}}.
#'
#' @return
#' \code{getBasePairing}: 
#' The result is a \code{\link{DotBracketDataFrame}} with following columns:
#' pos, forward, reverse, character (and optionally the base column). If a
#' position is unpaired, forward and reverse will be \code{0}, otherwise it will
#' match the base paired positions.
#' 
#' \code{getLoopIndices}: returns a \code{\link{LoopIndexList}}.
#'
#' @param x a \code{\link{DotBracketString}} or
#'   \code{\link{DotBracketStringSet}} object
#' @param compress \code{getBasePairing}: whether to return a
#'   \code{CompressedSplitDotBracketDataFrameList} or a
#'   \code{SimpleSplitDotBracketDataFrameList}
#' @param force \code{getDotBracket}: Should the dot bracket string be
#'   generated from the base pairing, if the \code{character} column is present?
#' @param bracket.type \code{getLoopIndices}: Which dot bracket annotation type
#'   should be converted into loop indices? Only usable, if more than one is
#'   present. (\code{1L = '()', 2L = '<>', 3L = '[]', 4L = '{}'})
#' @param warn.type.drops \code{getLoopIndices}: \code{TRUE}(default) or
#'   \code{FALSE}: Warn if more than one dot bracket annotation type is present
#'   in the input?
#' @param return.sequence if the input is a \code{StructuredXStringSet}:
#'   \code{TRUE}(default) or \code{FALSE}: Whether the sequence should be
#'   returned in the \code{base} column.
#' 
#' @examples 
#' data("dbs", package = "Structstrings")
#' # conversion
#' dbdf <- getBasePairing(dbs)
#' # ... and the round trip
#' dbs <- getDotBracket(dbdf)
#' 
#' # loop indices per bracket type
#' loopids <- getLoopIndices(dbs)
#' # choose the bracket type manually, if necessary
#' loopids <- getLoopIndices(dbs, bracket.type = 1L)
#' # do not show warning if mulitple bracket types are present
#' loopids <- getLoopIndices(dbs, bracket.type = 1L, warn.type.drops = FALSE)
NULL

# converts the character column from integer to character
# if it is not the empty character the closing character is added as well
.convert_char_type_to_character <- function(x)
{
  x$character <- strsplit(rawToChar(as.raw(as.integer(x$character))),"")[[1]]
  x
}

# convert dot bracket annotation in ct like format
.get_pairing <- function(x)
{
  # XString* classes cannot be converted to strings in C function
  # since functions are not available from Biostrings package externally
  chr <- lapply(x,as.character)
  partitioning <- IRanges::PartitioningByEnd(cumsum(width(x)))
  ans <- .Call2("new_DotBracketDFrameList_from_CHARACTER",
                paste0(chr,collapse = ""),
                partitioning,
                PACKAGE = "Structstrings")
  ans@unlistData <- .convert_char_type_to_character(ans@unlistData)
  validObject(ans)
  names(ans) <- names(x)
  return(ans)
}

setAs("DotBracketString","DotBracketDataFrame",
      function(from){
        .get_pairing(DotBracketStringSet(from))[[1L]]
      })
setAs("DotBracketStringSet","DotBracketDataFrameList",
      function(from){
        as(.get_pairing(from),"DotBracketDataFrameList")
      })
setAs("DotBracketStringSet","SimpleSplitDotBracketDataFrameList",
      function(from){
        as(.get_pairing(from),"SimpleSplitDotBracketDataFrameList")
      })
setAs("DotBracketStringSet","CompressedSplitDotBracketDataFrameList",
      function(from){
        .get_pairing(from)
      })

#' @rdname getBasePairing
#' @export
setMethod("getBasePairing",
          signature = "DotBracketString",
          definition = function(x){
            as(x,"DotBracketDataFrame")
          })
#' @rdname getBasePairing
#' @export
setMethod("getBasePairing",
          signature = "DotBracketStringSet",
          definition = function(x, compress = TRUE){
            if(compress){
              return(as(x,"CompressedSplitDotBracketDataFrameList"))
            }
            as(x,"SimpleSplitDotBracketDataFrameList")
          })

# DotBracketDataFrame conversion to DotBracketStringSet ------------------------

.get_dot_bracket <- function(x, force = FALSE)
{
  if(length(colnames(x[[1]])) >= 4L & !force){
    chr <- lapply(x,"[[","character")
    chr <- vapply(chr,paste,character(1),collapse = "")
    ans <- DotBracketStringSet(chr)
  } else {
    int <- lapply(x,"[[","reverse")
    ans <- .Call2("new_DotBracket_from_INTEGER",
                  int,
                  PACKAGE = "Structstrings")
    ans <- DotBracketStringSet(ans)
    names(ans) <- names(x)
  }
  validObject(ans)
  return(ans)
}

setAs("DotBracketDataFrame","DotBracketString",
      function(from){
        .get_dot_bracket(DotBracketDataFrameList(from))[[1L]]
      })
setAs("DotBracketDataFrameList","DotBracketStringSet",
      function(from){
        .get_dot_bracket(from)
      })
setAs("SimpleSplitDotBracketDataFrameList","DotBracketStringSet",
      function(from){
        .get_dot_bracket(from)
      })
setAs("CompressedSplitDotBracketDataFrameList","DotBracketStringSet",
      function(from){
        .get_dot_bracket(from)
      })

#' @rdname getBasePairing
#' @export
setMethod("getDotBracket",
          signature = "DotBracketDataFrame",
          definition = function(x, force = FALSE){
            .get_dot_bracket(DotBracketDataFrameList(x), 
                             force)[[1L]]
          })
#' @rdname getBasePairing
#' @export
setMethod("getDotBracket",
          signature = "DotBracketDataFrameList",
          definition = function(x, force = FALSE){
            .get_dot_bracket(x, force)
          })
#' @rdname getBasePairing
#' @export
setMethod("getDotBracket",
          signature = "SimpleSplitDotBracketDataFrameList",
          definition = function(x, force = FALSE){
            .get_dot_bracket(x, force)
          })
#' @rdname getBasePairing
#' @export
setMethod("getDotBracket",
          signature = "CompressedSplitDotBracketDataFrameList",
          definition = function(x, force = FALSE){
            .get_dot_bracket(x, force)
          })

# DotBracketDataFrame/DotBracketStringSet conversion to LoopIndexList ----------

.norm_bracket_type <- function(input, type)
{
  if(is(input,"DotBracketStringSet")){
    types <- unique(unlist(strsplit(as.character(input),"")))
  } else if(is(input,"CompressedSplitDotBracketDataFrameList")) {
    if(ncol(input[[1]]) >= 4L){
      types <- unique(input@unlistData$character)
    } else {
      types <- "("
    }
  } else {
    stop(".")
  }
  str_open_chr <- gsub("\\\\","",STRUCTURE_OPEN_CHR)
  types <- types[types != STRUCTURE_NEUTRAL_CHR]
  types_open <- which(str_open_chr %in% types)
  if(missing(type)){
    return(types_open[1L])
  }
  message <- paste0("Unknown bracket type. Only '",
                    paste(seq_along(str_open_chr), collapse = "','"),
                    "'are valid.")
  if(is.list(type)){
    stop(message,
         call. = FALSE)
  }
  type <- type[1L]
  if(is.character(type)) {
    if(!(type %in% str_open_chr)){
      stop(message,
           call. = FALSE)
    }
    type <- as.integer(which(str_open_chr == type))
  }
  if(!is.integer(type)) {
    type <- as.integer(type)
  }
  if(!(type %in% c(1,2,3,4))){
    stop(message,
         call. = FALSE)
  }
  if(!(type %in% types_open)){
    type <- types_open[1L]
    warning("Selected bracket type is not present in input. Reverting to ",
            "first type found.",
            call. = FALSE)
  }
  type
}

.check_for_dbtype_string <- function(x, type)
{
  non_dbtype <- 
    STRUCTURE_OPEN_CHR[!(STRUCTURE_OPEN_CHR %in% STRUCTURE_OPEN_CHR[type])]
  if(any(stringr::str_detect(as.character(x),non_dbtype))){
    return(FALSE)
  }
  TRUE
}

.check_for_dbtype_dbdf <- function(x, type)
{
  non_dbtype <- 
    paste0(
    STRUCTURE_OPEN_CHR[!(STRUCTURE_OPEN_CHR %in% STRUCTURE_OPEN_CHR[type])],
    STRUCTURE_CLOSE_CHR[!(STRUCTURE_CLOSE_CHR %in% STRUCTURE_CLOSE_CHR[type])])
  non_dbtype <- gsub("\\\\","",non_dbtype)
  if(any(x$character %in% non_dbtype)){
    return(FALSE)
  }
  TRUE
}
.norm_dbdf_bracket_type <- function(x, type, warn = TRUE)
{
  if(.check_for_dbtype_dbdf(x,type)){
    return(x)
  }
  if(warn){
    warning("DotBracketStringSet contains more than one dot bracket type.\n",
            "loop indices can only by calculated for one dot ",
            "bracket type at a time.\nRe-run 'getLoopIndices' with different ",
            "type selected to access the loop indices for other dot bracket ",
            "types.",
            call. = FALSE)
  }
  dbtype <- c(STRUCTURE_OPEN_CHR[type],STRUCTURE_CLOSE_CHR[type])
  dbtype <- gsub("\\\\","",dbtype)
  x[!(x$character %in% dbtype),"forward"] <- 0L
  x[!(x$character %in% dbtype),"reverse"] <- 0L
  x[!(x$character %in% dbtype),"character"] <- "."
  x
}

.get_idx_of_loops_from_dbs <- function(dbs, type, warn.type.drops)
{
  type <- .norm_bracket_type(dbs,type)
  if(!.is_a_bool(warn.type.drops)){
    stop("'warn.type.drops' must TRUE or FALSE.")
  }
  # XString* classes cannot be converted to strings in C function
  # since functions are not available from Biostrings package externally
  x <- lapply(dbs,as.character)
  if(warn.type.drops){
    test <- lapply(x, .check_for_dbtype_string, type)
    if(!all(unlist(test))){
      warning("DotBracketStringSet contains more than one dot bracket type.\n",
              "loop indices can only by calculated for one dot ",
              "bracket type at a time.\nRe-run 'getLoopIndices' with different",
              " type selected to access the loop indices for other dot bracket",
              " types.",
              call. = FALSE)
    }
  }
  # selection of loop types is implemented in C function.
  .Call2("new_LoopIndexList_from_CHARACTER_LIST",
         x,
         type,
         PACKAGE = "Structstrings")
}

.get_idx_of_loops_from_dbdfl <- function(dbdfl, type, warn.type.drops)
{
  type <- .norm_bracket_type(dbdfl,type)
  if(!.is_a_bool(warn.type.drops)){
    stop("'warn.type.drops' must TRUE or FALSE.")
  }
  if(is(dbdfl,"CompressedDataFrameList")){
    dbdfl@unlistData <- .norm_dbdf_bracket_type(dbdfl@unlistData, type)
  } else {
    dbdfl <- lapply(dbdfl, .norm_dbdf_bracket_type, type, warn.type.drops)
  }
  x <- lapply(dbdfl,"[[","reverse")
  .Call2("new_LoopIndexList_from_INTEGER_LIST",
         x,
         PACKAGE = "Structstrings")
}

#' @rdname getBasePairing
#' @export
setMethod("getLoopIndices",
          signature = "DotBracketString",
          definition = function(x, bracket.type, warn.type.drops = TRUE){
            .get_idx_of_loops_from_dbs(DotBracketStringSet(x),
                                       bracket.type, warn.type.drops)[[1L]]
          })

#' @rdname getBasePairing
#' @export
setMethod("getLoopIndices",
          signature = "DotBracketStringSet",
          definition = function(x, bracket.type, warn.type.drops = TRUE){
            .get_idx_of_loops_from_dbs(x, bracket.type, warn.type.drops)
          })

#' @rdname getBasePairing
#' @export
setMethod("getLoopIndices",
          signature = "DotBracketDataFrame",
          definition = function(x, bracket.type, warn.type.drops = TRUE){
            .get_idx_of_loops_from_dbdfl(SDBDFL(x), bracket.type,
                                         warn.type.drops)[[1L]]
          })

#' @rdname getBasePairing
#' @export
setMethod("getLoopIndices",
          signature = "DotBracketDataFrameList",
          definition = function(x, bracket.type, warn.type.drops = TRUE){
            getLoopIndices(as(x,"CompressedSplitDotBracketDataFrameList"),
                           bracket.type = bracket.type,
                           warn.type.drops = warn.type.drops)
          })

#' @rdname getBasePairing
#' @export
setMethod("getLoopIndices",
          signature = "SimpleSplitDotBracketDataFrameList",
          definition = function(x, bracket.type, warn.type.drops = TRUE){
            getLoopIndices(as(x,"CompressedSplitDotBracketDataFrameList"),
                           bracket.type, warn.type.drops)
          })

#' @rdname getBasePairing
#' @export
setMethod("getLoopIndices",
          signature = "CompressedSplitDotBracketDataFrameList",
          definition = function(x, bracket.type, warn.type.drops = TRUE){
            .get_idx_of_loops_from_dbdfl(x, bracket.type, warn.type.drops)
          })

# switch annotation ------------------------------------------------------------

#' @name convertAnnotation
#' 
#' @title Convert between dot bracket annotations
#' 
#' @description
#' \code{convertAnnotation} converts a type of dot bracket annotation into 
#' another. This only works if the original bracket type is present and the 
#' target bracket type is not.
#' 
#' @param x a \code{DotBracketString}, \code{DotBracketStringSet} or
#' \code{DotBracketStringSetList}
#' @param from which annotation type should be converted? Must be one of the 
#' following values: \code{1L = '()', 2L = '<>', 3L = '[]', 4L = '{}'} and must
#' be present in the input.
#' @param to Into which annotation type should the selected one be converted? 
#' Must be one of the following values: 
#' \code{1L = '()', 2L = '<>', 3L = '[]', 4L = '{}'}. Must not be present in the
#' input.
#' 
#' @return The modified input object, a \code{DotBracketString*} object.
#' 
#' @examples 
#' str <- "((.))..[[..]]...{{..}}......."
#' dbs <- DotBracketString(str)
#' convertAnnotation(dbs, 1L, 2L)
NULL

#' @importFrom stringi stri_locate_all_regex
.str_replace_all_custom <- function(string, pattern, replacement)
{
  locations <- stringi::stri_locate_all_regex(string, pattern,
                                              omit_no_match = TRUE)
  f <- which(!vapply(locations,function(l){nrow(l) == 0},logical(1)))
  # Currently now idea how to avoid the loops
  for(i in f){
    loc <- locations[[i]]
    for(j in seq_len(nrow(loc))){
      stringi::stri_sub(string,loc[j,"start"],loc[j,"end"]) <- replacement
    }
  }
  string
}

.convert_db_annotation <- function(x, from, to)
{
  if(missing(from) ||
     missing(to) ||
     length(from) > 1L || 
     length(to) > 1L || 
     is.list(from) || 
     is.list(to) || 
     !is.integer(from) || 
     !is.integer(to)){
    stop("'from' and 'to' dot bracket types must be single integer values.",
         call. = FALSE)
  }
  if(from == to){
    return(x)
  }
  x_chr <- as.character(x)
  db_types <- .get_db_types(x_chr)
  if(!(from %in% db_types)){
    stop("'from' dot bracket type was not found in the input.", call. = FALSE)
  }
  if(to %in% db_types){
    stop("'to' dot bracket type was found in the input. Choose another one.",
         call. = FALSE)
  }
  if(from < 1L || 
     to < 1L || 
     from > length(STRUCTURE_OPEN_CHR) || 
     to > length(STRUCTURE_OPEN_CHR)){
    stop("'from'/'to' is out of range.")
  }
  from_open_chr <- STRUCTURE_OPEN_CHR[from]
  from_close_chr <- STRUCTURE_CLOSE_CHR[from]
  to_open_chr <- gsub("\\\\","",STRUCTURE_OPEN_CHR[to])
  to_close_chr <- gsub("\\\\","",STRUCTURE_CLOSE_CHR[to])
  x_chr_ans <- paste0(x_chr,collapse = "")
  x_chr_ans <- .str_replace_all_custom(x_chr_ans, from_open_chr, to_open_chr)
  x_chr_ans <- .str_replace_all_custom(x_chr_ans, from_close_chr, to_close_chr)
  partitioning <- PartitioningByWidth(x)
  ans <- stringr::str_sub(x_chr_ans, start(partitioning), end(partitioning))
  names(ans) <- names(x)
  DotBracketStringSet(ans)
}

#' @rdname convertAnnotation
#' @export
setMethod("convertAnnotation",
          signature = "DotBracketString",
          definition = function(x, from, to){
            .convert_db_annotation(as(x,"DotBracketStringSet"), from, to)[[1]]
          })
#' @rdname convertAnnotation
#' @export
setMethod("convertAnnotation",
          signature = "DotBracketStringSet",
          definition = function(x, from, to){
            .convert_db_annotation(x, from, to)
          })
#' @rdname convertAnnotation
#' @export
setMethod("convertAnnotation",
          signature = "DotBracketStringSetList",
          definition = function(x, from, to){
            DotBracketStringSetList(lapply(x, .convert_db_annotation, from, to))
          })

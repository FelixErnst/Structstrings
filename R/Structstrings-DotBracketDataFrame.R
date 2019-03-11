#' @include Structstrings.R
NULL

#' @name DotBracketDataFrame
#' @aliases DotBracketDataFrame DotBracketDataFrameList
#' 
#' @title DataFrame for storing base pairing information
#' 
#' @description 
#' The \code{DotBracketDataFrame} object is derived from the
#' \code{\link[S4Vectors:DataFrame-class]{DataFrame}} class. The
#' \code{DotBracketDataFrameList} is implemented analogous, which is also
#' available as \code{CompressedSplitDotBracketDataFrameList}. Since the names
#' are quite long, the following short cut functions are available for object
#' creation: \code{DBDF}, \code{DBDFL} and \code{SDBDFL}.
#' 
#' The \code{DotBracketDataFrame} can only contain 5 columns, which are named
#' \code{pos}, \code{forward}, \code{reverse}, \code{character} and \code{base}.
#' The last two columns are optional. The type of the first three has to be
#' \code{integer}, whereas the fourth is a \code{character} and fifth is a 
#' \code{XStringSet} column.
#' 
#' Upon creation and modification, the validity of the contained base pairing 
#' information is checked. If the information is not correct, an error is 
#' thrown.
#' 
#' @param ... for \code{DotBracketDataFrame} the input vectors and for
#' \code{DotBracketDataFrameList} the \code{DataFrame} or the 
#' \code{DotBracketDataFrame} objects.
#' @param compress If \code{compress = TRUE}, returns a 
#' \code{CompressedSplitDataFrameList} else returns a 
#' \code{SimpleSplitDataFrameList}.
#' @param cbindArgs If \code{cbindArgs = FALSE}, the ... arguments are coerced 
#' to \code{DotBracketDataFrame} objects and concatenated to form the result. If
#' \code{cbindArgs = TRUE}, the arguments are combined as columns. The arguments
#' must then be the same length, with each element of an argument mapping to an 
#' element in the result.
#' 
#' @return a \code{DotBracketDataFrame*} object.
#' 
#' @examples 
#' # Manual creation
#' df <- DataFrame(pos = c(1,2,3,4,5,6),
#'                 forward = c(6,5,0,0,2,1),
#'                 reverse = c(1,2,0,0,5,6))
#' # Either works
#' dbdf <- as(df,"DotBracketDataFrame")
#' dbdf <- DotBracketDataFrame(df)
#' # With multiple input DataFrames a SplitDotBracketDataFrameList is returned
#' dbdfl <- DotBracketDataFrame(df,df,df,df)
#' 
#' # Creation from a DotBracketString object is probably more common
#' data("dbs", package = "Structstrings", envir = environment())
#' dbdfl <- getBasePairing(dbs)
#' # Elements are returned as DotBracketDataFrames
#' dbdfl[[1]]
NULL

# DotBracketDataFrame ----------------------------------------------------------

DOTBRACKET_DATAFRAME_COLNAMES <- c("pos","forward","reverse","character","base")
DOTBRACKET_DATAFRAME_INT_COLS <- c("pos","forward","reverse")
DOTBRACKET_DATAFRAME_CHR_COLS <- c("character")
DOTBRACKET_DATAFRAME_NUCLEOTIDE_COLS <- c("base")

#' @rdname DotBracketDataFrame
#' @export
setClass(Class = "DotBracketDataFrame",
         contains = c("DataFrame"))
#' @rdname DotBracketDataFrame
#' @export
setClass(Class = "DotBracketDataFrameList",
         contains = c("DataFrameList", "SimpleList"))
#' @rdname DotBracketDataFrame
#' @export
setClass(Class = "SplitDotBracketDataFrameList",
         contains = c("SimpleSplitDataFrameList"))


setClass("CompressedDotBracketDataFrameList",
         prototype = prototype(unlistData = new("DotBracketDataFrame")),
         contains = c("CompressedDataFrameList"))
#' @rdname DotBracketDataFrame
#' @export
setClass("CompressedSplitDotBracketDataFrameList",
         contains = c("SplitDataFrameList",
                      "CompressedDotBracketDataFrameList"))

.valid.DotBracketDataFrame <- function(x)
{
  message <- paste0("At least 3 inputs are expected. If unnamed they are used ",
                    "in this order: pos, forward, reverse. The fourth and fifth ",
                    "are optional and must be character and XStringSet.")
  messageType <- paste0("The types of the input are expected to be ",
                        "'integer', 'integer', 'integer', 'character' and ",
                        "optionally 'XStringSet'.")
  if((ncol(x) > 1L && ncol(x) < 3L) || ncol(x) > 5L ){
    return(paste0(message,messageType))
  }
  if(!all(colnames(x) %in% DOTBRACKET_DATAFRAME_COLNAMES)){
    return(paste0(message))
  }
  if(any(x$forward < 0) || 
     any(x$reverse < 0) || 
     sum(x$forward) != sum(x$reverse)){
    return(paste0("Unmatched position in 'DotBracketDataFrame' object. Invalid ",
         "DotBracket annotation"))
  }
  colTypes <- lapply(x, classNameForDisplay)
  if(!all(unlist(colTypes[DOTBRACKET_DATAFRAME_INT_COLS]) == "integer")){
    return(paste0(messageType))
  }
  if(ncol(x) >= 4L){
    if(!all(unlist(colTypes[DOTBRACKET_DATAFRAME_CHR_COLS]) == "character")){
      return(paste0(messageType))
    }
  }
  if(ncol(x) == 5L){
    if(colnames(x)[5L] != DOTBRACKET_DATAFRAME_NUCLEOTIDE_COLS || 
       !is(x[,5L],"XStringSet")){
      return(paste0(messageType))
    }
  }
  NULL
}

.valid.DotBracketDataFrameList <- function(x)
{
  if(length(x) > 0L) {
    if(!all(vapply(as.list(x),is,logical(1),"DotBracketDataFrame"))){
      return(paste0("Only 'DotBracketDataFrame' are supported as elements."))
    }
    if(is(x,"CompressedList")){
      return(.valid.DotBracketDataFrame(x@unlistData))
    } else {
      ans <- lapply(x,.valid.DotBracketDataFrame)
      f <- !vapply(ans,is.null,logical(1))
      if(any(f)){
        return(ans[f])
      }
    }
    
  }
  NULL
}
S4Vectors:::setValidity2("DotBracketDataFrame", .valid.DotBracketDataFrame)
S4Vectors:::setValidity2("DotBracketDataFrameList",
                         .valid.DotBracketDataFrameList)
S4Vectors:::setValidity2("SplitDotBracketDataFrameList",
                         .valid.DotBracketDataFrameList)
S4Vectors:::setValidity2("CompressedSplitDotBracketDataFrameList",
                         .valid.DotBracketDataFrameList)

# DotBracketDataFrame conversion -----------------------------------------------

setMethod("relistToClass", "DotBracketDataFrame",
          function(x) "CompressedSplitDotBracketDataFrameList"
)

.adjust_DotBracketDataFrame_col_types <- function(from)
{
  if(all(colnames(from) %in% DOTBRACKET_DATAFRAME_COLNAMES) && ncol(from) > 5L){
    from[,DOTBRACKET_DATAFRAME_COLNAMES]
  }
  f <- !vapply(from@listData[DOTBRACKET_DATAFRAME_INT_COLS],
               is.integer,
               logical(1))
  if(any(f)){
    from@listData[DOTBRACKET_DATAFRAME_INT_COLS][f] <- 
      lapply(from@listData[DOTBRACKET_DATAFRAME_INT_COLS][f],as.integer)
  }
  if(length(from@listData) >= 4L){
    f <- !vapply(from@listData[DOTBRACKET_DATAFRAME_CHR_COLS],
                 is.character,
                 logical(1))
    if(any(f)){
      from@listData[DOTBRACKET_DATAFRAME_CHR_COLS][f] <- 
        lapply(from@listData[DOTBRACKET_DATAFRAME_CHR_COLS][f],as.character)
    }
  }
  from
}

.DataFrame_To_DotBracketDataFrame <- function(from)
{
  class(from) <- "DotBracketDataFrame"
  from <- .adjust_DotBracketDataFrame_col_types(from)
  validObject(from)
  from
}

.SimpleDataFrameList_To_DotBracketDataFrameList <- function(from)
{
  from@listData <- .norm_DotBracketDataFrame_input(from@listData)
  class(from) <- "DotBracketDataFrameList"
  validObject(from)
  from
}
.SimpleSplitDataFrameList_To_SplitDotBracketDataFrameList <- function(from)
{
  from@listData <- .norm_DotBracketDataFrame_input(from@listData)
  class(from) <- "SplitDotBracketDataFrameList"
  validObject(from)
  from
}
.CSDataFrameList_To_CSDotBracketDataFrameList <- function(from)
{
  ans <- unlist(from, use.names = FALSE)
  ans <- .DataFrame_To_DotBracketDataFrame(ans)
  ans <- relist(ans, from)
  validObject(ans)
  ans
}

#' @name DotBracketDataFrame
#' @export
setAs("DataFrame", "DotBracketDataFrame",
      function(from) .DataFrame_To_DotBracketDataFrame(from))
#' @name DotBracketDataFrame
#' @export
setAs("SimpleDataFrameList", "DotBracketDataFrameList",
      function(from) .SimpleDataFrameList_To_DotBracketDataFrameList(from))
#' @name DotBracketDataFrame
#' @export
setAs("SimpleSplitDataFrameList", "SplitDotBracketDataFrameList",
      function(from) .SimpleSplitDataFrameList_To_SplitDotBracketDataFrameList(from))
#' @name DotBracketDataFrame
#' @export
setAs("CompressedSplitDataFrameList", "CompressedSplitDotBracketDataFrameList",
      function(from) .CSDataFrameList_To_CSDotBracketDataFrameList(from))
#' @name DotBracketDataFrame
#' @export
setAs("list", "DotBracketDataFrameList",
      function(from) do.call("DotBracketDataFrameList",from))
#' @name DotBracketDataFrame
#' @export
setAs("list", "SplitDotBracketDataFrameList",
      function(from) do.call("SplitDotBracketDataFrameList",
                             c(from, list(compress = FALSE))))
#' @name DotBracketDataFrame
#' @export
setAs("list", "CompressedSplitDotBracketDataFrameList",
      function(from) do.call("SplitDotBracketDataFrameList",
                             c(from, list(compress = TRUE))))
#' @name DotBracketDataFrame
#' @export
setAs("CompressedSplitDotBracketDataFrameList", "DotBracketDataFrameList",
      function(from) DotBracketDataFrameList(as.list(from)))

#' @name DotBracketDataFrame
#' @export
setAs("CompressedSplitDotBracketDataFrameList", "SplitDotBracketDataFrameList",
      function(from) SplitDotBracketDataFrameList(as.list(from),
                                                  compress = FALSE))


# DotBracketDataFrame constructor ----------------------------------------------

.norm_DotBracketDataFrame_input <- function(x)
{
  if(length(x) == 0L){
    return(x)
  }
  f <- vapply(x,is,logical(1),"DataFrame") & 
    !vapply(x,is,logical(1),"DotBracketDataFrame")
  if(any(f)){
    x[f] <- lapply(x[f], .DataFrame_To_DotBracketDataFrame)
  }
  x
}

.rename_unnamed_cols <- function(x)
{
  if(is.null(names(x)) && ncol(x) <= 4L){
    colnames(x) <- DOTBRACKET_DATAFRAME_COLNAMES[seq_len(ncol(x))]
  }
  x
}

.rename_unnamed_args <- function(x)
{
  if(is.null(names(x)) && length(x) <= 4L){
    names(x) <- DOTBRACKET_DATAFRAME_COLNAMES[seq_along(x)]
  }
  x
}

.flatten_input_list <- function(x)
{
  if(length(x) == 0L){
    return(x)
  }
  f <- vapply(x,is.list,logical(1))
  if(any(f)){
    x <- do.call(c,c(list(x[!f]),x[f]))
  }
  x
}

#' @rdname DotBracketDataFrame
#' @export
DotBracketDataFrame <- function(...)
{
  args <- list(...)
  args <- .flatten_input_list(args)
  f <- vapply(args,is,logical(1),"DataFrame")
  if(all(f)){
    if(length(f) == 1L){
      return(as(args[[f]],"DotBracketDataFrame"))
    }
    return(SplitDotBracketDataFrameList(args))
  }
  if(any(f)){
    stop("Mixed inputs. Use either vectors per column or a DataFrame.")
  }
  args <- .rename_unnamed_args(args)
  ans <- DataFrame(args, row.names = NULL, check.names = TRUE)
  .DataFrame_To_DotBracketDataFrame(ans)
}
#' @rdname DotBracketDataFrame
#' @export
DBDF <- function(...)
{
  DotBracketDataFrame(..., row.names = NULL,check.names = TRUE)
}
#' @rdname DotBracketDataFrame
#' @export
DotBracketDataFrameList <- function(...)
{
  args <- list(...)
  args <- .flatten_input_list(args)
  args <- .norm_DotBracketDataFrame_input(args)
  .SimpleDataFrameList_To_DotBracketDataFrameList(DataFrameList(args))
}
#' @rdname DotBracketDataFrame
#' @export
DBDFL <- function(...) DotBracketDataFrameList(...)
#' @rdname DotBracketDataFrame
#' @export
SplitDotBracketDataFrameList <- function(..., compress = TRUE, 
                                         cbindArgs = FALSE)
{
  args <- list(...)
  args <- .flatten_input_list(args)
  ans <- IRanges::SplitDataFrameList(args,
                                     compress = compress,
                                     cbindArgs = cbindArgs)
  if(compress){
    return(.CSDataFrameList_To_CSDotBracketDataFrameList(ans))
  }
  .SimpleSplitDataFrameList_To_SplitDotBracketDataFrameList(ans)
}
#' @rdname DotBracketDataFrame
#' @export
SDBDFL <- function(..., compress = TRUE, cbindArgs = FALSE)
{
  SplitDotBracketDataFrameList(...,  compress = compress, cbindArgs = cbindArgs)
}

#' @rdname Structstrings-internals
#' @param x,i,j,...,value See \link{DataFrame}.
#' @export
setReplaceMethod(
  "[", "DotBracketDataFrame",
  function(x, i, j, ..., value)
  {
    # Starting from Bioc 3.6, value is of class 
    # DotBracketDataFrame instead of DataFrame. 
    # this is a problem since missing columns get autopopulated.
    # So its need to be converted to a DataFrame again.
    # 
    # Reason/place for this conversion unknown. Maybe directly 
    # Base C since DataFrame derives from list.
    value <- as(value[,seq_along(i),drop = FALSE],"DataFrame")
    callNextMethod(x, i, value = value)
  }
)

#' @include Structstrings.R
NULL

#' @name DotBracketDataFrame
#' @aliases DotBracketDataFrame DotBracketDataFrameList
#' 
#' @title DataFrame for storing base pairing information
#' 
#' @description 
#' The \code{DotBracketDataFrame} and \code{DotBracketDFrame} object is derived 
#' from the \code{\link[S4Vectors:DataFrame-class]{DataFrame}} and 
#' \code{\link[S4Vectors:DataFrame-class]{DFrame}} classes. 
#' \code{DotBracketDataFrame} implents the concept and can be used to implement
#' other backends than the in-memory one as done by \code{DotBracketDFrame}. 
#' 
#' The \code{DotBracketDataFrameList} is implemented analogous, which is also
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
#' \code{CompressedSplitDotBracketDataFrameList} else returns a 
#' \code{SimpleSplitDotBracketDataFrameList}.
#' @param cbindArgs If \code{cbindArgs = FALSE}, the ... arguments are coerced 
#' to \code{DotBracketDataFrame} objects and concatenated to form the result. If
#' \code{cbindArgs = TRUE}, the arguments are combined as columns. The arguments
#' must then be the same length, with each element of an argument mapping to an 
#' element in the result.
#' @param row.names See \code{\link[S4Vectors:DataFrame-class]{DataFrame}}
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
#' data("dbs", package = "Structstrings")
#' dbdfl <- getBasePairing(dbs)
#' # Elements are returned as DotBracketDataFrames
#' dbdfl[[1]]
NULL

# DotBracket & DotBracketDataFrame ---------------------------------------------

DOTBRACKET_DATAFRAME_COLNAMES <- c("pos","forward","reverse","character","base")
DOTBRACKET_DATAFRAME_INT_COLS <- c("pos","forward","reverse")
DOTBRACKET_DATAFRAME_CHR_COLS <- c("character")
DOTBRACKET_DATAFRAME_NUCLEOTIDE_COLS <- c("base")

################################################################################
# DotBracket - concept 
################################################################################

#' @rdname DotBracketDataFrame
#' @export
setClass(Class = "DotBracketDataFrame",
         contains = c("VIRTUAL","DataFrame"))

################################################################################
# DotBracketDataFrame - implementation 
################################################################################

#' @rdname DotBracketDataFrame
#' @export
setClass(Class = "DotBracketDFrame",
         contains = c("DotBracketDataFrame","DFrame"))

################################################################################
# DotBracketDataFrame - implementation 
################################################################################

#' @rdname DotBracketDataFrame
#' @export
setClass(Class = "DotBracketDataFrameList",
         contains = c("SimpleDataFrameList"),
         prototype = list(elementType = "DotBracketDataFrame"))

#' @rdname DotBracketDataFrame
#' @export
setClass(Class = "DotBracketDFrameList",
         contains = c("DFrameList", "DotBracketDataFrameList"),
         prototype = list(elementType = "DotBracketDFrame"))

#' @rdname DotBracketDataFrame
#' @export
setClass(Class = "SimpleSplitDotBracketDataFrameList",
         contains = c("SimpleSplitDataFrameList"),
         prototype = list(elementType = "DotBracketDataFrame"))

#' @rdname DotBracketDataFrame
#' @export
setClass(Class = "SimpleSplitDotBracketDFrameList",
         contains = c("SimpleSplitDFrameList",
                      "SimpleSplitDotBracketDataFrameList"),
         prototype = list(elementType = "DotBracketDFrame"))

#' @rdname DotBracketDataFrame
#' @export
setClass("CompressedSplitDotBracketDataFrameList",
         contains = c("CompressedSplitDataFrameList"),
         slots = c(unlistData = "DotBracketDataFrame"),
         prototype = list(unlistData = new("DotBracketDFrame")))

#' @rdname DotBracketDataFrame
#' @export
setClass("CompressedSplitDotBracketDFrameList",
         contains = c("CompressedSplitDFrameList",
                      "CompressedSplitDotBracketDataFrameList"),
         slots = c(unlistData = "DotBracketDFrame"),
         prototype = list(unlistData = new("DotBracketDFrame")))

.valid.DotBracketDataFrame <- function(object)
{
  message <- paste0("At least 3 columns are expected. If unnamed they are used",
                    " in this order: pos, forward, reverse. The fourth and ",
                    "fifth are optional and must be character and XStringSet.")
  messageType <- paste0("The types of the input are expected to be ",
                        "'integer', 'integer', 'integer', 'character' and ",
                        "optionally 'XStringSet'.")
  if((ncol(object) > 1L && ncol(object) < 3L) || ncol(object) > 5L ){
    return(paste0(message,messageType))
  }
  if(!all(colnames(object) %in% DOTBRACKET_DATAFRAME_COLNAMES)){
    return(paste0(message))
  }
  if(any(object$forward < 0) || 
     any(object$reverse < 0) || 
     sum(object$forward) != sum(object$reverse)){
    return(paste0("Unmatched position in 'DotBracketDataFrame' object. Invalid",
           " DotBracket annotation"))
  }
  colTypes <- lapply(object, classNameForDisplay)
  if(!all(unlist(colTypes[DOTBRACKET_DATAFRAME_INT_COLS]) == "integer")){
    return(paste0(messageType))
  }
  if(ncol(object) >= 4L){
    if(!all(unlist(colTypes[DOTBRACKET_DATAFRAME_CHR_COLS]) == "character")){
      return(paste0(messageType))
    }
  }
  if(ncol(object) == 5L){
    if(colnames(object)[5L] != DOTBRACKET_DATAFRAME_NUCLEOTIDE_COLS || 
       !is(object[,5L],"XStringSet")){
      return(paste0(messageType))
    }
  }
  NULL
}

.valid.DotBracketDataFrameList <- function(object)
{
  if(length(object) > 0L) {
    if(!all(vapply(as.list(object),is,logical(1),"DotBracketDataFrame"))){
      return(paste0("Only 'DotBracketDataFrame' are supported as elements."))
    }
    if(is(object,"CompressedList")){
      return(.valid.DotBracketDataFrame(object@unlistData))
    } else {
      ans <- lapply(object,.valid.DotBracketDataFrame)
      f <- !vapply(ans,is.null,logical(1))
      if(any(f)){
        return(ans[f])
      }
    }
    
  }
  NULL
}

setValidity("DotBracketDataFrame", .valid.DotBracketDataFrame)
setValidity("DotBracketDataFrameList", .valid.DotBracketDataFrameList)
setValidity("SimpleSplitDotBracketDataFrameList",
            .valid.DotBracketDataFrameList)
setValidity("CompressedSplitDotBracketDataFrameList",
            .valid.DotBracketDataFrameList)

################################################################################
# DotBracketDataFrame conversion
################################################################################

setMethod("relistToClass", "DotBracketDFrame",
          function(x) "CompressedSplitDotBracketDFrameList"
)

.adjust_DotBracketDataFrame_col_types <- function(from)
{
  if(!all(colnames(from) %in% DOTBRACKET_DATAFRAME_COLNAMES) && 
     ncol(from) > 3L){
    COLNAMES <- DOTBRACKET_DATAFRAME_COLNAMES %in% colnames(from)
    COLNAMES <- DOTBRACKET_DATAFRAME_COLNAMES[COLNAMES]
    from <- from[,COLNAMES]
    if(ncol(from) < 3L){
      stop("Input could not be courced to a meaningful DotBracketDataFrame. ",
           "Please check the names and length of the input.", call. = FALSE)
    }
  }
  INT_COLS <- DOTBRACKET_DATAFRAME_INT_COLS %in% colnames(from)
  INT_COLS <- DOTBRACKET_DATAFRAME_INT_COLS[INT_COLS]
  f <- !vapply(from@listData[INT_COLS],
               is.integer,
               logical(1))
  if(any(f)){
    from@listData[INT_COLS][f] <- 
      lapply(from@listData[INT_COLS][f],as.integer)
  }
  if(length(from@listData) >= 4L){
    CHR_COLS <- DOTBRACKET_DATAFRAME_CHR_COLS %in% colnames(from)
    CHR_COLS <- DOTBRACKET_DATAFRAME_CHR_COLS[CHR_COLS]
    f <- !vapply(from@listData[CHR_COLS],
                 is.character,
                 logical(1))
    if(any(f)){
      from@listData[CHR_COLS][f] <- 
        lapply(from@listData[CHR_COLS][f],as.character)
    }
  }
  from
}

.DataFrame_To_DotBracketDataFrame <- function(from)
{
  from <- .adjust_DotBracketDataFrame_col_types(from)
  if(!is(from,"DotBracketDFrame")){
    class(from) <- "DotBracketDFrame"
  }
  validObject(from)
  from
}

.SimpleDataFrameList_To_DotBracketDataFrameList <- function(from)
{
  listData <- .norm_DotBracketDataFrame_input(from@listData)
  from <- S4Vectors:::new_SimpleList_from_list("DotBracketDFrameList", listData)
  validObject(from)
  from
}

.SimpleSplitDataFrameList_To_SimpleSplitDotBracketDataFrameList <- 
  function(from){
  listData <- .norm_DotBracketDataFrame_input(from@listData)
  from <- new("SimpleSplitDotBracketDFrameList", listData = listData)
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
setAs("SimpleSplitDataFrameList", "SimpleSplitDotBracketDataFrameList",
      function(from) 
        .SimpleSplitDataFrameList_To_SimpleSplitDotBracketDataFrameList(from))
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
setAs("list", "SimpleSplitDotBracketDataFrameList",
      function(from) do.call("SplitDotBracketDataFrameList",
                             c(from, list(compress = FALSE))))
#' @name DotBracketDataFrame
#' @export
setAs("list", "CompressedSplitDotBracketDataFrameList",
      function(from) do.call("SplitDotBracketDataFrameList",
                             c(from, list(compress = TRUE))))

#' @name DotBracketDataFrame
#' @export
setAs("DotBracketDataFrameList",
      "SimpleSplitDotBracketDataFrameList",
      function(from) SplitDotBracketDataFrameList(as.list(from),
                                                  compress = FALSE))
#' @name DotBracketDataFrame
#' @export
setAs("DotBracketDataFrameList", "CompressedSplitDotBracketDataFrameList",
      function(from) SplitDotBracketDataFrameList(as.list(from)))

#' @name DotBracketDataFrame
#' @export
setAs("SimpleSplitDotBracketDataFrameList", "DotBracketDataFrameList",
      function(from) DotBracketDataFrameList(as.list(from)))

#' @name DotBracketDataFrame
#' @export
setAs("SimpleSplitDotBracketDataFrameList",
      "CompressedSplitDotBracketDataFrameList",
      function(from) SplitDotBracketDataFrameList(as.list(from),
                                                  compress = TRUE))
#' @name DotBracketDataFrame
#' @export
setAs("CompressedSplitDotBracketDataFrameList", "DotBracketDataFrameList",
      function(from) DotBracketDataFrameList(as.list(from)))

#' @name DotBracketDataFrame
#' @export
setAs("CompressedSplitDotBracketDataFrameList",
      "SimpleSplitDotBracketDataFrameList",
      function(from) SplitDotBracketDataFrameList(as.list(from),
                                                  compress = FALSE))

################################################################################
# DotBracketDataFrame constructor
################################################################################

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

.rename_unnamed_args <- function(x)
{
  if(is.null(names(x)) && length(x) <= 5L){
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
DotBracketDataFrame <- function(..., row.names = NULL){
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
  ans <- DataFrame(args, row.names = row.names, check.names = TRUE)
  .DataFrame_To_DotBracketDataFrame(ans)
}

#' @rdname DotBracketDataFrame
#' @export
DBDF <- function(...){
  DotBracketDataFrame(..., row.names = NULL)
}

#' @rdname DotBracketDataFrame
#' @export
DotBracketDataFrameList <- function(...)
{
  args <- list(...)
  args <- .flatten_input_list(args)
  ans <- DataFrameList(args)
  .SimpleDataFrameList_To_DotBracketDataFrameList(ans)
}

#' @rdname DotBracketDataFrame
#' @export
DBDFL <- function(...){
  DotBracketDataFrameList(...)
}  

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
  .SimpleSplitDataFrameList_To_SimpleSplitDotBracketDataFrameList(ans)
}

#' @rdname DotBracketDataFrame
#' @export
SDBDFL <- function(..., compress = TRUE, cbindArgs = FALSE)
{
  SplitDotBracketDataFrameList(...,  compress = compress, cbindArgs = cbindArgs)
}

# show method ------------------------------------------------------------------

setMethod("classNameForDisplay", "DotBracketDFrame",
          function(x) {
            if (class(x) == "DotBracketDFrame") 
              "DotBracketDataFrame" 
            else 
              class(x)
          }
)

setMethod("classNameForDisplay", "DotBracketDFrameList",
          function(x) {
            if (class(x) == "DotBracketDFrameList") 
              "DotBracketDataFrameList" 
            else 
              class(x)
          }
)

# misc functions ---------------------------------------------------------------

#' @rdname Structstrings-internals
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

#' @rdname Structstrings-internals
#' @export
setReplaceMethod(
  "colnames", "CompressedSplitDotBracketDataFrameList",
  function(x, value)
  {
    x <- callNextMethod()
    validObject(x)
    x
  }
)

#' @include Structstrings.R
#' @include Structstrings-DotBracketStringSet.R
NULL

#' @name StructuredXStringSet
#' @aliases StructuredRNAStringSet StructuredModRNAStringSet
#' 
#' @title StructuredRNAStringSet and StructuredModRNAStringSet for storing 
#' DotBracketAnnotation alongside nucleotide sequences
#' 
#' @description 
#' The \code{\link{StructuredXStringSet}} class can be used to store structure 
#' information alongside RNA sequences. The class behaves like the 
#' \code{\link[Biostrings:QualityScaledXStringSet-class]{QualityScaledXStringSet}}
#' classes.
#' 
#' Please note, that this does not check for validity regarding base pairing 
#' capabilities.
#' 
#' @details 
#' the \code{dotbracket} function allows access to the included 
#' \code{\link{DotBracketStringSet}}.
#' 
#' @param x For the \code{Structured*StringSet} constructors: Either a character
#' vector, or an \code{RNA*ModRNAString}, \code{RNA*ModRNAtringSet} or 
#' \code{RNA*ModRNAStringViews} object. For \code{writeQualityScaledXStringSet}:
#' A \code{StructuredRNAStringSet} or a \code{StructuredModRNAStringSet} 
#' derivative.
#' @param structure A \code{\link{DotBracketStringSet}}
#' @param bracket.type \code{getLoopIndices}: Which dot bracket annotation type 
#' should be converted into loop indices? Only usable, if more than one is 
#' present.
#' (\code{1L = '()', 2L = '<>', 3L = '[]', 4L = '{}'})
#' @param warn.type.drops See \code{\link{getLoopIndices}}
#' @param use.names,type,filepath,nrec,skip,seek.first.rec,append,...
#' See \code{\link{DotBracketStringSet-io}}
#' @param compress See \code{\link{getBasePairing}} or
#' \code{\link{DotBracketStringSet-io}}
#' @param return.sequence \code{TRUE}(default) or \code{FALSE}: Whether the 
#' sequence should be returned in the \code{base} column.
#' 
#' @return a \code{StructuredRNAStringSet} or \code{StructuredModRNAStringSet}
#' object.
#' 
#' @examples
#' str <- DotBracketStringSet("(())")
#' seq <- RNAStringSet("AGCU")
#' sdbs <- StructuredRNAStringSet(seq,str)
NULL

# virtual StructuredXStringSet class -------------------------------------------
setClass("StructuredXStringSet",
         contains = "XStringSet",
         representation(
           "VIRTUAL",
           structure = "DotBracketStringSet"
         )
)

setMethod("parallelSlotNames", "StructuredXStringSet",
          function(x) c("structure", callNextMethod())
)

#' @rdname StructuredXStringSet
#' @export
setClass("StructuredRNAStringSet",
         contains=c("RNAStringSet", "StructuredXStringSet")
)
#' @rdname StructuredXStringSet
#' @export
setClass("StructuredModRNAStringSet",
         contains=c("ModRNAStringSet", "StructuredXStringSet")
)

# checking validity ------------------------------------------------------------

.valid.StructuredXStringSet <- function(object){
  message <- NULL
  if (!all(nchar(object@structure) == 1 ||
           nchar(object@structure) == nchar(object))){
    message <- c(message,
                 "'nchar(structure)' must equal 1 or nchar of 'XStringSet'")
  }
  message
}

setValidity2("StructuredXStringSet",
            function(object){
              problems <- .valid.StructuredXStringSet(object)
              if (is.null(problems)) TRUE else problems
            }
)

# accessors --------------------------------------------------------------------

#' @rdname StructuredXStringSet
#' @export
setGeneric("dotbracket", function(x) standardGeneric("dotbracket"), 
           useAsDefault = function(x) x@structure)

# constructors -----------------------------------------------------------------

.normarg_structure <- function(structure, x){
  if (!is(structure, "DotBracketStringSet")){
    stop("'structure' must be of class 'DotBracketStringSet' or be ",
         "coercible to one.",
         call. = FALSE)
  }
  structure_width <- width(structure)
  x_width <- width(x)
  if (length(structure) == length(x)) {
    recycle_me <- structure_width != x_width
    if (any(recycle_me & structure_width != 1L)){
      stop("the DotBracket strings must be of length 1 or have the ",
           "same length as their corresponding string in 'x'")
    }
    recycle_idx <- which(recycle_me)
    width2 <- x_width[recycle_idx]
    idx <- relist(rep.int(1L, sum(width2)),
                  IRanges::PartitioningByWidth(width2))
    structure[recycle_idx] <- structure[recycle_idx][idx]
    return(structure)
  }
  if (length(structure) == 1L) {
    if (all(x_width == structure_width)){
      return(rep.int(structure, length(x)))
    }
    if (structure_width != 1L) {
      stop("when 'structure' is a single string it must be ",
           "a single letter or have the same width as all ",
           "the strings in 'x'")
    }
    structure <- DotBracketStringSet(BStringSet(rep.int(structure[[1L]],
                                                        max(x_width)),
                                                start = 1L,
                                                end = x_width))
    return(structure)
  }
  stop("'length(structure)' must equal 'length(x)' or 1")
}

StructuredXStringSet <- function(x, structure) {
  if (!is(x, "XStringSet")){
    stop("'x' must be of class 'XStringSet'")
  }
  structure <- .normarg_structure(structure, x)
  output <- as(x, paste0("Structured", class(x)))
  if(is.null(names(output)) && 
     !is.null(names(structure))){
    names(output) <- names(structure)
  }
  if(!is.null(names(structure))){
    names(structure) <- NULL
  }
  slot(output, "structure", check = FALSE) <- structure
  output
}

#' @rdname StructuredXStringSet
#' @export
StructuredRNAStringSet <- function(x, structure){
  StructuredXStringSet(RNAStringSet(x),
                       as(structure,"DotBracketStringSet"))
}
#' @rdname StructuredXStringSet
#' @export
StructuredModRNAStringSet <- function(x, structure){
  StructuredXStringSet(ModRNAStringSet(x),
                       as(structure,"DotBracketStringSet"))
}

# overwrite some functions to work with StructuredXStringSet -------------------
setMethod("windows", "StructuredXStringSet",
          function(x, start = NA, end = NA, width = NA){
            x <- callNextMethod()
            x@structure <- windows(x@structure,
                                   start = start,
                                   end = end,
                                   width = width)
            x
          }
)

setMethod("reverse", "StructuredXStringSet",
          function(x){
            x <- callNextMethod()
            x@structure <- reverse(x@structure)
            x
          }
)

setMethod("reverseComplement", "StructuredXStringSet",
          function(x){
            x <- callNextMethod()
            x@structure <- reverse(x@structure)
            x
          }
)

# show method ------------------------------------------------------------------

setMethod("show", "StructuredXStringSet",
          function(object){
            cat("  A ", class(object), " instance containing:\n", sep="")
            cat("\n")
            if(is(object,"ModStringSet")){
              selectMethod("show", "ModStringSet")(as(object, "ModStringSet"))
            } else {
              selectMethod("show", "XStringSet")(as(object, "XStringSet"))
            }
            cat("\n")
            show(dotbracket(object))
            cat("\n")
          }
)

# read and writing functions ---------------------------------------------------

.read_StructuredXStringSet <- function(type = "RNA",
                                       filepath,
                                       nrec,
                                       skip,
                                       seek.first.rec,
                                       use.names){
  className <- paste0("Structured",type,"StringSet")
  methodName <- paste0("read",type,"StringSet")
  x <- do.call(methodName, list(filepath,
                                format = "fastq",
                                nrec,
                                skip,
                                seek.first.rec,
                                use.names,
                                with.qualities = TRUE))
  structure <- DotBracketStringSet(mcols(x)[ , "qualities"])
  do.call(className, list(x, structure))
}
#' @rdname StructuredXStringSet
#' @export
readStructuredRNAStringSet <- function(filepath,
                                       nrec = -1L,
                                       skip = 0L,
                                       seek.first.rec = FALSE,
                                       use.names = TRUE){
  .read_StructuredXStringSet("RNA",
                             filepath,
                             nrec,
                             skip,
                             seek.first.rec,
                             use.names)
}
#' @rdname StructuredXStringSet
#' @export
readStructuredModRNAStringSet <- function(filepath,
                                          nrec = -1L,
                                          skip = 0L,
                                          seek.first.rec = FALSE,
                                          use.names = TRUE){
  .read_StructuredXStringSet("ModRNA",
                             filepath,
                             nrec,
                             skip,
                             seek.first.rec,
                             use.names)
}
#' @rdname StructuredXStringSet
#' @export
writeStructuredXStringSet <- function(x,
                                      filepath,
                                      append = FALSE,
                                      compress = FALSE){
  if(is.null(names(x)) && is.null(names(dotbracket(x)))){
    stop("either 'x' or 'dotbracket' must have names")
  }
  if (!is(x, "StructuredXStringSet")){
    stop("'x' must be a StructuredXStringSet object")
  }
  writeXStringSet(x,
                  filepath,
                  append,
                  compress,
                  compression_level = NA,
                  format = "fastq",
                  qualities = dotbracket(x))
}

# conversion -------------------------------------------------------------------

#' @rdname StructuredXStringSet
#' @export
setMethod("getBasePairing", "StructuredXStringSet",
          function(x,
                   compress = TRUE,
                   return.sequence = FALSE) {
            ans <- getBasePairing(dotbracket(x),
                                  compress)
            # if sequence should not be save in the DotBracketDataFrame
            if(return.sequence){
              return(ans)
            }
            # downgrade to sequence only object
            if(is(x,"ModStringSet")){
              seq <- as(x, "ModStringSet")
            } else {
              seq <- as(x, "XStringSet")
            }
            classNameSeq <- class(seq)
            seqs <- lapply(seq,
                           function(z){
                             as(Views(z,
                                      start = seq_len(length(z)),
                                      width = 1),
                                classNameSeq)
                           })
            if(is(ans,"CompressedList")){
              seqList <- do.call(paste0(classNameSeq,"List"),
                                 seqs)
              ans@unlistData$base <- unlist(do.call(paste0(classNameSeq,"List"),
                                                    seqs))
            } else {
              ans <- do.call("SplitDotBracketDataFrameList",
                             c(mapply(
                                 function(z,s){
                                   z$base <- s
                                   z
                                 },
                                 ans,
                                 seqs),
                             list(compress = FALSE)))
            }
            ans
          }
)

#' @rdname StructuredXStringSet
#' @export
setMethod("getLoopIndices", 
          "StructuredXStringSet",
          function(x,
                   bracket.type,
                   warn.type.drops = TRUE) {
            getLoopIndices(dotbracket(x),
                       bracket.type,
                       warn.type.drops)
          }
)

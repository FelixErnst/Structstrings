#' @title Structstrings: implementation of the dot bracket annotations with 
#' Biostrings
#'
#' @author Felix G M Ernst [aut,cre]
#'
#' @description
#' The \code{Structstrings} package implements the widely used to bracket
#' annotation for storing base pairing information in structured RNA. For
#' example it is used in the ViennaRNA package (Lorenz et al. 2011), the
#' tRNAscan-SE software (Lowe et al. 1997) and the tRNAdb (Jühling et al. 2009).
#'
#' \code{Structstrings} uses the infrastructure provided by the
#' \code{Biostrings} package and derives the class
#' \code{\link{DotBracketString}} and such from the equivalent
#' \code{\link{BString}} class. From these base pair table can be produced for
#' in depth analysis. For this purpose the \code{\link{DotBracketDataFrame}}
#' class is derived from the \code{\link{DataFrame}} class. In addition the loop
#' IDs of the base pairs can be retrieved as a \code{\link{LoopIndexList}}, a
#' derivate if the \code{\link{IntegerList}}. Generally, it checks automatically
#' for the validity of the dot bracket annotation.
#'
#' The conversion of the \code{\link{DotBracketString}} to the base pair table
#' and the loop indices is implemented in C for efficiency. The C implementation
#' to a large extent inspired by the
#' \href{https://www.tbi.univie.ac.at/RNA/}{ViennaRNA} package.
#'
#' This package was developed as a requirement for the \code{tRNA} package. 
#' However, other projects might benefit as well, so it was split of and 
#' improved upon.
#' 
#' @section Manual:
#' Please refer to the Structstrings vignette for an example how to work and 
#' use the package: \href{../doc/Structstrings.html}{Structstrings}.
#' 
#' @references
#' Lorenz, Ronny; Bernhart, Stephan H.; Höner zu Siederdissen, Christian; Tafer,
#' Hakim; Flamm, Christoph; Stadler, Peter F.; Hofacker, Ivo L. (2011):
#' "ViennaRNA Package 2.0". Algorithms for Molecular Biology 6:26.
#' \href{https://doi.org/10.1186/1748-7188-6-26}{doi:10.1186/1748-7188-6-26}
#'
#' Lowe, T.M.; Eddy, S.R.(1997): "tRNAscan-SE: A program for improved detection
#' of transfer RNA genes in genomic sequence". Nucl. Acids Res. 25: 955-964.
#' \href{https://doi.org/10.1093/nar/25.5.955}{doi:10.1093/nar/25.5.955}
#'
#' Jühling, Frank; Mörl, Mario; Hartmann, Roland K.; Sprinzl, Mathias; Stadler,
#' Peter F.; Pütz, Joern (2009): "TRNAdb 2009: Compilation of tRNA Sequences and
#' tRNA Genes." Nucleic Acids Research 37 (suppl_1): D159–D162.
#' \href{https://doi.org/10.1093/nar/gkn772}{doi:10.1093/nar/gkn772}.
#' 
#' @name Structstrings
NULL

#' @keywords internal
"_PACKAGE"

#' @useDynLib Structstrings
#' @import methods
#' @import S4Vectors
#' @import IRanges
#' @import BiocGenerics
#' @import Biostrings
requireNamespace("S4Vectors")
requireNamespace("BiocGenerics")
requireNamespace("Biostrings")

#' @name Structstrings-internals
#' 
#' @title Structstrings internals
#' 
#' @description 
#' Analog to \code{Biostrings} there are a few objects, which should only be 
#' used internally, but may be of use to other package developers.
#' Otherwise take care.
#' 
#' @param seqtype,x,start,end,width,value,i,j,... used internally
#' 
#' @examples 
#' DOTBRACKET_CHAR_VALUES
#' DOTBRACKET_ALPHABET
#' STRUCTURE_NEUTRAL_CHR
#' STRUCTURE_OPEN_CHR
#' STRUCTURE_CLOSE_CHR
#' 
#' # the replace method for a DotBracketDataFrame had to be reimplemented
#' # because of the requirement of columns for a DotBracketDataFrameList and
#' # DotBracketDataFrame
#' data("dbs", package = "Structstrings")
#' dbdfl <- getBasePairing(dbs)
#' # Elements are returned as DotBracketDataFrames
#' dbdf <- dbdfl[[1]]
#' dbdfl[[1]] <- dbdf
#' dbdfl[1] <- dbdfl[1]
NULL

# this must match the defines in inst/include/Structstrings_defines.h
#' @rdname Structstrings-internals
#' @format a \code{integer} vector of length 9 containing the integer values
#' of the dotbracket alphabet
#' @export
DOTBRACKET_CHAR_VALUES <- c(40L,41L,46L,60L,62L,91L,93L,123L,125L)
#' @rdname Structstrings-internals
#' @format a \code{character} vector of length 9 containing the single 
#' characters of the dotbracket alphabet
#' @export
DOTBRACKET_ALPHABET <- strsplit(rawToChar(as.raw(DOTBRACKET_CHAR_VALUES)),
                                "")[[1]]
#' @rdname Structstrings-internals
#' @format a \code{character} vector of length 1 containing the character for
#' unpaired positions
#' @export
STRUCTURE_NEUTRAL_CHR <- c(".")
#' @rdname Structstrings-internals
#' @format a \code{character} vector of length 4 containing the opening 
#' character of the dotbracket alphabet
#' @export
STRUCTURE_OPEN_CHR <- c("\\(","<","\\[","\\{")
#' @rdname Structstrings-internals
#' @format a \code{character} vector of length 4 containing the closing 
#' character of the dotbracket alphabet
#' @export
STRUCTURE_CLOSE_CHR <- c("\\)",">","\\]","\\}")

# data -------------------------------------------------------------------------

#' @name Structstrings-data
#' @title Structstrings example data
#' @description Example data for using the Structstrings package
#' @source sequence and  dot bracket annotation of tRNAscan-SE output for 
#' *S. cerevisiae* imported using 
#' \code{\link[tRNAscanImport:tRNAscanImport]{tRNAscanImport}}. The example file
#' is part of the \code{tRNAscanImport} package.
#' @docType data
#' @format object of class \code{\link{DotBracketStringSet}} and 
#' \code{\link{DNAStringSet}}
#' 
#' @usage data(dbs)
#' @keywords datasets
"dbs"
#' @name Structstrings-data
#' @usage data(nseq)
"nseq"

# Import of non-exported functions
.namesW <- Biostrings:::.namesW
.XStringSetList <- Biostrings:::XStringSetList

.subseq <- function(x, start = NA, end = NA, width = NA){
  subseq(as(x,"BString"),start = start, end = end, width = width)
}
.toSeqSnippet <- function(x, width){
  if (width < 7L)
    width <- 7L
  ## Do NOT use nchar() here as it wouldn't do the right thing on a
  ## MaskedXString object!
  x_len <- length(x)
  if (x_len <= width) {
    ans <- as.character(x)
  } else {
    w1 <- (width - 2L) %/% 2L
    w2 <- (width - 3L) %/% 2L
    ans <- paste0(as.character(.subseq(x, start=1, width=w1)),
                  "...",
                  as.character(.subseq(x, end=x_len, width=w2)))
  }
  if (is(x, "XString") || is(x, "MaskedXString"))
    class(ans) <- c(seqtype(x), class(ans))  # for S3 dispatch
  # in add_colors()
  ans
}

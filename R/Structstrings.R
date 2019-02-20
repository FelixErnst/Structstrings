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
#' @docType package
#' @name Structstrings
NULL

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
#' Analog to \code{Biostrings} there are a few functions, which should only be 
#' used internally. Otherwise take care.
NULL

# this must match the defines in inst/include/Structstrings_defines.h
#' @rdname Structstrings-internals
#' @export
DOTBRACKET_CHAR_VALUES <- c(40L,41L,46L,60L,62L,91L,93L,123L,125L)
#' @rdname Structstrings-internals
#' @export
DOTBRACKET_ALPHABET <- strsplit(rawToChar(as.raw(DOTBRACKET_CHAR_VALUES)),
                                "")[[1]]
#' @rdname Structstrings-internals
#' @export
STRUCTURE_NEUTRAL_CHR <- c(".")
#' @rdname Structstrings-internals
#' @export
STRUCTURE_OPEN_CHR <- c("\\(","<","\\[","\\{")
#' @rdname Structstrings-internals
#' @export
STRUCTURE_CLOSE_CHR <- c("\\)",">","\\]","\\}")

# data -------------------------------------------------------------------------

#' @name Structstrings-data
#' @title Structstrings example data
#' @description Example data for using the Structstrings package
#' @docType data
#' @usage dbs
#' @format object of class \code{\link{DotBracketStringSet}} and 
#' \code{\link{DNAStringSet}}
#' @keywords datasets Structstrings
"dbs"
#' @name Structstrings-data
#' @usage seq
"seq"

#' @include Structstrings.R
NULL

#' @name DotBracketStringSet-io
#' @aliases readDotBracketStringSet readDotBracketStringSet
#' 
#' @title Reading and writing DotBracketStringSet objects
#' 
#' @description
#' \code{readDotBracketStringSet} and \code{writeDotBracketStringSet} are 
#' functions to read and write dot bracket strings from/to file. Since the
#' \code{<>} is in conflict with the fasta format, saving to fastq file is
#' sometimes the only option. Saving a string with a \code{<>} bracket type to a
#' fasta file will throw an error.
#' 
#' The functions use the underlying \code{Biostrings} infrastructure and share
#' most of its parameters. For a more detailed look have a look 
#' \code{\link[Biostrings:XStringSet-io]{here}}.
#' 
#' @param x A DotBracketStringSet object
#' @param filepath The file name, when writing, or file name(s) when reading.
#' @param format "fasta" or "fastq"
#' @param nrec Single integer. The maximum of number of records to read in. 
#' Negative values are ignored.
#' @param skip Single non-negative integer. The number of records of the data 
#' file(s) to skip before beginning to read in records.
#' @param append \code{TRUE} or \code{FALSE}. If \code{TRUE} output will be 
#' appended to file. Otherwise, it will overwrite the contents of file.
#' @param seek.first.rec,with.qualities,compress,...,use.names,objname,dirpath,save.dups,verbose 
#' Have a look \code{\link[Biostrings:XStringSet-io]{here}}.
#'
#' @return \code{readDotBracketStringSet} returns a \code{DotBracketStringSet}
#' object, \code{writeDotBracketStringSet} returns \code{NULL} invisibly.
#' 
#' @examples
#' data("dbs", package = "Structstrings", envir = environment())
#' file <- tempfile()
#' # works both since a DotBracketStringSet is a BStringSet
#' writeXStringSet(dbs,file)
#' writeDotBracketStringSet(dbs,file)
#' # to return immediatly a DotBracketStringSet us readDotBracketStringSet()
#' dbs2 <- readDotBracketStringSet(file)
NULL

#' @rdname DotBracketStringSet-io
#' @export
readDotBracketStringSet <- function(filepath, format = "fasta", nrec = -1L,
                                    skip = 0L, seek.first.rec = FALSE,
                                    use.names = TRUE, with.qualities = FALSE){
  ans <- Biostrings::readBStringSet(filepath, format, nrec, skip,
                                    seek.first.rec, use.names, with.qualities)
  as(ans,"DotBracketStringSet")
}

#' @rdname DotBracketStringSet-io
#' @export
writeDotBracketStringSet <- function(x, filepath, append = FALSE,
                                     compress = FALSE, format = "fasta", ...){
  if(2L %in% .get_db_types(x)){
    stop("The dot bracket type '<>' cannot be saved as a fasta file, since it ",
         "interferes with the fasta nomenclature. Please convert it into ",
         "another type of dot bracket annotation using convertAnnotation() or ",
         "save it in the fastq format.",
         call. = FALSE)
  }
  Biostrings::writeXStringSet(x, filepath, append, compress, 
                              compression_level = NA, format, ...)
}

#' @rdname DotBracketStringSet-io
#' @export
saveDotBracketStringSet <- function(x, objname, dirpath = ".",
                                    save.dups = FALSE, verbose = TRUE){
  Biostrings::saveXStringSet(x, objname = objname, dirpath = dirpath,
                             save.dups = save.dups, verbose = verbose)
}


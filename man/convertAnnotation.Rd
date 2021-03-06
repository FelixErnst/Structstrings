% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/Structstrings-conversion.R
\name{convertAnnotation}
\alias{convertAnnotation}
\alias{convertAnnotation,DotBracketString-method}
\alias{convertAnnotation,DotBracketStringSet-method}
\alias{convertAnnotation,DotBracketStringSetList-method}
\title{Convert between dot bracket annotations}
\usage{
convertAnnotation(x, from, to)

\S4method{convertAnnotation}{DotBracketString}(x, from, to)

\S4method{convertAnnotation}{DotBracketStringSet}(x, from, to)

\S4method{convertAnnotation}{DotBracketStringSetList}(x, from, to)
}
\arguments{
\item{x}{a \code{DotBracketString}, \code{DotBracketStringSet} or
\code{DotBracketStringSetList}}

\item{from}{which annotation type should be converted? Must be one of the 
following values: \code{1L = '()', 2L = '<>', 3L = '[]', 4L = '{}'} and must
be present in the input.}

\item{to}{Into which annotation type should the selected one be converted? 
Must be one of the following values: 
\code{1L = '()', 2L = '<>', 3L = '[]', 4L = '{}'}. Must not be present in the
input.}
}
\value{
The modified input object, a \code{DotBracketString*} object.
}
\description{
\code{convertAnnotation} converts a type of dot bracket annotation into 
another. This only works if the original bracket type is present and the 
target bracket type is not.
}
\examples{
str <- "((.))..[[..]]...{{..}}......."
dbs <- DotBracketString(str)
convertAnnotation(dbs, 1L, 2L)
}

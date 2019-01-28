# Structstrings: implementation of the dot bracket annotations with Biostrings

The `Structstrings` package implements the widely used to bracket annotation for 
storing base pairing information in structured RNA. For example it is heavily 
used in the ViennaRNA ([Lorenz et al. 2011](#Literature)) package, the tRNAscan-SE 
([Lowe et al. 1997](#Literature)) software and the tRNAdb 
([Juehling et al. 2009](#Literature)).

`Structstrings` uses the infrastructure provided by the
[Biostrings](#Literature) package and derives the class `DotBracketString` and
related class from the `BString` class. From these base pair tables can be
produced for in depth analysis, for which the `DotBracketDataFrame` class
is derived from the `DataFrame` class. In addition, the loop indices of the base
pairs can be retrieved as a `LoopIndexList`, a derivate if the `IntegerList` 
class. Generally, all classes check automatically for the validity of the base
pairing information.

The conversion of the `DotBracketString` to the base pair table and the loop 
indices is implemented in C for efficiency. The C implementation is to a large 
extent inspired by the [ViennaRNA](https://www.tbi.univie.ac.at/RNA/) package.

This package was developed as an improvement for the `tRNA` package. However,
other projects might benefit as well, so it was split of and improved upon.

## Installation

The current version of the `Structstrings` package is available from GitHub.
 
```{r}
# Dependencies
devtools::install_github("FelixErnst/Modstrings")
BiocManager::install("Biostrings")
# Installation
devtools::install_github("FelixErnst/Structstrings")
# Load and attach thepackage
library("Structstrings")
```
A submission to Bioconductor is planned.

## Functions

Please have a look at vignette for details on the provided functions.

# Literature

- Lorenz, Ronny; Bernhart, Stephan H.; Höner zu Siederdissen, Christian; 
Tafer, Hakim; Flamm, Christoph; Stadler, Peter F.; Hofacker, Ivo L. (2011):
"ViennaRNA Package 2.0". Algorithms for Molecular Biology 6:26. 
doi:[10.1186/1748-7188-6-26](https://doi.org/10.1186/1748-7188-6-26)
- Lowe, T.M.; Eddy, S.R.(1997): "tRNAscan-SE: A program for 
improved detection of transfer RNA genes in genomic sequence". Nucl. Acids Res. 
25: 955-964. doi:[10.1093/nar/25.5.955](https://doi.org/10.1093/nar/25.5.955)
- Jühling, Frank; Mörl, Mario; Hartmann, Roland K.; Sprinzl, Mathias; Stadler,
Peter F.; Pütz, Joern (2009): "TRNAdb 2009: Compilation of tRNA Sequences and
tRNA Genes." Nucleic Acids Research 37 (suppl_1): D159–D162.
doi:[10.1093/nar/gkn772](https://doi.org/10.1093/nar/gkn772). 
- Pagès, H.; Aboyoun, P.; Gentleman, R.; DebRoy, S. (2018). "Biostrings: 
Efficient manipulation of biological strings." R package version 2.50.1. 

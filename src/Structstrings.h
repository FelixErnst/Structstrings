/*
* 
*/
#include "../inst/include/Structstrings_defines.h"
/*
* 
*/
#include <stdlib.h>
#include <string.h>

#include "S4Vectors_interface.h"
#include "IRanges_interface.h"

/* utils.c */
void* util_mem_alloc(
    unsigned size
);

/* DotBracketDataFrame_class.c */
SEXP new_DotBracketDataFrame(
    SEXP vars,
    SEXP rownames,
    SEXP nrows
);

SEXP new_DotBracketDataFrameList(
    SEXP unlistData,
    SEXP partitioning
);
  
SEXP new_DotBracketDataFrame_from_CHARACTER(
    const char *chr,
    SEXP offset
);

SEXP construct_offset_from_ends(
    const char *chr,
    SEXP ends,
    SEXP offset
);
  
SEXP new_DotBracketDataFrameList_from_CHARACTER(
    SEXP x,
    SEXP partitioning
);

SEXP get_dot_bracket_from_base_pairing(
    SEXP x
);

SEXP new_DotBracket_from_INTEGER(
    SEXP x
);

SEXP new_LoopIndexList_from_CHARACTER_LIST(
    SEXP x,
    SEXP type
);

SEXP new_LoopIndexList_from_INTEGER_LIST(
    SEXP x
);

SEXP new_LoopIndexList_from_LIST(
    SEXP list
);

SEXP new_LoopIndexList(
    SEXP list,
    SEXP partitioning
);

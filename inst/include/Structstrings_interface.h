#include "Structstrings_defines.h"

/* DotBracketDataFrame_class.c */
SEXP new_DotBracketDataFrame(SEXP vars,
                             SEXP rownames,
                             SEXP nrows);

SEXP new_DotBracketDataFrameList(SEXP unlistData,
                                 SEXP partitioning);

SEXP new_DotBracketDataFrame_from_CHARACTER(const char *chr,
                                            const int *offset);

SEXP new_DotBracketDataFrameList_from_CHARACTER(SEXP x,
                                                SEXP partitioning);

SEXP new_LoopIndexList_from_CHARACTER(SEXP x,
                                      SEXP type);

SEXP new_LoopIndexList_from_INTEGER(SEXP x);

SEXP new_DotBracket_from_INTEGER(SEXP x);

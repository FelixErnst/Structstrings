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

SEXP new_LoopIndexList_from_CHARACTER_LIST(SEXP x,
                                           SEXP type);

SEXP new_LoopIndexList_from_INTEGER_LIST(SEXP x);

SEXP new_LoopIndexList_from_LIST(SEXP list);

SEXP new_LoopIndexList(SEXP list, SEXP partitioning);

SEXP new_DotBracket_from_INTEGER(SEXP x);

/*
 * 
 */
#ifndef STRUCTSTRINGS_DEFINES_H
#define STRUCTSTRINGS_DEFINES_H
/*
 * 
 */
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

/*
 * this must match the values in R/Structstrings.R
 */
#define STRUCTURE_CHR_NULL 46
#define STRUCTURE_OPEN_CHAR_INIT                               \
bracket_types_open[4] = {"(","<","[","{"}
#define STRUCTURE_CLOSE_CHAR_INIT                              \
bracket_types_close[4] = {")",">","]","}"}
#define STRUCTURE_OPEN_INT_INIT                               \
bracket_int_open[4] = {40,60,91,123}
#define STRUCTURE_CLOSE_INT_INIT                              \
bracket_int_close[4] = {41,62,93,125}
#define STRUCTURE_N_CHAR_TYPES 4
#endif

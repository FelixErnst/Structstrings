#include "Structstrings_interface.h"

#define DEFINE_CCALLABLE_STUB(retT, stubname, Targs, args)                            \
typedef retT(*__ ## stubname ## _funtype__)Targs;                                     \
retT stubname Targs                                                                   \
{                                                                                     \
  static __ ## stubname ## _funtype__ fun = NULL;                                     \
  if (fun == NULL)                                                                    \
    fun = (__ ## stubname ## _funtype__) R_GetCCallable("Structstrings", "_" #stubname); \
  return fun args;                                                                    \
}

/*
* Using the above macro when retT (the returned type) is void will make Sun
* Studio 12 C compiler unhappy. So we need to use the following macro to
* handle that case.
*/
#define DEFINE_NOVALUE_CCALLABLE_STUB(stubname, Targs, args)                          \
typedef void(*__ ## stubname ## _funtype__)Targs;                                     \
void stubname Targs                                                                   \
{                                                                                     \
  static __ ## stubname ## _funtype__ fun = NULL;                                     \
  if (fun == NULL)                                                                    \
    fun = (__ ## stubname ## _funtype__) R_GetCCallable("Structstrings", "_" #stubname); \
  fun args;                                                                           \
  return;                                                                             \
}                                                              \

/*
 * Stubs for callables defined in DotBracketDataFrame_class.c
 */
DEFINE_CCALLABLE_STUB(SEXP, new_DotBracketDFrame,
                      (SEXP vars, SEXP rownames, SEXP nrows),
                      (     vars,      rownames,      nrows)
)
DEFINE_CCALLABLE_STUB(SEXP, new_DotBracketDFrameList,
                      (SEXP unlistData, SEXP partitioning),
                      (     unlistData,      partitioning)
)

DEFINE_CCALLABLE_STUB(SEXP, new_DotBracketDFrameList_from_CHARACTER,
                      (SEXP x, SEXP partitioning),
                      (     x,      partitioning)
)

DEFINE_CCALLABLE_STUB(SEXP, new_LoopIndexList_from_CHARACTER_LIST,
                      (SEXP x, SEXP type),
                      (     x,      type)
)
  
DEFINE_CCALLABLE_STUB(SEXP, new_LoopIndexList_from_INTEGER_LIST,
                      (SEXP x),
                      (     x)
)

DEFINE_CCALLABLE_STUB(SEXP, new_LoopIndexList,
                      (SEXP list, SEXP partitioning),
                      (     list,      partitioning)
)

DEFINE_CCALLABLE_STUB(SEXP, new_LoopIndexList_from_LIST,
                      (SEXP list),
                      (     list)
)
  
DEFINE_CCALLABLE_STUB(SEXP, new_DotBracket_from_INTEGER,
                      (SEXP x),
                      (     x)
)
  

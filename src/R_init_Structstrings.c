#include "Structstrings.h"

#define CALLMETHOD_DEF(fun, numArgs) {#fun, (DL_FUNC) &fun, numArgs}

#define REGISTER_CCALLABLE(fun) \
R_RegisterCCallable("Structstrings", #fun, (DL_FUNC) &fun)        \
  
static const R_CallMethodDef callMethods[] = {
  /* DotBracketDataFrame.c */
  CALLMETHOD_DEF(new_DotBracketDFrame, 3),
  CALLMETHOD_DEF(new_DotBracketDFrameList, 2),
  CALLMETHOD_DEF(new_DotBracketDFrameList_from_CHARACTER, 2),
  CALLMETHOD_DEF(new_DotBracket_from_INTEGER, 1),
  CALLMETHOD_DEF(new_LoopIndexList_from_CHARACTER_LIST, 2),
  CALLMETHOD_DEF(new_LoopIndexList_from_INTEGER_LIST, 1),
  CALLMETHOD_DEF(new_LoopIndexList_from_LIST, 1),
  CALLMETHOD_DEF(new_LoopIndexList, 2),
  {NULL, NULL, 0}
};

void R_init_Structstrings(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, TRUE);
  return;
}

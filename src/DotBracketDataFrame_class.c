#include "Structstrings.h"

/* dot bracket string to data frame functions */

SEXP new_DotBracketDFrame(SEXP vars, SEXP rownames, SEXP nrows)
{
  SEXP ans;
  PROTECT(ans = new_DataFrame("DotBracketDFrame", 
                              vars,
                              rownames, 
                              nrows));
  UNPROTECT(1);
  return ans;
}

SEXP new_DotBracketDFrameList(SEXP unlistData, SEXP partitioning)
{
  SEXP ans;
  PROTECT(ans = new_CompressedList("CompressedSplitDotBracketDataFrameList",
                                   unlistData,
                                   partitioning));
  UNPROTECT(1);
  return ans;
}

long *get_base_pairing_per_char_pair(R_xlen_t length, 
                                     const char *str,
                                     char bracket_open,
                                     char bracket_close)
{
  long *table, *stack, i,j,hx;
  
  stack = (long *) util_mem_alloc(sizeof(long) * (length + 1));
  table = (long *) util_mem_alloc(sizeof(long) * (length + 2));
  table[0] = length;
  
  for(hx = 0, i = 1; i <= length; i++) {
    if(str[i-1] == bracket_open){
      stack[hx++]=i;
    } else if(str[i-1] == bracket_close){
      j = stack[--hx];
      if (hx<0) {
        error("unbalanced '%s%s' brackets in dot bracket structure",
              bracket_open,
              bracket_close);
        free(stack);
        return NULL;
      }
      table[i]=j;
      table[j]=i;
    } else {
      table[i]= 0;
    }
  }
  free(stack);
  if (hx != 0) {
    error("unbalanced '%s%s' brackets in dot bracket structure",
          bracket_open,
          bracket_close);
    return NULL;
  }
  return(table);
}

SEXP new_DotBracketDFrame_from_CHARACTER(const char *chr, SEXP offset)
{
  R_xlen_t length = (R_xlen_t) strlen(chr);
  if(LENGTH(offset) != length){
    error("offset must be of same length then the input string.");
  }
  char *STRUCTURE_OPEN_CHAR_INIT;
  char *STRUCTURE_CLOSE_CHAR_INIT;
  int STRUCTURE_OPEN_INT_INIT;
  int STRUCTURE_CLOSE_INT_INIT;
  
  SEXP ans, pos, forward, reverse, chrtype_int;
  int *ppos, *pforward, *preverse, *pchrtype_int, *poffset, z, i;
  PROTECT(pos = NEW_INTEGER(length));
  PROTECT(forward = NEW_INTEGER(length));
  PROTECT(reverse = NEW_INTEGER(length));
  PROTECT(chrtype_int = NEW_INTEGER(length));
  ppos = INTEGER(pos);
  pforward = INTEGER(forward);
  preverse = INTEGER(reverse);
  pchrtype_int = INTEGER(chrtype_int);
  poffset = INTEGER(offset);
  
  int j = 0;
  long *table[STRUCTURE_N_CHAR_TYPES];
  for(j = 0; j < STRUCTURE_N_CHAR_TYPES; j++){
    table[j] = get_base_pairing_per_char_pair(length,
                                              chr,
                                              *bracket_types_open[j],
                                              *bracket_types_close[j]);
    table[j][0] = 0;
  }
  
  for (i = 1; i <= length; i++) {
    z = i-1;
    ppos[z] = i - poffset[z];
    pchrtype_int[z] = STRUCTURE_CHR_NULL;
    pforward[z] = 0;
    preverse[z] = 0;
    
    for(j = 0; j < STRUCTURE_N_CHAR_TYPES; j++){
      if(table[j][i] != 0){
        pforward[z] = table[j][table[j][i]] - poffset[z];
        preverse[z] = table[j][i] - poffset[z];
        if( pforward[z] < preverse[z] ){
          pchrtype_int[z] = bracket_int_open[j];
        }
        if( pforward[z] > preverse[z] ){
          pchrtype_int[z] = bracket_int_close[j];
        }
      }
    }
  }
  for(j = 0; j < STRUCTURE_N_CHAR_TYPES; j++){
    free(table[j]);
  }
  
  SEXP list, names;
  PROTECT(list = NEW_LIST(4));
  SET_VECTOR_ELT(list, 0, pos);
  SET_VECTOR_ELT(list, 1, forward);
  SET_VECTOR_ELT(list, 2, reverse);
  SET_VECTOR_ELT(list, 3, chrtype_int);
  
  PROTECT(names = NEW_STRING(4));
  SET_STRING_ELT(names, 0, mkChar("pos"));
  SET_STRING_ELT(names, 1, mkChar("forward"));
  SET_STRING_ELT(names, 2, mkChar("reverse"));
  SET_STRING_ELT(names, 3, mkChar("character"));
  SET_NAMES(list, names);
  PROTECT(ans = new_DotBracketDFrame(list, NULL_USER_OBJECT, ScalarInteger(length)));
  UNPROTECT(7);
  return ans;
}

SEXP construct_offset_from_ends(const char *chr, SEXP ends, SEXP offset)
{
  if(IS_INTEGER(ends) == FALSE){
    error("ends must be 'integer'.");
  }
  R_xlen_t ends_length = xlength(ends);
  R_xlen_t chr_length = (R_xlen_t) strlen(chr);
  if(LENGTH(offset) != chr_length){
    error("offset must be of same length then the input string.");
  }
  
  int *poffset, *pends, start, z;
  poffset = INTEGER(offset);
  pends = INTEGER(ends);
  
  start = 0;
  for(int i = 0; i < ends_length; i++){
    z = pends[i] - start;
    for(int j = 0; j < z; j++){
      poffset[start+j] = start;
    }
    start = pends[i];
  }
  return offset;
}

SEXP new_DotBracketDFrameList_from_CHARACTER(SEXP x, SEXP partitioning)
{
  /* 
   * input check
   */ 
  if (LENGTH(x) != 1){
    error("Input must be of length = 1.");
  }
  if (IS_CHARACTER(x) == FALSE){
    error("Input must be a 'character'.");
  }
  const char *chr;
  SEXP ans, unlistData, ends, offset;
  
  chr = CHAR(asChar(x));
  PROTECT(offset = NEW_INTEGER(strlen(chr)));
  PROTECT(ends = get_PartitioningByEnd_end(partitioning));
  offset = construct_offset_from_ends(chr, ends, offset);
  PROTECT(unlistData = new_DotBracketDFrame_from_CHARACTER(chr, offset));
  PROTECT(ans = new_DotBracketDFrameList(unlistData,partitioning));
  UNPROTECT(4);
  return ans;
}

/* integer list to dot bracket string  */
SEXP get_dot_bracket_from_base_pairing(SEXP x)
{
  /* input checks */
  if(IS_INTEGER(x) == FALSE){
    error("Input elements must be 'integer'.");
  }
  R_xlen_t length = xlength(x);
  if(length == 0){
    error("Input has length = 0.");
  }
  
  SEXP dotbracket;
  char *db;
  char *STRUCTURE_OPEN_CHAR_INIT;
  char *STRUCTURE_CLOSE_CHAR_INIT;
  int *px, i = 0, z = -1, lr[STRUCTURE_N_CHAR_TYPES] = {0};
  px = INTEGER(x);
  db = (char *) util_mem_alloc(sizeof(char) * (length + 2));
  
  if (length > 0) {
    memset(db, '.', length);
    for (i = 0; i < length; i++) {
      if (px[i] > i) {
        if(px[i] > lr[z] || z < 0){
          z++;
          lr[z] = px[i];
        }
        if(z > 1){
          if(px[i] < lr[z-1]){
            --z;
          }
        }
        if(z < 0 || z > STRUCTURE_N_CHAR_TYPES){
          error("To many loop types need. Maximum is currently %d. Current loop %d %d.",
                STRUCTURE_N_CHAR_TYPES,
                z,
                i);
        }
        db[i] = *bracket_types_open[z];
        db[px[i]-1] = *bracket_types_close[z];
      }
    }
  }
  db[i] = '\0';
  
  PROTECT(dotbracket = mkChar(db));
  UNPROTECT(1);
  return dotbracket;
}

SEXP new_DotBracket_from_INTEGER(SEXP x)
{
  /* input checks */
  if(IS_LIST(x) == FALSE){
    error("Input must be 'list'.");
  }
  
  R_xlen_t length = xlength(x);
  SEXP ans = PROTECT(allocVector(STRSXP, length));
  for(int i = 0; i < length; i++){
    SET_STRING_ELT(ans, i, get_dot_bracket_from_base_pairing(VECTOR_ELT(x, i)));
  }
  UNPROTECT(1);
  return ans;
}

/* data frame or dot bracket string to loop id list */
int *get_loopids_from_base_pairing_table(const long *table)
{
  int i, hx, l, nl;
  int length;
  int *stack  = NULL;
  int *loop   = NULL;
  
  length  = table[0];
  stack   = (int *) util_mem_alloc(sizeof(int) * (length + 1));
  loop    = (int *) util_mem_alloc(sizeof(int) * (length + 2));
  hx      = l = nl = 0;
  
  for (i = 1; i <= length; i++) {
    if ((table[i] != 0) && (i < table[i])) {
      nl++;
      l           = nl;
      stack[hx++] = i;
    }
    
    loop[i] = l;
    
    if ((table[i] != 0) && (i > table[i])) {
      --hx;
      if (hx > 0)
        l = loop[stack[hx - 1]];  
      else
        l = 0;                    
      
      if (hx < 0) {
        error("Unbalanced base pairings in input.");
        free(stack);
        return NULL;
      }
    }
  }
  loop[0] = nl;
  free(stack);
  return loop;
}

SEXP new_loopids_from_CHARACTER(SEXP x, int type)
{
  const char *chr;
  long *table;
  int *ids;
  
  if (LENGTH(x) != 1){
    error("zero or more than one input sequence");
  }
  if(type < 0 || type > STRUCTURE_N_CHAR_TYPES){
    error("bracket type is out of range.");
  }
  chr = CHAR(asChar(x));
  R_xlen_t length = (R_xlen_t) strlen(chr);
  char *STRUCTURE_OPEN_CHAR_INIT;
  char *STRUCTURE_CLOSE_CHAR_INIT;
  
  table = get_base_pairing_per_char_pair(length, 
                                         chr, 
                                         *bracket_types_open[type-1],
                                         *bracket_types_close[type-1]);
  ids = get_loopids_from_base_pairing_table(table);
  free(table);
  
  SEXP ans = PROTECT(NEW_INTEGER(length));
  int *pans;
  
  pans = INTEGER(ans);
  int z;
  for (int i = 1; i <= length; i++) {
    z = i-1;
    pans[z] = ids[i];
  }
  free(ids);
  
  UNPROTECT(1);
  return ans;
}

SEXP new_loopids_from_INTEGER(SEXP x)
{
  if(IS_INTEGER(x) == FALSE){
    error("Input must contain values of type 'integer' only.");
  }
  
  long *table;
  int *ids,*px;
  int z;
  R_xlen_t length = xlength(x);
  
  px = INTEGER(x);
  table = (long *) util_mem_alloc(sizeof(long) * (length + 2));
  table[0] = length;
  
  for(int i = 1; i <= length; i++){
    z = i-1;
    table[i] = px[z];
  }
  ids = get_loopids_from_base_pairing_table(table);
  
  SEXP ans = PROTECT(NEW_INTEGER(length));
  int *pans;
  
  pans = INTEGER(ans);
  for (int i = 1; i <= length; i++) {
    z = i-1;
    pans[z] = ids[i];
  }
  free(table);
  free(ids);
  
  UNPROTECT(1);
  return ans;
}

SEXP new_LoopIndexList(SEXP list, SEXP partitioning)
{
  SEXP ans_unlistData, ans;
  /* get unlistData */
  PROTECT(ans_unlistData = R_tryEval(lang2(install("unlist"), list), 
                                     R_GlobalEnv, NULL));
  SET_NAMES(ans_unlistData, R_NilValue);
  /* input check */
  if(IS_INTEGER(ans_unlistData) == FALSE){
    error("Input list must contain integer values only.");
  }
  /* construct LoopIndexList */
  PROTECT(ans = new_CompressedList("LoopIndexList",ans_unlistData,
                                   partitioning));
  UNPROTECT(2);
  return ans;
}

SEXP new_LoopIndexList_from_LIST(SEXP list)
{
  /* get unlistData and partitioningByEnd */
  SEXP ans_breakpoints, ans_partitioning, ans_names;
  int length, j, i, *pans_breakpoints;
  length = LENGTH(list);
  /* get breakpoints for partitioningByEnd */
  PROTECT(ans_breakpoints = NEW_INTEGER(length));
  pans_breakpoints = INTEGER(ans_breakpoints);
  j = 0;
  for (i = 0; i < length; i++) {
    j = j + LENGTH(VECTOR_ELT(list, i));
    pans_breakpoints[i] = j;
  }
  PROTECT(ans_names = GET_NAMES(list));
  PROTECT(ans_partitioning = new_PartitioningByEnd("PartitioningByEnd",
                                                   ans_breakpoints, ans_names));
  UNPROTECT(3);
  return new_LoopIndexList(list, ans_partitioning);
}

SEXP new_LoopIndexList_from_CHARACTER_LIST(SEXP x, SEXP type)
{
  int bracket_type, length, i;
  length = LENGTH(x);
  /* input check */
  if(LENGTH(type) != 1 || IS_INTEGER(type) == FALSE){
    error("'type' must be a single integer value.");
  }
  for(i = 0; i < length; i++){
    if(IS_CHARACTER(VECTOR_ELT(x, i)) == FALSE){
      error("Elements of input list must all be integer values.");
    }
  }
  /* get loop ids */
  bracket_type = asInteger(type);
  SEXP list = PROTECT(allocVector(VECSXP, length));
  for(i = 0; i < length; i++){
    SET_VECTOR_ELT(list, i, new_loopids_from_CHARACTER(VECTOR_ELT(x, i),
                                                       bracket_type));
  }
  namesgets(list, GET_NAMES(x));
  UNPROTECT(1);
  return new_LoopIndexList_from_LIST(list);
}

SEXP new_LoopIndexList_from_INTEGER_LIST(SEXP x)
{
  int length, i;
  length = LENGTH(x);
  /* input check */
  for(i = 0; i < length; i++){
    if(IS_INTEGER(VECTOR_ELT(x, i)) == FALSE){
      error("Elements of input list must all be integer values.");
    }
  }
  /* get loop ids */
  SEXP list = PROTECT(allocVector(VECSXP, length));
  for(i = 0; i < length; i++){
    SET_VECTOR_ELT(list, i, new_loopids_from_INTEGER(VECTOR_ELT(x, i)));
  }
  namesgets(list, GET_NAMES(x));
  UNPROTECT(1);
  return new_LoopIndexList_from_LIST(list);
}

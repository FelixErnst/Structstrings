#include "Structstrings.h"

void * util_mem_alloc(unsigned size)
  {
    void *pointer;
    if ((pointer = (void *)calloc(1, (size_t)size)) == NULL) {
      error("alloc error: requested size: %d\n", size);
    }
    return pointer;
  }
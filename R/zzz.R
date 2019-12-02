
.onLoad <- function(libname, pkgname)
{
  DOTBRACKET_COLORED_LETTERS <<- make_DOTBRACKET_COLORED_LETTERS()
}

.onUnload <- function(libpath)
{
  library.dynam.unload("Structstrings", libpath)
}

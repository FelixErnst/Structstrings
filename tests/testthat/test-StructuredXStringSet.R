context("StructuredXStringSet")
test_that("StructuredXStringSet:",{
  data("dbs", package = "Structstrings", envir = environment())
  data("seq", package = "Structstrings", envir = environment())
  seq <- RNAStringSet(seq)
  sdbs <- StructuredRNAStringSet(unname(seq),dbs)
  expect_s4_class(sdbs,"StructuredRNAStringSet")
  expect_named(sdbs,names(dbs))
  sdbs <- StructuredRNAStringSet(seq,dbs)
  expect_s4_class(sdbs,"StructuredRNAStringSet")
  expect_named(sdbs,names(seq))
  expect_equal(as.character(sdbs),as.character(RNAStringSet(seq)))
  expect_s4_class(dotbracket(sdbs),"DotBracketStringSet")
  # StructuredXStringSet writing and reading
  file <- tempfile()
  writeStructuredXStringSet(sdbs,file)
  sdbs2 <- readStructuredRNAStringSet(file)
  expect_s4_class(sdbs2,"StructuredRNAStringSet")
  expect_equal(sdbs,sdbs2)
  sdbs2 <- readStructuredRNAStringSet(file)
  expect_s4_class(sdbs2,"StructuredRNAStringSet")
  expect_equal(sdbs,sdbs2)
  # slot accessors
  actual <- dotbracket(sdbs)
  expect_s4_class(actual,"DotBracketStringSet")
  sdbs2 <- sdbs
  dotbracket(sdbs2) <- actual
  expect_s4_class(sdbs2,"StructuredRNAStringSet")
  expect_equal(sdbs,sdbs2)
  dotbracket(sdbs2) <- NULL
  expect_s4_class(sdbs2,"RNAStringSet")
  sdbs2 <- sdbs
  dotbracket(sdbs2) <- as.character(actual)
  expect_s4_class(sdbs2,"StructuredRNAStringSet")
  expect_equal(sdbs,sdbs2)
})

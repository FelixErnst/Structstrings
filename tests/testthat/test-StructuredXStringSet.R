context("StructuredXStringSet")
test_that("StructuredXStringSet:",{
  data("dbs", package = "Structstrings")
  data("nseq", package = "Structstrings")
  nseq <- Biostrings::RNAStringSet(nseq)
  sdbs <- StructuredRNAStringSet(unname(nseq),dbs)
  expect_s4_class(sdbs,"StructuredRNAStringSet")
  expect_named(sdbs,names(dbs))
  sdbs <- StructuredRNAStringSet(nseq,dbs)
  expect_s4_class(sdbs,"StructuredRNAStringSet")
  expect_named(sdbs,names(nseq))
  expect_equal(as.character(sdbs),as.character(Biostrings::RNAStringSet(nseq)))
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

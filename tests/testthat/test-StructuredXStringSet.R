context("StructuredXStringSet")
test_that("StructuredXStringSet:",{
  data("dbs", package = "Structstrings")
  data("nseq", package = "Structstrings")
  nseq <- Biostrings::RNAStringSet(nseq)
  sdbs <- StructuredRNAStringSet(unname(nseq),dbs)
  #
  expect_error(StructuredRNAStringSet(unname(nseq),""),
               "when 'structure' is a single string it must be a single")
  expect_error(StructuredRNAStringSet("",dbs),
               "'length\\(structure\\)' must equal 'length\\(x\\)' or 1")
  #
  expect_s4_class(sdbs,"StructuredRNAStringSet")
  expect_named(sdbs,names(dbs))
  sdbs <- StructuredRNAStringSet(nseq,dbs)
  expect_s4_class(sdbs,"StructuredRNAStringSet")
  expect_named(sdbs,names(nseq))
  expect_equal(as.character(sdbs),as.character(Biostrings::RNAStringSet(nseq)))
  expect_s4_class(dotbracket(sdbs),"DotBracketStringSet")
  expect_equal(StructuredRNAStringSet(nseq[1],dbs[1]),sdbs[1])
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
  #
  expect_output(show(sdbs))
  #
  dbdfl <- getBasePairing(sdbs)
  expect_s4_class(dbdfl,"CompressedSplitDotBracketDataFrameList")
  expect_equal(ncol(unlist(dbdfl)),4L)
  dbdfl <- getBasePairing(sdbs, return.sequence = TRUE)
  expect_s4_class(dbdfl,"CompressedSplitDotBracketDataFrameList")
  expect_equal(ncol(unlist(dbdfl)),5L)
  dbdfl <- getBasePairing(sdbs, compress = FALSE, return.sequence = TRUE)
  expect_s4_class(dbdfl,"SimpleSplitDotBracketDataFrameList")
  expect_equal(ncol(unlist(dbdfl)),5L)
  #
  lil <- getLoopIndices(sdbs)
  expect_s4_class(lil,"LoopIndexList")
  expect_named(lil)
})

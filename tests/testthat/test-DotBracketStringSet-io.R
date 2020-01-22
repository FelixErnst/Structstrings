context("DotBracketStringSet input/output")
test_that("DotBracketStringSet input/output:",{
  data("dbs", package = "Structstrings")
  dbs2 <- DotBracketStringSet(getDotBracket(getBasePairing(dbs),TRUE))
  file <- tempfile()
  writeXStringSet(dbs,file)
  dbs2 <- readDotBracketStringSet(file)
  expect_equal(dbs,dbs2)
  writeDotBracketStringSet(dbs,file)
  dbs2 <- readDotBracketStringSet(file)
  expect_equal(dbs,dbs2)
  #
  dbs2 <- convertAnnotation(dbs,1L,2L)
  expect_error(writeDotBracketStringSet(dbs2,file),
               "The dot bracket type '<>' cannot be saved as ")
  #
  file <- tempfile(fileext = ".rda")
  expect_output(saveDotBracketStringSet(dbs,"dbs"))
})

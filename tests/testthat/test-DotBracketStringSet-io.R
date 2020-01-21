context("DotBracketStringSet input/output")
test_that("DotBracketStringSet input/output:",{
  data("dbs", package = "Structstrings")
  dbs2 <- DotBracketStringSet(getDotBracket(getBasePairing(dbs),TRUE))
  file <- tempfile()
  writeXStringSet(dbs,file)
  dbs2 <- readDotBracketStringSet(file)
  expect_equal(dbs,dbs2)
})

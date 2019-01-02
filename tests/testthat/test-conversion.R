context("DotBracketStringSet conversion")
test_that("DotBracketStringSet conversion:",{
  data("dbs", package = "Structstrings", envir = environment())
  dbdfl <- getBasePairing(dbs)
  dbs2 <- getDotBracket(dbdfl)
  expect_equal(as.character(dbs),as.character(dbs2))
  expect_equal(dbs,dbs2)
  loopids <- getLoopIndices(dbs)
  loopids2 <- getLoopIndices(dbdfl)
  expect_equal(loopids,loopids2)
  expect_warning({
    getLoopIndices(dbs, 2L)
  })
  # check equality of converted from getBasePairing to getDotBracket
  dbdf <- unlist(dbdfl)
  dbdf$character <- NULL
  dbdfl2 <- relist(dbdf,dbdfl)
  dbs2 <- getDotBracket(dbdfl2)
  expect_equal(as.character(dbs),as.character(dbs2))
  #
  s <- unique(unlist(strsplit(unlist(as.character(dbs)),"")))
  expect_equal(s,c("(",".",")"))
  # convertAnnotation
  dbs2 <- convertAnnotation(dbs, 1L, 1L)
  s <- unique(unlist(strsplit(unlist(as.character(dbs2)),"")))
  expect_equal(s,c("(",".",")"))
  dbs3 <- convertAnnotation(dbs, 1L, 2L)
  s <- unique(unlist(strsplit(unlist(as.character(dbs3)),"")))
  expect_equal(s,c("<",".",">"))
  dbs4 <- convertAnnotation(dbs, 1L, 3L)
  s <- unique(unlist(strsplit(unlist(as.character(dbs4)),"")))
  expect_equal(s,c("[",".","]"))
  dbs5 <- convertAnnotation(dbs, 1L, 4L)
  s <- unique(unlist(strsplit(unlist(as.character(dbs5)),"")))
  expect_equal(s,c("{",".","}"))
  expect_equal(getLoopIndices(dbs),getLoopIndices(dbs2))
  expect_equal(getLoopIndices(dbs),getLoopIndices(dbs3))
  expect_equal(getLoopIndices(dbs),getLoopIndices(dbs4))
  expect_equal(getLoopIndices(dbs),getLoopIndices(dbs5))
  expect_error(convertAnnotation(dbs, 1L, 5L),
               "'from'/'to' is out of range")
  expect_error(convertAnnotation(dbs, 2L, 5L),
               "'from' dot bracket type was not found in the input")
  expect_error(convertAnnotation(dbs, "aaa", 5L),
               "'from' and 'to' dot bracket types must be single integer")
  # pseudoloop interconversion
  db <- DotBracketString("((((....[[[))))....((((....<<<<...))))]]]....>>>>...")
  dbdf <- getBasePairing(db)
  dbdf$character <- NULL
  db2 <- getDotBracket(dbdf)
  expect_equal(as.character(db2),
               "((((....<<<))))....<<<<....[[[[...>>>>>>>....]]]]...")
})

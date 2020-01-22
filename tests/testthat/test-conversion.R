context("DotBracketStringSet/DotBracketDataFrame conversion")
test_that("DotBracketStringSet/DotBracketDataFrame conversion:",{
  data("dbs", package = "Structstrings")
  #
  dbdfl <- getBasePairing(dbs)
  expect_equivalent(as(dbdfl[[1]],"DotBracketString"),dbs[[1]])
  expect_equal(as(dbdfl[1],"DotBracketStringSet"),dbs[1])
  dbdfl2 <- getBasePairing(dbs,compress = FALSE)
  expect_equal(as(dbdfl2[1],"DotBracketStringSet"),dbs[1])
  expect_equal(as(DotBracketDataFrameList(dbdfl2[[1]]),"DotBracketStringSet"),
               unname(dbs[1]))
  expect_equal(as(as(dbdfl2[1],"DotBracketDataFrameList"),"DotBracketStringSet"),
               dbs[1])
  #
  expect_equal(as(dbs[[1]],"DotBracketDataFrame"),dbdfl[[1]])
  expect_s4_class(as(dbs[1],"DotBracketDataFrameList"),"DotBracketDFrameList")
  expect_equal(as(dbs[1],"SimpleSplitDotBracketDataFrameList"),dbdfl2[1])
  expect_equal(as(dbs[1],"CompressedSplitDotBracketDataFrameList"),dbdfl[1])
  #
  dbs2 <- getDotBracket(dbdfl)
  expect_equal(as.character(dbs),as.character(dbs2))
  expect_equal(dbs,dbs2)
  expect_equivalent(getDotBracket(dbdfl[[1]]),dbs[[1]])
  expect_equal(getDotBracket(as(dbdfl[1],
                                "SimpleSplitDotBracketDataFrameList")),
               dbs[1])
  expect_equal(getDotBracket(as(dbdfl[1], "DotBracketDataFrameList")),
               dbs[1])
  
  expect_equivalent(as(dbdfl[[1]],"DotBracketString"),dbs[[1]])
  expect_equal(as(as(dbdfl[1],"SimpleSplitDotBracketDataFrameList"),
                  "DotBracketStringSet"),
               dbs[1])
  expect_equal(as(as(dbdfl[1], "DotBracketDataFrameList"),
                  "DotBracketStringSet"),
               dbs[1])
  #
  loopids <- getLoopIndices(dbs)
  loopids2 <- getLoopIndices(dbdfl)
  expect_equal(loopids,loopids2)
  expect_warning({
    getLoopIndices(dbs, 2L)
  })
  actual <- getLoopIndices(dbs[[1]])
  expect_type(actual,"integer")
  expect_equal(actual,loopids[[1]])
  expect_equal(getLoopIndices(as(dbdfl[1], "DotBracketDataFrameList")),
               loopids[1])
  expect_equal(getLoopIndices(as(dbdfl[1], "SimpleSplitDotBracketDataFrameList")),
               loopids[1])
  expect_equal(getLoopIndices(as(dbdfl[1], "CompressedSplitDotBracketDataFrameList")),
               loopids[1])
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
  expect_equivalent(convertAnnotation(dbs[[1]], 1L, 1L),dbs[[1]])
  db <- convertAnnotation(dbs[[1]], 1L, 2L)
  s <- unique(unlist(strsplit(as.character(db),"")))
  expect_equal(s,c("<",".",">"))
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
  dbsl <- DotBracketStringSetList(dbs,dbs)
  expect_equivalent(convertAnnotation(dbsl[1], 1L, 1L),dbsl[1])
  dbsl2 <- convertAnnotation(dbsl, 1L, 2L)
  s <- unique(unlist(strsplit(unlist(as.character(unlist(dbsl2))),"")))
  expect_equal(s,c("<",".",">"))
  # pseudoloop interconversion
  db <- DotBracketString("((((....[[[))))....((((....<<<<...))))]]]....>>>>...")
  dbdf <- getBasePairing(db)
  dbdf$character <- NULL
  db2 <- getDotBracket(dbdf)
  expect_equal(as.character(db2),
               "((((....<<<))))....<<<<....[[[[...>>>>>>>....]]]]...")
})

context("DotBracketStringSet type conversion")
test_that("DotBracketStringSet type conversion:",{
  data("dbs", package = "Structstrings")
  actual <- xvcopy(dbs[[1]])
  bs <- as(dbs,"BStringSet")
  expect_equal(dbs,as(bs,"DotBracketStringSet"))
  expect_equivalent(actual,as(bs[[1]],"DotBracketString"))
  il <- as(dbs,"IntegerList")
  expect_equal(dbs,as(il,"DotBracketStringSet"))
  expect_equivalent(dbs[1],as(il[1],"DotBracketStringSet"))
  expect_equivalent(actual,as(il[[1]],"DotBracketString"))
  cl <- as.character(dbs)
  expect_equal(dbs,as(cl,"DotBracketStringSet"))
  expect_equivalent(dbs[1],as(cl[1],"DotBracketStringSet"))
  expect_equivalent(actual,as(cl[1],"DotBracketString"))
})

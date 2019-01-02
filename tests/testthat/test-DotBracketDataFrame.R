context("DotBracketDataFrame")
test_that("DotBracketDataFrame:",{
  df <- DataFrame(pos = c(1,2,3,4,5,6),
                  forward = c(6,5,0,0,2,1),
                  reverse = c(1,2,0,0,5,6))
  dbdf <- as(df,"DotBracketDataFrame")
  expect_equal(DotBracketDataFrame(df),dbdf)
  expect_s4_class(dbdf,"DotBracketDataFrame")
  # subsetting is a bit dangerous
  expect_error({validObject(dbdf[1,])})
  #
  dfl <- DataFrameList(df,df)
  expect_s4_class(dfl,"SimpleDataFrameList")
  dbdfl1 <- as(dfl,"DotBracketDataFrameList")
  expect_s4_class(dbdfl1,"DotBracketDataFrameList")
  #
  dfl <- SplitDataFrameList(df,df, compress = FALSE)
  expect_s4_class(dfl,"SimpleSplitDataFrameList")
  dbdfl2 <- as(dfl,"SplitDotBracketDataFrameList")
  expect_s4_class(dbdfl2,"SplitDotBracketDataFrameList")
  dbdfl3 <- as(dfl,"DotBracketDataFrameList")
  expect_s4_class(dbdfl3,"DotBracketDataFrameList")
  #
  dfl <- SplitDataFrameList(df,df, compress = TRUE)
  expect_s4_class(dfl,"CompressedSplitDataFrameList")
  dbdfl4 <- as(dfl,"CompressedSplitDotBracketDataFrameList")
  expect_s4_class(dbdfl4,"CompressedSplitDotBracketDataFrameList")
  #
  dfl <- list(df,df)
  expect_type(dfl,"list")
  dbdfl5 <- as(dfl,"DotBracketDataFrameList")
  expect_s4_class(dbdfl5,"DotBracketDataFrameList")
  dbdfl6 <- as(dfl,"SplitDotBracketDataFrameList")
  expect_s4_class(dbdfl6,"SplitDotBracketDataFrameList")
  dbdfl7 <- as(dfl,"CompressedSplitDotBracketDataFrameList")
  expect_s4_class(dbdfl7,"CompressedSplitDotBracketDataFrameList")
  expect_equal(dbdfl7,dbdfl4)
  expect_equal(dbdfl3,dbdfl1)
  expect_equal(dbdfl3,dbdfl5)
  expect_equal(dbdfl6,dbdfl2)
})

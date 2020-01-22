context("DotBracketDataFrame")
test_that("DotBracketDataFrame:",{
  df <- DataFrame(pos = c(1,2,3,4,5,6),
                  forward = c(6,5,0,0,2,1),
                  reverse = c(1,2,0,0,5,6))
  df2 <- DataFrame(pos2 = c(1,2,3,4,5,6),
                   forward = c(-6,5,0,0,2,1),
                   reverse = c(1,2,0,0,5,6))
  #
  actual <- Structstrings:::.adjust_DotBracketDataFrame_col_types(df)
  expect_type(actual$pos,"integer")
  expect_type(actual$forward,"integer")
  expect_type(actual$reverse,"integer")
  #
  expect_error(as(df[,c(1,2)],"DotBracketDataFrame"),
               'invalid class "DotBracketDataFrame" object')
  expect_error(as(df2,"DotBracketDataFrame"),
               'invalid class "DotBracketDataFrame" object')
  df2 <- DataFrame(pos = c(1,2,3,4,5,6),
                   forward = c(-6,5,0,0,2,1),
                   reverse = c(1,2,0,0,5,6))
  expect_error(as(df2,"DotBracketDataFrame"),
               'invalid class "DotBracketDataFrame" object')
  df2 <- df
  class(df2) <- "DotBracketDataFrame"
  expect_error(validObject(df2),
               'invalid class "DotBracketDataFrame" object: The types')
  df2 <- list(c(1,2,3,4,5,6),
              c(6,5,0,0,2,1),
              c(1,2,0,0,5,6))
  actual <- Structstrings:::.rename_unnamed_args(df2)
  expect_type(actual,"list")
  expect_named(actual,c("pos","forward","reverse"))
  actual <- DotBracketDataFrame(df2)
  expect_s4_class(actual,"DotBracketDataFrame")
  expect_equal(actual,DBDF(df2))
  rm(df2)
  #
  dbdf <- as(df,"DotBracketDataFrame")
  expect_equal(DotBracketDataFrame(df),dbdf)
  expect_s4_class(dbdf,"DotBracketDataFrame")
  # subsetting is a bit dangerous
  expect_error({validObject(dbdf[1,])})
  #
  dfl <- DataFrameList(df,df)
  expect_s4_class(dfl,"SimpleDataFrameList")
  dbdfl1 <- DotBracketDataFrameList(df,df)
  expect_equal(dbdfl1,DBDFL(df,df))
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
  dbdfl4 <- SplitDotBracketDataFrameList(df,df)
  expect_equal(dbdfl4,SDBDFL(df,df))
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
  #
  dbdfl8 <- dbdfl4
  cn <- IRanges::CharacterList(c("pos","forward","reverse"),
                               c("pos","forward","reverse"))
  expect_equal(dbdfl4,dbdfl8)
  cn <- IRanges::CharacterList(c("pos","forward","reverse2"),
                               c("pos","forward","reverse2"))
  expect_error({colnames(dbdfl8) <- cn},
               'invalid class "CompressedSplitDotBracketDataFrameList" object')
})

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
               'invalid class "DotBracketDFrame" object: At least 3')
  expect_error(as(df2,"DotBracketDataFrame"),
               'invalid class "DotBracketDFrame" object: At least 3')
  df2 <- DataFrame(pos = c(1,2,3,4,5,6),
                   forward = c(-6,5,0,0,2,1),
                   reverse = c(1,2,0,0,5,6))
  expect_error(as(df2,"DotBracketDataFrame"),
               'invalid class "DotBracketDFrame" object: Unmatched')
  df2 <- df
  class(df2) <- "DotBracketDFrame"
  expect_error(validObject(df2),
               'invalid class "DotBracketDFrame" object: The types')
  df2 <- DataFrame(pos = c(1,2,3,4,5,6),
                   forward = c(6,5,0,0,2,1),
                   reverse = c(1,2,0,0,5,6),
                   character = c(1,2,3,4,5,6))
  class(df2) <- "DotBracketDFrame"
  expect_error(validObject(df2),
               'invalid class "DotBracketDFrame" object: The types')
  df2 <- DataFrame(pos = c(1,2,3,4,5,6),
                   forward = c(6,5,0,0,2,1),
                   reverse = c(1,2,0,0,5,6),
                   character = c("(","(",".",".",")",")"),
                   base = c(1,2,0,0,5,6))
  class(df2) <- "DotBracketDFrame"
  expect_error(validObject(df2),
               'invalid class "DotBracketDFrame" object: The types')
  df2 <- list(c(1,2,3,4,5,6),
              c(6,5,0,0,2,1),
              c(1,2,0,0,5,6))
  #
  actual <- Structstrings:::.rename_unnamed_args(df2)
  expect_type(actual,"list")
  expect_named(actual,c("pos","forward","reverse"))
  df2 <- DataFrame(pos = c(1,2,3,4,5,6),
                  forward = c(6,5,0,0,2,1),
                  reverse = c(1,2,0,0,5,6),
                  pos2 = c(1,2,3,4,5,6),
                  forward2 = c(6,5,0,0,2,1),
                  reverse2 = c(1,2,0,0,5,6))
  expect_equal(Structstrings:::.adjust_DotBracketDataFrame_col_types(df2),
               df)
  df2 <- as.list(df2)
  names(df2) <- NULL
  expect_equal(names(Structstrings:::.rename_unnamed_args(df2[c(1,2,3)])),
               colnames(df))
  #
  expect_error(DotBracketDataFrame(),
               'invalid class "DotBracketDFrame" object: At least')
  #
  actual <- DotBracketDataFrame(df2[c(1,2,3)])
  expect_s4_class(actual,"DotBracketDataFrame")
  expect_equal(actual,DBDF(df2[c(1,2,3)]))
  expect_error(DotBracketDataFrame(df2),
               "Input could not be courced to a meaningful")
  expect_error(DBDF(df2),
               "Input could not be courced to a meaningful")
  rm(df2)
  #
  dbdf <- as(df,"DotBracketDataFrame")
  expect_equal(DotBracketDataFrame(df),dbdf)
  expect_s4_class(dbdf,"DotBracketDataFrame")
  # subsetting is a bit dangerous
  expect_error({validObject(dbdf[1,,drop=FALSE])})
  #
  dfl <- DataFrameList(df,df)
  expect_s4_class(dfl,"SimpleDataFrameList")
  dbdfl1 <- DotBracketDataFrameList(df,df)
  expect_equal(dbdfl1,DBDFL(df,df))
  expect_true(validObject(dbdfl1))
  dbdfl1 <- as(dfl,"DotBracketDataFrameList")
  expect_s4_class(dbdfl1,"DotBracketDataFrameList")
  #
  dfl <- SplitDataFrameList(df,df, compress = FALSE)
  expect_s4_class(dfl,"SimpleSplitDataFrameList")
  dbdfl2 <- as(dfl,"SimpleSplitDotBracketDataFrameList")
  expect_s4_class(dbdfl2,"SimpleSplitDotBracketDataFrameList")
  dbdfl3 <- as(dfl,"DotBracketDataFrameList")
  expect_s4_class(dbdfl3,"DotBracketDataFrameList")
  #
  dfl <- SplitDataFrameList(df,df, compress = TRUE)
  expect_s4_class(dfl,"CompressedSplitDataFrameList")
  dbdfl4 <- as(dfl,"CompressedSplitDotBracketDataFrameList")
  expect_s4_class(dbdfl4,"CompressedSplitDotBracketDataFrameList")
  expect_equal(dbdfl4,DotBracketDataFrame(df,df))
  expect_true(validObject(dbdfl4))
  dbdfl4 <- SplitDotBracketDataFrameList(df,df)
  expect_equal(dbdfl4,SDBDFL(df,df))
  #
  dfl <- list(df,df)
  expect_type(dfl,"list")
  dbdfl5 <- as(dfl,"DotBracketDataFrameList")
  expect_s4_class(dbdfl5,"DotBracketDataFrameList")
  dbdfl6 <- as(dfl,"SimpleSplitDotBracketDataFrameList")
  expect_s4_class(dbdfl6,"SimpleSplitDotBracketDataFrameList")
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
  colnames(dbdfl8) <- cn
  expect_equal(dbdfl4,dbdfl8)
  cn <- IRanges::CharacterList(c("pos","forward","reverse2"),
                               c("pos","forward","reverse2"))
  expect_error({colnames(dbdfl8) <- cn},
               'invalid class "CompressedSplitDotBracketDFrameList" object')
  expect_error(DotBracketDataFrame(c(as.list(dbdfl4),list(x = TRUE))),
               "Mixed inputs. Use either vectors per column or a DataFrame")
  #
  
})

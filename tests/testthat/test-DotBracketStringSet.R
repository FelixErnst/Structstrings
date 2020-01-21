context("DotBracket")
test_that("DotBracket:",{
  data("dbs", package = "Structstrings")
  # DotBracketString
  str <- as.character(dbs)
  db <- DotBracketString(str[1])
  expect_s4_class(db,class = "DotBracketString")
  expect_equal(unname(str[1]),as.character(db))
  b <- BString(db)
  expect_s4_class(b,class = "BString")
  expect_equal(as.character(b),as.character(db))
  expect_equal(as(b,"DotBracketString"),db)
  i <- as.integer(db)
  expect_type(i,"integer")
  expect_true(all(unique(i) %in% Structstrings::DOTBRACKET_CHAR_VALUES))
  expect_equal(as(i,"DotBracketString"),db)
  # DotBracketStringSet
  dbs <- DotBracketStringSet(str)
  dbs2 <- DBS(str)
  expect_equal(dbs,dbs2)
  expect_equal(as.character(dbs),str)
  bs <- BStringSet(dbs)
  expect_s4_class(bs,class = "BStringSet")
  expect_equal(as.character(bs),as.character(dbs))
  expect_equal(as(bs,"DotBracketStringSet"),dbs)
  expect_equal(as(b,"DotBracketStringSet")[[1]],as(dbs[[1]],"DotBracketStringSet")[[1]])
  expect_equal(as(i,"DotBracketStringSet")[[1]],as(dbs[[1]],"DotBracketStringSet")[[1]])
  il <- IntegerList(dbs)
  expect_s4_class(il,class = "IntegerList")
  expect_equal(as(il,"DotBracketStringSet"),dbs)
  expect_equal(il,as(dbs,"IntegerList"))
  # DotBracketStringSetList
  dbsl <- DotBracketStringSetList(dbs,dbs)
  expect_s4_class(dbsl,class = "DotBracketStringSetList")
})
context("DotBracket errors")
test_that("DotBracket errors:",{
  data("dbs", package = "Structstrings")
  expect_error(expect_message({
    DotBracketString("abcd")
  },"Invalid character"))
  expect_error(expect_message({
    DotBracketStringSet(c("abcd","abcd"))
  },"Invalid character"))
  expect_error(expect_message({
    DotBracketStringSetList(DotBracketStringSet(c("abcd","abcd")),
                            DotBracketStringSet(c("abcd","abcd")))
  },"Invalid character"))
  
})

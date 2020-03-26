context("DotBracketString")
test_that("DotBracketString:",{
  data("dbs", package = "Structstrings")
  # DotBracketString
  str <- as.character(dbs)
  db <- DotBracketString(str[1])
  expect_s4_class(db,class = "DotBracketString")
  expect_equal(unname(str[1]),as.character(db))
  expect_equal(db,DB(str[1]))
  expect_true(validObject(db))
  b <- BString(db)
  expect_s4_class(b,class = "BString")
  expect_equal(as.character(b),as.character(db))
  expect_equal(as(b,"DotBracketString"),db)
  i <- as.integer(db)
  expect_type(i,"integer")
  expect_true(all(unique(i) %in% Structstrings::DOTBRACKET_CHAR_VALUES))
  expect_equal(as(i,"DotBracketString"),db)
  expect_s4_class(as(integer(),"DotBracketString"),
                  "DotBracketString")
  expect_output(show(db))
  #
  expect_equal(alphabet(db),
               c("(",")",".","<",">","[","]","{","}"))
  expect_equal(encoding(db),
               c(40L,41L,46L,60L,62L,91L,93L,123L,125L))
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
  expect_equal(unname(dbs[1]),DotBracketStringSet(bs[[1]]))
  expect_output(show(dbs))
  expect_type(showAsCell(dbs),"character")
  il <- IntegerList(dbs)
  expect_s4_class(il,class = "IntegerList")
  expect_equal(as(il,"DotBracketStringSet"),dbs)
  expect_equal(il,as(dbs,"IntegerList"))
  expect_equal(DotBracketStringSet(),as(integer(0),"DotBracketStringSet"))
  # DotBracketStringSetList
  dbsl <- DotBracketStringSetList(dbs,dbs)
  expect_s4_class(dbsl,class = "DotBracketStringSetList")
  expect_equal(dbsl,DBSL(dbs,dbs))
  ##############################################################################
  # windows
  expect_equal(windows(dbs),dbs)
  expect_error(windows(dbs,1,2),'invalid class')
  # subseq
  expect_equal(subseq(dbs),dbs)
  expect_error(threebands(dbs,2,3),'invalid class')
  actual <- threebands(dbs)
  expect_equal(actual$middle,unname(dbs))
  expect_error(subseq(dbs,1,2),'invalid class')
  expect_error(subseq(dbs[[1]],1,2),'invalid class')
  #
  expect_null(Structstrings:::.check_for_invalid_db_letters(list(str[1]),
                                                            DOTBRACKET_ALPHABET))
  #
  expect_equal(as.character(DotBracketString(">>>>....<<<<")),
               "<<<<....>>>>")
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
  expect_output(expect_error(Structstrings:::.check_for_invalid_db_values("a",
                                                                          DOTBRACKET_CHAR_VALUES),
                             "Invalid values"))
})

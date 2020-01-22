context("LoopIndexList")
test_that("LoopIndexList:",{
  #
  expect_error(LoopIndexList(c(1L,2L,1L)),
               'invalid class "LoopIndexList" object:')
  #
  lil <- LoopIndexList(c(1L,2L,2L,1L),c(1L,2L,2L,1L))
  expect_s4_class(lil,"LoopIndexList")
  il <- as(lil,"IntegerList")
  expect_equal(lil,as(il,"LoopIndexList"))
  sil <- as(il,"SimpleIntegerList")
  expect_equal(lil,as(sil,"LoopIndexList"))
  expect_true(validObject(lil))
})

context("LINECLEAN")

test_that('removes comments',{
  expect_equal(LINECLEAN('The text after the comments will be removed --comment'),
               'The text after the comments will be removed ')
})

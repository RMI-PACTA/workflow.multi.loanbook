test_that("check_and_prepare_by_group() returns expected value", {
  expect_identical(check_and_prepare_by_group(by_group = NULL), NULL)
  expect_identical(check_and_prepare_by_group(by_group = "NULL"), NULL)
  expect_identical(check_and_prepare_by_group(by_group = "foo"), "foo")
  expect_identical(check_and_prepare_by_group(by_group = "foo, bar"), c("foo", "bar"))
})

test_that("check_and_prepare_by_group() returns error when passed wrong type of input", {
  expect_condition(check_and_prepare_by_group(by_group = 1L))
  expect_condition(check_and_prepare_by_group(by_group = 1))
  expect_condition(check_and_prepare_by_group(by_group = 1.1))
  expect_condition(check_and_prepare_by_group(by_group = list(a = 1)))
  expect_condition(check_and_prepare_by_group(by_group = data.frame(a = 1)))
})

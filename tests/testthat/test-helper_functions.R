test_that("check_and_prepare_by_group() returns expected value", {
  expect_identical(check_and_prepare_by_group(by_group = NULL), NULL)
  expect_identical(check_and_prepare_by_group(by_group = "NULL"), NULL)
  expect_identical(check_and_prepare_by_group(by_group = "foo"), "foo")
  expect_identical(check_and_prepare_by_group(by_group = "foo, bar"), "foo, bar")
})

test_that("check_and_prepare_by_group() returns error when passed wrong type of input", {
  expect_condition(check_and_prepare_by_group(by_group = 1L))
  expect_condition(check_and_prepare_by_group(by_group = 1))
  expect_condition(check_and_prepare_by_group(by_group = 1.1))
  expect_condition(check_and_prepare_by_group(by_group = list(a = 1)))
  expect_condition(check_and_prepare_by_group(by_group = data.frame(a = 1)))
})

test_that("check_and_prepare_by_group() returns error when input not NULL or of length 1", {
  expect_no_condition(check_and_prepare_by_group(by_group = "foo"))
  expect_no_condition(check_and_prepare_by_group(by_group = c("foo")))
  expect_no_condition(check_and_prepare_by_group(by_group = "foo, bar"))
  expect_no_condition(check_and_prepare_by_group(by_group = "foo, bar, baz"))
  expect_no_condition(check_and_prepare_by_group(by_group = NULL))

  expect_condition(check_and_prepare_by_group(by_group = c("foo", "bar")))
  expect_condition(check_and_prepare_by_group(by_group = c("foo", "bar", "baz")))
})

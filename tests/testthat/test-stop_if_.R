test_that("stop_if_not_inherits()", {
  expect_no_condition(stop_if_not_inherits(1L, "integer"))
  expect_no_condition(stop_if_not_inherits(1, "numeric"))
  expect_no_condition(stop_if_not_inherits(1.1, "numeric"))
  expect_no_condition(stop_if_not_inherits("A", "character"))
  expect_no_condition(stop_if_not_inherits(list(a = 1), "list"))
  expect_no_condition(stop_if_not_inherits(data.frame(a = 1), "data.frame"))

  expect_condition(stop_if_not_inherits(1, "integer"))
  expect_condition(stop_if_not_inherits(1.1, "integer"))
  expect_condition(stop_if_not_inherits("A", "integer"))
  expect_condition(stop_if_not_inherits(list(a = 1), "integer"))
  expect_condition(stop_if_not_inherits(data.frame(a = 1), "integer"))

  expect_condition(stop_if_not_inherits(1L, "numeric"))
  expect_condition(stop_if_not_inherits("A", "numeric"))
  expect_condition(stop_if_not_inherits(list(a = 1), "numeric"))
  expect_condition(stop_if_not_inherits(data.frame(a = 1), "numeric"))

  expect_condition(stop_if_not_inherits(1L, "character"))
  expect_condition(stop_if_not_inherits(1, "character"))
  expect_condition(stop_if_not_inherits(1.1, "character"))
  expect_condition(stop_if_not_inherits(list(a = 1), "character"))
  expect_condition(stop_if_not_inherits(data.frame(a = 1), "character"))

  expect_condition(stop_if_not_inherits(1L, "list"))
  expect_condition(stop_if_not_inherits(1, "list"))
  expect_condition(stop_if_not_inherits(1.1, "list"))
  expect_condition(stop_if_not_inherits("A", "list"))
  expect_condition(stop_if_not_inherits(data.frame(a = 1), "list"))

  expect_condition(stop_if_not_inherits(1L, "data.frame"))
  expect_condition(stop_if_not_inherits(1, "data.frame"))
  expect_condition(stop_if_not_inherits(1.1, "data.frame"))
  expect_condition(stop_if_not_inherits("A", "data.frame"))
  expect_condition(stop_if_not_inherits(list(a = 1), "data.frame"))
})


test_that("stop_if_not_length()", {
  expect_no_condition(stop_if_not_length(NA, 1))
  expect_no_condition(stop_if_not_length(numeric(0), 0))
  expect_no_condition(stop_if_not_length(1, 1))
  expect_no_condition(stop_if_not_length(1:2, 2))
  expect_no_condition(stop_if_not_length(character(0), 0))
  expect_no_condition(stop_if_not_length("A", 1))
  expect_no_condition(stop_if_not_length(c("A", "B"), 2))
  expect_no_condition(stop_if_not_length(list(), 0))
  expect_no_condition(stop_if_not_length(list(a = 1), 1))
  expect_no_condition(stop_if_not_length(list(a = 1, b = 2), 2))

  expect_condition(stop_if_not_length(NULL, 1))
  expect_condition(stop_if_not_length(numeric(0), 1))
  expect_condition(stop_if_not_length(1, 2))
  expect_condition(stop_if_not_length(1:2, 1))
  expect_condition(stop_if_not_length(character(0), 1))
  expect_condition(stop_if_not_length("A", 2))
  expect_condition(stop_if_not_length(c("A", "B"), 1))
  expect_condition(stop_if_not_length(list(), 1))
  expect_condition(stop_if_not_length(list(a = 1), 2))
  expect_condition(stop_if_not_length(list(a = 1, b = 2), 1))
})


test_that("stop_if_dir_not_found()", {
  tmp_dir <- tempdir()
  expect_no_condition(stop_if_dir_not_found(tmp_dir))
  expect_condition(stop_if_dir_not_found(file.path(tmp_dir, "XYZ")))
})


test_that("stop_if_file_not_found()", {
  tmp_file <- tempfile()
  file.create(tmp_file)
  expect_no_condition(stop_if_file_not_found(tmp_file))
  expect_condition(stop_if_file_not_found(file.path(".", tmp_file)))
  unlink(tmp_file)
})


test_that("stop_if_sheet_not_found()", {
  tmp_file <- tempfile()
  writexl::write_xlsx(list("test" = data.frame(a = 1)), tmp_file)
  expect_no_condition(stop_if_sheet_not_found("test", tmp_file))
  expect_condition(stop_if_sheet_not_found("xyz", tmp_file))
  unlink(tmp_file)
})


test_that("stop_if_not_expected_columns()", {
  data <- data.frame(a = 1, b = 2, c = 3)
  expect_no_condition(stop_if_not_expected_columns(data, c("a")))
  expect_no_condition(stop_if_not_expected_columns(data, c("b")))
  expect_no_condition(stop_if_not_expected_columns(data, c("c")))
  expect_no_condition(stop_if_not_expected_columns(data, c("a", "b")))
  expect_no_condition(stop_if_not_expected_columns(data, c("b", "c")))
  expect_no_condition(stop_if_not_expected_columns(data, c("a", "b", "c")))
  expect_no_condition(stop_if_not_expected_columns(data, c("b", "c", "a")))

  expect_condition(stop_if_not_expected_columns(data, c("x")))
  expect_condition(stop_if_not_expected_columns(data, c("x", "y", "z")))
  expect_condition(stop_if_not_expected_columns(data, c("a", "x")))
})

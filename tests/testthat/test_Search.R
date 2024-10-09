# Tests for the Search function
test_that("Input validation works", {
  # Test for missing AppId
  expect_error(
    Search(AppKey = "ff3c0f08-e66e-4189-971b-43094327265b", pageNumber = 1, orderBy = "Relevance", query = "test"),
    "AppId must be specified"
  )

  # Test for non-numeric AppId
  expect_error(
    Search(AppId = "5206", AppKey = "ff3c0f08-e66e-4189-971b-43094327265b", pageNumber = 1, orderBy = "Relevance", query = "test"),
    "AppId must be numeric"
  )

  # Test for missing AppKey
  expect_error(
    Search(AppId = 5206, pageNumber = 1, orderBy = "Relevance", query = "test"),
    "AppKey must be specified"
  )

  # Test for non-character AppKey
  expect_error(
    Search(AppId = 5206, AppKey = 0455, pageNumber = 1, orderBy = "Relevance", query = "test"),
    "AppId must be character"
  )

  # Test for missing pageNumber
  expect_error(
    Search(AppId = 5206, AppKey = "ff3c0f08-e66e-4189-971b-43094327265b", orderBy = "Relevance", query = "test"),
    "pageNumber must be specified"
  )

  # Test for non-numeric pageNumber
  expect_error(
    Search(AppId = 5206, AppKey = "ff3c0f08-e66e-4189-971b-43094327265b", pageNumber = "one", orderBy = "Relevance", query = "test"),
    "pageNumber must be numeric"
  )

  # Test for non-scalar pageNumber
  expect_error(
    Search(AppId = 5206, AppKey = "ff3c0f08-e66e-4189-971b-43094327265b", pageNumber = c(1, 2), orderBy = "Relevance", query = "test"),
    "pageNumber must be scalar"
  )

  # Test for missing query
  expect_error(
    Search(AppId = 5206, AppKey = "ff3c0f08-e66e-4189-971b-43094327265b", pageNumber = 1, orderBy = "Relevance"),
    "query must be specified"
  )

  # Test for non-character query
  expect_error(
    Search(AppId = 5206, AppKey = "ff3c0f08-e66e-4189-971b-43094327265b", pageNumber = 1, orderBy = "Relevance", query = 123),
    "query must be character"
  )

  # Test for missing orderBy
  expect_error(
    Search(AppId = 5206, AppKey = "ff3c0f08-e66e-4189-971b-43094327265b", pageNumber = 1, query = "test"),
    "orderBy must be specified"
  )

  # Test for invalid orderBy value
  expect_error(
    Search(AppId = 5206, AppKey = "ff3c0f08-e66e-4189-971b-43094327265b", pageNumber = 1, orderBy = "string", query = "test"),
    "orderBy can be only Relevance, PriceAscending or PriceDescending"
  )
})

test_that("Search function returns valid results", {
  result <- Search(AppId = 5206, AppKey = "ff3c0f08-e66e-4189-971b-43094327265b", pageNumber = 1, orderBy = "Relevance", query = "sdfdgergergtrty")
  expect_type(result, "list")
  expect_s3_class(result$df, "data.frame")
  expect_equal(result$total_pages, 0)
  expect_equal(result$df$ShortDescription, "Nothing found!")
})

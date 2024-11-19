# Tests for the Search function
test_that("Input validation works", {
  # Test for missing AppId
  expect_error(
    Search(AppKey = "e59b000c-1077-4427-be3f-e2e6f1bbfe35", orderBy = "Relevance", query = "test"),
    "AppId must be specified"
  )

  # Test for non-numeric AppId
  expect_error(
    Search(AppId = "5229", AppKey = "e59b000c-1077-4427-be3f-e2e6f1bbfe35", orderBy = "Relevance", query = "test"),
    "AppId must be numeric"
  )

  # Test for missing AppKey
  expect_error(
    Search(AppId = 5229, orderBy = "Relevance", query = "test"),
    "AppKey must be specified"
  )

  # Test for non-character AppKey
  expect_error(
    Search(AppId = 5229, AppKey = 0455, orderBy = "Relevance", query = "test"),
    "AppId must be character"
  )

  # Test for missing query
  expect_error(
    Search(AppId = 5229, AppKey = "e59b000c-1077-4427-be3f-e2e6f1bbfe35", orderBy = "Relevance"),
    "query must be specified"
  )

  # Test for non-character query
  expect_error(
    Search(AppId = 5229, AppKey = "e59b000c-1077-4427-be3f-e2e6f1bbfe35", orderBy = "Relevance", query = 123),
    "query must be character"
  )

  # Test for missing orderBy
  expect_error(
    Search(AppId = 5229, AppKey = "e59b000c-1077-4427-be3f-e2e6f1bbfe35", query = "test"),
    "orderBy must be specified"
  )

  # Test for invalid orderBy value
  expect_error(
    Search(AppId = 5229, AppKey = "e59b000c-1077-4427-be3f-e2e6f1bbfe35", orderBy = "string", query = "test"),
    "orderBy can be only Relevance, PriceAscending or PriceDescending"
  )
})

test_that("Search function returns valid results", {
  result <- Search(AppId = 5229, AppKey = "e59b000c-1077-4427-be3f-e2e6f1bbfe35", orderBy = "Relevance", query = "sdfdgergergtrty")
  expect_s3_class(result, "data.frame")
  expect_equal(result$ShortDescription, "Nothing found!")
})

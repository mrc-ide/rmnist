context("rmnist")

test_that("fetch", {
  if (!has_images(NULL)) {
    expect_message(download_mnist(), "Downloading MNIST")
  }
  expect_silent(download_mnist())
  expect_message(download_mnist(TRUE), "MNIST images already found")
})

test_that("data", {
  d <- load_mnist(FALSE)
  expect_is(d, "mnist")
  expect_equal(sort(names(d)), sort(c("label", "data")))

  expect_is(d$label, "integer")
  expect_equal(range(d$label), c(0L, 9L))

  expect_is(d$data, "array")
  expect_equal(dim(d$data), c(28L, 28L, 10000L))
})

test_that("s3", {
  d <- load_mnist(FALSE)
  expect_equal(length(d), 10000L)

  dsub <- d[1:3]
  expect_equal(length(dsub), 3L)

  expect_output(print(dsub), "<mnist object", fixed = TRUE)
  expect_equal(as.integer(dsub), dsub$label)

  digit <- dsub[[2]]
  expect_equal(digit, d[[2]])
  expect_equal(attr(digit, "label"), d$label[[2]])
  expect_is(digit, "mnist_digit")
  expect_is(unclass(digit), "matrix")
  expect_equal(dim(digit), c(28L, 28L))

  expect_output(print(digit), "<mnist digit", fixed = TRUE)
  expect_output(print(digit, TRUE), "@", fixed = TRUE)

  expect_equal(as.integer(digit), attr(digit, "label"))
})

test_that("training data set", {
  d <- load_mnist(TRUE)
  expect_equal(length(d), 60000)
  expect_is(d, "mnist")
})

## Loading all the images at once is fairly slow, so it would be best
## to avoid doing that and just load them on demand.  We can memoise
## this process so that it's fast within a single session without
## hitting the disk.  To do this we should read the index file?
read_mnist_label_file <- function(filename) {
  con <- gzfile(filename, "rb")
  on.exit(close(con))
  n <- readBin(con, "integer", n = 2L, size = 4L, endian = "big")
  as.integer(readBin(con, raw(), n[[2L]]))
}

read_mnist_image_file <- function(filename) {
  con <- gzfile(filename, "rb")
  on.exit(close(con))
  n <- readBin(con, "integer", n = 4L, size = 4L, endian = "big")
  if (n[[1L]] != 2051L) {
    stop("Invalid magic number")
  }
  n <- n[-1L]
  m <- prod(n)
  array(as.integer(readBin(con, raw(), n = prod(n))), rev(n))
}

load_mnist <- function(train) {
  key <- if (train) "train" else "t10k"
  if (!exists(key, cache)) {
    cache[[key]] <- read_mnist(train)
  }
  cache[[key]]
}

read_mnist <- function(train = FALSE) {
  path <- cache_dir()
  if (train) {
    f_labels <- "train-labels-idx1-ubyte.gz"
    f_images <- "train-images-idx3-ubyte.gz"
  } else {
    f_labels <- "t10k-labels-idx1-ubyte.gz"
    f_images <- "t10k-images-idx3-ubyte.gz"
  }

  labels <- read_mnist_label_file(file.path(path, f_labels))
  images <- read_mnist_image_file(file.path(path, f_images))

  ret <- list(label = labels, data = images)
  class(ret) <- "mnist"
  ret
}

FILENAMES <- c("train-images-idx3-ubyte.gz",
               "train-labels-idx1-ubyte.gz",
               "t10k-images-idx3-ubyte.gz",
               "t10k-labels-idx1-ubyte.gz")
URL <- "http://yann.lecun.com/exdb/mnist"

cache_dir <- function() {
  getOption("rmnist.cache_dir", rappdirs::user_cache_dir("rmnist"))
}

has_images <- function() {
  all(FILENAMES %in% dir(cache_dir()))
}

download_mnist <- function() {
  if (!has_images()) {
    urls <- file.path(URL, FILENAMES)
    path <- cache_dir()
    dir.create(path, FALSE, TRUE)
    for (f in FILENAMES) {
      download_file(file.path(URL, f), file.path(path, f))
    }
  }
}

download_file <- function(url, dest) {
  tmp <- tempfile()
  on.exit(file.remove(tmp))
  status <- download.file(url, tmp, mode = "wb")
  if (status != 0) {
    stop("Download failed with code ", status)
  }
  ok <- suppressWarnings(file.rename(tmp, dest))
  if (!ok) {
    ## Linux
    ok <- file.copy(tmp, dest, overwrite = TRUE)
    if (!ok) {
      stop("Error moving file after downloading")
    }
  }
  invisible(dest)
}

## Internal cache to avoid reloading the files more than needed (can
## be fairly slow)
cache <- NULL
.onLoad <- function(...) {
  cache <<- new.env(parent = emptyenv())
}

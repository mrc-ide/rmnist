##' Load the MNIST images.  There are two sets "train" and "t10k"; the
##' \code{train} argument switches between these.  The actual images
##' are not shipped with the package as they are ~11MB.  Instead these
##' can be downloaded with \code{download_mnist} or by passing
##' \code{download_if_missing = TRUE} to this function.
##'
##' The first time within a session that the images are loaded, the
##' loading process may be slow as the files are read from disk (this
##' is particularly true for the "train = TRUE" data set, which is the
##' larger of the two).  Subsequent loads will be much faster though,
##' as the processed images are cached within a session.
##'
##' @title Load the MNIST images
##'
##' @param train Logical, indicating if the training data set should
##'   be loaded.  If \code{FALSE} then the "t10k" set is loaded.
##'
##' @param download_if_missing Logical, indicating if the images
##'   should be downloaded if not present.  See
##'   \code{\link{download_mnist}} for details about the downloading
##'   and where files will be stored.
##'
##' @export
##' @examples
##' mnist <- load_mnist(FALSE)
##' mnist
##'
##' x <- mnist[[2]]
##' x
##' print(x, TRUE)
##' plot(x)
load_mnist <- function(train, download_if_missing = FALSE) {
  key <- if (train) "train" else "t10k"
  if (!exists(key, cache)) {
    if (!has_images()) {
      if (download_if_missing) {
        download_mnist()
      } else {
        stop("Please run download_mnist() to download files first")
      }
    }
    cache[[key]] <- read_mnist(train)
  }
  cache[[key]]
}

##' Download the MNIST images.  Because these about 11MB they do not
##' ship with the package, but are downloaded as needed.
##'
##' The downloaded images will be stored on your system by default at
##' the directory given by \code{rappdirs::user_cache_dir("rmnist")}.
##' Alternatively, you can specify your own location for the images by
##' setting the option \code{rmnist.cache_dir} (e.g.,
##' \code{rmnist.cache_dir = tempfile()}).  To see what value will be
##' used, you can run the (unexported) function
##' \code{rmnist:::cache_dir()}.
##'
##' @title Download the MNIST images
##'
##' @param verbose Print a message even if the images are already found
##'
##' @param quiet Passed through to \code{download.file} to suppress
##'   the download progress bar.
##'
##' @export
download_mnist <- function(verbose = FALSE, quiet = FALSE) {
  if (!has_images()) {
    urls <- file.path(URL, FILENAMES)
    path <- cache_dir()
    dir.create(path, FALSE, TRUE)
    message("Downloading MNIST images to ", path)
    for (f in FILENAMES) {
      download_file(file.path(URL, f), file.path(path, f), quiet = quiet)
    }
  } else if (verbose) {
    message(sprintf("MNIST images already found at '%s'", cache_dir()))
  }
}

read_mnist_label_file <- function(filename) {
  con <- gzfile(filename, "rb")
  on.exit(close(con))
  n <- readBin(con, "integer", n = 2L, size = 4L, endian = "big")
  stopifnot(n[[1L]] == 2049L)
  as.integer(readBin(con, raw(), n[[2L]]))
}

read_mnist_image_file <- function(filename) {
  con <- gzfile(filename, "rb")
  on.exit(close(con))
  n <- readBin(con, "integer", n = 4L, size = 4L, endian = "big")
  stopifnot(n[[1L]] == 2051L)
  n <- n[-1L]
  m <- prod(n)
  array(as.integer(readBin(con, raw(), n = prod(n))), rev(n))
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

download_file <- function(url, dest, ...) {
  tmp <- tempfile()
  on.exit(file.remove(tmp))
  status <- download.file(url, tmp, mode = "wb", ...)
  if (status != 0) {
    stop("Download failed with code ", status)
  }
  ok <- suppressWarnings(file.rename(tmp, dest))
  if (ok) {
    on.exit()
  } else {
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

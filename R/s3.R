##' @export
`[.mnist` <- function(x, i, ...) {
  ret <- list(label = x$label[i],
              data = x$data[, , i, drop = FALSE])
  class(ret) <- class(x)
  ret
}

##' @export
`[[.mnist` <- function(x, i, ...) {
  stopifnot(length(i) == 1L)
  ret <- x$data[, , i, drop = TRUE]
  attr(ret, "label") <- x$label[[i]]
  class(ret) <- c("mnist_digit", "matrix")
  ret
}

##' @export
length.mnist <- function(x, ...) {
  length(x$label)
}

##' @export
print.mnist <- function(x, ...) {
  cat(sprintf("<mnist object of %s entries>\n", length(x)))
  n <- getOption("width", 80)
  lab <- paste(head(x$label, max(floor((n - 15) / 2), 1)), collapse = " ")
  cat(sprintf("  label: %s...\n", lab))
  invisible(x)
}

##' @export
as.integer.mnist <- function(x, ...) {
  x$label
}

##' @export
print.mnist_digit <- function(x, show_digit = FALSE, ...) {
  cat(sprintf("<mnist digit (%d)>\n", attr(x, "label")))
  if (show_digit) {
    cat(paste0(format(x), "\n"))
  } else {
    cat("  (use print(x, TRUE) to display the digit to the terminal\n")
  }
  invisible(x)
}

##' @export
format.mnist_digit <- function(x, ...) {
  tx <- t(x)
  cols <- c(" ", ".", ":", "-", "=", "+", "*", "#", "%", "@")
  i <- findInterval(tx, seq(0, 255, length.out = length(cols)))
  apply(array(cols[i], dim(tx)), 1, paste, collapse = "")
}

##' @export
as.integer.mnist_digit <- function(x, ...) {
  attr(x, "label")
}

##' @export
plot.mnist_digit <- function(x, ..., box = TRUE) {
  ix <- 255 - x[, rev(seq_len(nrow(x))), drop = FALSE]
  graphics::image(ix, col = gray(seq(0, 1, length.out = 256)), axes = FALSE)
  if (box) {
    graphics::box()
  }
}

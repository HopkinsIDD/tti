stop_glue <- function(..., .sep = "", .envir = parent.frame(),
                      call. = FALSE, .domain = NULL) {
  stop(
    glue::glue(..., .sep = .sep, .envir = .envir),
    call. = call., domain = .domain
  )
}

is_probability <- function(x) {
  if (!is.numeric(x) || x < 0 || x > 1) {
    stop_glue(
      "Parameter `{deparse(substitute(x))}` ",
      "must be a numeric value between 0 and 1.
              You input: {x}"
    )
  }
  invisible(TRUE)
}

is_positive <- function(x) {
  if (!is.numeric(x) || x < 0) {
    stop_glue(
      "Parameter `{deparse(substitute(x))}` ",
      "must be a positive numeric value.
              You input: {x}"
    )
  }
  invisible(TRUE)
}

not_equal <- function(x, y, ...) {
  !isTRUE(all.equal(x, y, ...))
}

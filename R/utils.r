dots <- function(...) {
  eval(substitute(alist(...)))  #note substitude an eval
}

# question!
named_dots <- function(...) {
  auto_name(dots(...))
}


#naming x if x has no names
auto_names <- function(x) {
  nms <- names2(x)
  missing <- nms == ""
  if (all(!missing)) return(nms)

  deparse2 <- function(x) paste(deparse(x, 500L), collapse = "")
  defaults <- vapply(x[missing], deparse2, character(1), USE.NAMES = FALSE)

  nms[missing] <- defaults
  nms
}

#
deparse_trunc <- function(x, width = getOption("width")) {
  text <- deparse(x, width.cutoff = width)
  if (length(text) == 1 && nchar(text) < width) return(text)

  paste0(substr(text[1], 1, width - 3), "...")
}

#DONE
auto_name <- function(x) {
  names(x) <- auto_names(x)
  x
}

#DONE
is.lang <- function(x) {
  is.name(x) || is.atomic(x) || is.call(x)
}

#DONE
is.lang.list <- function(x) {
  if (is.null(x)) return(TRUE)

  is.list(x) && all_apply(x, is.lang)
}

# note this usage!!
on_failure(is.lang.list) <- function(call, env) {
  paste0(call$x, " is not a list containing only names, calls and atomic vectors")
}

#DONE
only_has_names <- function(x, nms) {
  all(names(x) %in% nms)
}

on_failure(all_names) <- function(call, env) {
  x_nms <- names(eval(call$x, env))
  nms <- eval(call$nms, env)
  extra <- setdiff(x_nms, nms)

  paste0(call$x, " has named components: ", paste0(extra, collapse = ", "), ".",
    "Should only have names: ", paste0(nms, collapse = ","))
}

all_apply <- function(xs, f) {
  for (x in xs) {
    if (!f(x)) return(FALSE)
  }
  TRUE
}


any_apply <- function(xs, f) {
  for (x in xs) {
    if (f(x)) return(TRUE)
  }
  FALSE
}

drop_last <- function(x) {
  if (length(x) <= 1L) return(NULL)
  x[-length(x)]
}

compact <- function(x) Filter(Negate(is.null), x)

# return x's name if x has names,else return blank space
names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

#DONE
"%||%" <- function(x, y) if(is.null(x)) y else x

is.wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5) {
  abs(x - round(x)) < tol
}

# qustion!!
as_df <- function(x) {
  class(x) <- "data.frame"
  attr(x, "row.names") <- c(NA_integer_, -length(x[[1]]))

  x
}

deparse_all <- function(x) {
  deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")
  vapply(x, deparse2, FUN.VALUE = character(1))
}

commas <- function(...) paste0(..., collapse = ", ")

in_travis <- function() identical(Sys.getenv("TRAVIS"), "true")

named <- function(...) {
  x <- c(...)

  missing_names <- names2(x) == ""
  names(x)[missing_names] <- x[missing_names]

  x
}


#so cool
unique_name <- local({
  i <- 0

  function() {
    i <<- i + 1
    paste0("_W", i)
  }
})

isFALSE <- function(x) identical(x, FALSE)

substitute_q <- function(x, env) {
  call <- substitute(substitute(x, env), list(x = x))
  eval(call)
}

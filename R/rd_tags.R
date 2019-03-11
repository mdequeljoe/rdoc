#convert rd tags to their corresponding formatting

tag_ <- function(l, default = character(1)) {
  if (is.null(x <- attr(l, "Rd_tag")))
    return(default)
  x
}

apply_fmt <- function(l, op, cl){
  lapply(l, function(d){
    if (is.list(d)){
      at <- attributes(d)
      x <- apply_fmt(d, op, cl)
      attributes(x) <- at
    }
    else{
      at <- attributes(d)
      x <- paste0(op, d, cl)
      attributes(x) <- at
    }
    x
  })
}

is_all_text <- function(l){
  rd_tag <- vapply(l, attr, character(1), which = "Rd_tag")
  all(rd_tag == "TEXT")
}

convert_tag <- function(op, cl){
  function(l) {
    l <- apply_fmt(l, op, cl)

    if (is_all_text(l)){
      l <- Reduce(paste, l)
      attr(l, "Rd_tag") <- "TEXT"
      return(l)
    }

    if (length(l) > 1)
      return(l)

    l[[1]]
  }
}

fmt_italics <- convert_tag("\033[3m", "\033[23m")
fmt_bold <- convert_tag("\033[1m", "\033[22m")

format_rdo <- function(l) {

  att <- attributes(l)
  o <- lapply(l, function(d) {
    if (is.list(d))
      d <- format_rdo(d)
    d
  })
  attributes(o) <- att

  # R logo symbol
  if (tag_(o) == "\\R"){
    x <- "<R>"
    attr(x, "Rd_tag") <- "TEXT"
    return(x)
  }

  if (tag_(o) == "\\emph")
    return(fmt_italics(o))

  if (tag_(o) == "\\bold" || tag_(o) == "\\strong")
    return(fmt_bold(o))
  o
}


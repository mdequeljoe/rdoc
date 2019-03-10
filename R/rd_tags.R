#convert rd tags

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

convert_tag <- function(op, cl){
  function(l) {
    l <- apply_fmt(l, op, cl)

    att <- attr(l[[1]], "Rd_tag")
    l <- unlist(l, recursive = FALSE)

    if (is.null(att))
      attr(l, "Rd_tag") <- "TEXT"
    else
      attr(l, "Rd_tag") <- att
    l
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

  # empty list
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


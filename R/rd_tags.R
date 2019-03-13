#convert rd tags to their corresponding formatting

tag_ <- function(l, default = character(1)) {
  if (is.null(x <- attr(l, "Rd_tag")))
    return(default)
  x
}

get_subtags <- function(l){
  subtag_ <- function(l){
    lapply(l, function(d){
      if (is.list(d))
        subtag_(d)
      else
        tag_(d)
    })
  }
  l <- subtag_(l)
  unlist(l)
}

apply_fmt <- function(l, op, cl){
  lapply(l, function(d){
    at <- attributes(d)
    if (is.list(d))
      x <- apply_fmt(d, op, cl)
    else
      x <- paste0(op, d, cl)
    attributes(x) <- at
    x
  })
}

convert_tag <- function(op, cl){
  function(l) {
    l <- apply_fmt(l, op, cl)
    attr(l, "Rd_tag") <- "\\special"
    l
  }
}


fmt_italics <- convert_tag("\033[3m", "\033[23m")
fmt_bold <- convert_tag("\033[1m", "\033[22m")
fmt_squotes <- convert_tag("'", "'")

fmt_inline_code <- function(l){
  st <- get_subtags(l)

  if (!all(st == "RCODE"))
    return(l)

  l <- unlist(l)
  l <- tryCatch(
    prettycode::highlight(l),
    error = function(e) l,
    warning = function(w) l
  )
  attr(l, "Rd_tag") <- "TEXT"
  l
}

format_rdo <- function(l) {

  att <- attributes(l)
  if (tag_(l) %in% c("\\example", "\\examples", "\\usage"))
    return(l)

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

  if (tag_(o) == "\\code")
    return(fmt_inline_code(o))

  if (tag_(o) == "\\emph")
    return(fmt_italics(o))

  if (tag_(o) == "\\bold" || tag_(o) == "\\strong")
    return(fmt_bold(o))
  o
}


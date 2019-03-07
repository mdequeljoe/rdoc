#convert rd tags

tag_ <- function(l) {
  if (is.null(x <- attr(l, "Rd_tag")))
    return(character(1))
  x
}

to_flat <- function(l) {
  if (is.null(x <- attr(l, "flat")))
    return(FALSE)
  x
}

save_tags <- function(l){

  get_tag <- function(l){
    lapply(l, function(d){
      if (is.list(d))
        get_tag(d)
      else
        attr(d, "Rd_tag")
    })
  }

  a <- get_tag(l)
  unlist(a)
}

collapse_ <- function(l){
  att <- attributes(l)
  tags <- save_tags(l)
  l <- unlist(l, recursive = FALSE)
  l <- restore_tags(l, tags)
  attributes(l) <- att
  l
}

restore_tags <- function(l, tags){
  i <- 1
  l <- lapply(l, function(d){
    if (is.null(attr(d, "Rd_tag")))
      attr(d, "Rd_tag") <- tags[i]
    i <<- i + 1
    d
  })
  l
}

convert_tag <- function(op, cl){
  function(l) {
    l[[1]][1] <- paste0(op, l[[1]][1])
    l[[length(l)]][1] <- paste0(l[[length(l)]][1], cl)
    tags <- save_tags(l)
    l <- unlist(l, recursive = FALSE)
    l <- restore_tags(l, tags)
    attr(l, "flat") <- TRUE
    l
  }
}

fmt_italics <- convert_tag("\033[3m", "\033[23m")
fmt_bold <- convert_tag("\033[1m", "\033[22m")

format_rdo <- function(l) {

  as_flat <- FALSE
  att <- attributes(l)
  o <- lapply(l, function(d) {
    if (is.list(d)) {
      d <- format_rdo(d)

      if (to_flat(d))
        as_flat <<- TRUE
    }
    d
  })
  attributes(o) <- att

  if (as_flat)
    o <- collapse_(o)

  if (tag_(o) == "\\emph")
    return(fmt_italics(o))

  if (tag_(o) == "\\bold" || tag_(o) == "\\strong")
    return(fmt_bold(o))
  o
}


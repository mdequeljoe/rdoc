#convert rd tags

tag_ <- function(l, default = character(1)) {
  if (is.null(x <- attr(l, "Rd_tag")))
    return(default)
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
      if (is.list(d) && length(d))
        get_tag(d)
      else
        tag_(d, "TEXT")
    })
  }
  a <- get_tag(l)
  unlist(a)
}

collapse_tag <- function(l, id){
  for (i in id){
    att <- attributes(l[[i]])
    l[[i]] <- unlist(l[[i]], recursive = FALSE)
    attributes(l[[i]]) <- att
  }
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
    attr(l, "Rd_tag") <- "TEXT"
    attr(l, "flat") <- TRUE
    l
  }
}

fmt_italics <- convert_tag("\033[3m", "\033[23m")
fmt_bold <- convert_tag("\033[1m", "\033[22m")

format_rdo <- function(l) {

  id <- numeric()
  i <- 1L
  att <- attributes(l)
  o <- lapply(l, function(d) {
    if (is.list(d)) {
      d <- format_rdo(d)

      if (to_flat(d)){
        id <<- c(id, i)
      }
    }
    i <<- i + 1
    d
  })
  attributes(o) <- att

  if (length(id))
    o <- collapse_tag(o, id)

  if (tag_(o) == "\\emph")
    return(fmt_italics(o))

  if (tag_(o) == "\\bold" || tag_(o) == "\\strong")
    return(fmt_bold(o))
  o
}


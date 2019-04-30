#convert rd tags to their corresponding text formatting

tag_ <- function(l, default = character(1)) {
  if (is.null(x <- attr(l, "Rd_tag")))
    return(default)
  x
}

subtags_ <- function(l) {
  stag <- function(l) {
    lapply(l, function(d) {
      if (is.list(d) && length(d))
        stag(d)
      else
        tag_(d)
    })
  }
  l <- stag(l)
  unlist(l)
}

apply_fmt <- function(l, f) {
  lapply(l, function(d) {
    at <- attributes(d)
    if (is.list(d))
      x <- apply_fmt(d, f)
    else
      x <- f(d)
    attributes(x) <- at
    x
  })
}

# convert style tags
# replace with \\special tag to keep existing list as is
# since from Rd2txt we want to use 'writeContent' with no markup :
#
# "\\special" = ,
# "\\var" = writeContent(block, tag),
#
#
# "\\bold"=,
# "\\strong"= {
#   put("*")
#   writeContent(block, tag)
#   put("*")
# },

format_tag <- function(l, f, tag = "\\special") {
  if (is.null(f))
    return(l)
  l <- apply_fmt(l, f)
  attr(l, "Rd_tag") <- tag
  l
}

format_href <- function(l, f){
  if (is.null(f))
    return(l)
  s <- subtags_(l)
  l <- unlist(l)
  href <- f(l[s == "VERB"])
  href <- sprintf("[%s](%s)", l[s == "TEXT"], href)
  attr(href, "Rd_tag") <- "TEXT"
  href
}

format_link <- function(l, f){
  if (is.null(f))
    return(l)

  src <- f(attr(l, "Rd_option"))
  if (is.null(src))
    return(l)

  link <- sprintf("[%s](%s)", unlist(l), src)
  attr(link, "Rd_tag") <- "TEXT"
  link
}

format_inline_code <-
  function(l, styles = prettycode::default_style()) {

    if (!all(subtags_(l) == "RCODE"))
      return(l)

    # encode string to avoid cases like
    # "\"\\1\"" -> "\" and "\"\\9\"" -> error
    l <- unlist(l)
    l <- gsub("\\\\dots", "...", l)
    l <- gsub("[[:cntrl:]]", "", l)
    l <- encodeString(l)
    l <- tryCatch(
      prettycode::highlight(l, styles),
      error = function(e)
        l,
      warning = function(w)
        l
    )
    attr(l, "Rd_tag") <- "TEXT"
    l
  }

# a unlist->paste->strsplit won't always work
# since tabs and cr may be confounded as they
#   are represented as:
#   structure(list(), Rd_tag = "\\tab")
#   structure(list(), Rd_tag = "\\cr")
#' @importFrom cli boxx
format_table <- function(l, box_options = NULL) {
  v <- vapply(l[[2]], function(d) {
    if (tag_(d) == "\\tab")
      return(" ")
    if (tag_(d) == "\\cr")
      return("\n")
    d
  }, character(1))

  if (v[1] == "\n")
    v <- v[-1]
  v <- paste(v, collapse = "")
  v <- strsplit(v, "\n")
  if (!is.null(box_options))
    v <- c(v, box_options)
  do.call(cli::boxx, v)[]
}

table_id <- function(n = character(0))
  paste0("##>>RDOC_TABLE_", n)

# format rdo
# return formatted rd object as well as list as formatted tables
# can't replace tables directly since Rd2txt will interfere with
# the formatting by stripping blankspaces and line breaks.
# tables are substituted in later via the tags ##>>RDOC_TABLE_n

format_rdo <-
  function(l,
           opts = rdoc_text_formats(),
           exclude = c("\\example", "\\examples", "\\usage")) {

    tabular <- list()
    format_rdo_ <- function(l) {

      if (tag_(l) %in% exclude)
        return(l)

      att <- attributes(l)
      o <- lapply(l, function(d) {
        if (is.list(d))
          d <- format_rdo_(d)
        d
      })
      attributes(o) <- att

      if (tag_(o) == "\\R" && !is.null(opts$r_logo)){
        x <- opts$r_logo("R")
        attr(x, "Rd_tag") <- "TEXT"
        return(x)
      }

      if (tag_(o) == "\\tabular" && !is.null(opts$table)){
        v <- format_table(o, opts$table)
        tabular[length(tabular) + 1] <<- v
        x <- sprintf("\n\n%s\n\n", table_id(length(tabular)))
        attr(x, "Rd_tag") <- "TEXT"
        return(x)
      }

      if (tag_(o) == "\\pkg")
        return(format_tag(o, opts$pkg))

      if (tag_(o) == "\\code")
        return(format_inline_code(o, opts$inline_code))

      if (tag_(o) == "\\emph")
        return(format_tag(o, opts$italic))

      if (tag_(o) == "\\bold" || tag_(o) == "\\strong")
        return(format_tag(o, opts$bold))

      if (tag_(o) == "\\email")
        return(format_tag(o, opts$email))

      if (tag_(o) == "\\url")
        return(format_tag(o, opts$url))

      if (tag_(o) == "\\href")
        return(format_href(o, opts$href))

      if (tag_(o) == "\\link")
        return(format_link(o, opts$link))

      o
    }

   l <- format_rdo_(l)
   if (length(tabular))
    names(tabular) <- table_id(1:length(tabular))
   list(rdo = l, tables = tabular)
  }

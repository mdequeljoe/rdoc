rd_ <- function(which = NULL) {
  function(topic,
           package = NULL,
           lib.loc = NULL){

    if (is.function(topic))
      topic <- as.character(substitute(topic))

    if (!missing(package)) {
      p <- as.character(substitute(package))
      if (p %in% loadedNamespaces())
        package <- p
    }

    k <- as.call(list(
      utils::`help`,
      topic,
      package,
      lib.loc
    ))
    k <- eval(k)[]
    help_path <- k[1L:length(k)]

    d <-
      Rdoc$new(topic, help_path, rd_opts())
    d$show(which)
  }
}

#' R docs
#'
#' Refer to colourised \R docs in the console
#' @family rd access
#' @aliases rd
#' @param topic \code{character(1)}, help topic
#' @param package \code{character(1)}, package of help topic. Defaults to NULL.
#' @param lib.loc \code{character(1)}, library location of package. Defaults to null.
#'
#' @examples \dontrun{
#'
#' rd("rd", by_section = FALSE)
#' rd(grepl)
#' rd_example("min")
#' rd_usage(substr)
#'
#' }
#' @export
rd <- rd_()


#' @rdname rd
#' @export
rd_details <- rd_("details")

#' @rdname rd
#' @export
rd_arguments <- rd_("arguments")

#' @rdname rd
#' @export
rd_usage <- rd_("usage")

#' @rdname rd
#' @aliases rd_examples
#' @export
rd_example <- rd_("examples")

#' @export
rd_examples <- rd_example


rd_question <- function(type, topic) {
  type <- substitute(type)
  topic <- substitute(topic)

  if (missing(topic)) {
    topic <- type
    k <- as.call(list(utils::`?`, topic))
  } else {
    k <- as.call(list(utils::`?`, type, topic))
  }

  k <- eval(k)[]
  help_path <- k[1L:length(k)]
  topic <- as.character(topic)
  topic <- topic[length(topic)]

  d <-
    Rdoc$new(topic,
             help_path,
             rd_opts())
  d$show()
}

#' rdoc
#' rdoc-style \code{?} replacement for \code{utils::`?`}
#'
#' @details
#' Calling \code{rdoc()} will override \code{utils::`?`}
#' @export
rdoc <- function(){

  if ("rdoc" %in% searchpaths()){
    base::detach("rdoc")
    return(invisible(NULL))
  }

  e <- new.env()
  e$`?` <- rd_question
  base::attach(e, name = "rdoc", warn.conflicts = FALSE)
}


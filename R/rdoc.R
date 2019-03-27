#' @importFrom utils help
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

#' Colourised R documentation
#'
#' Refer to colourised \R docs as terminal/console output. Provides a
#' replacement for \code{help}. A number of common section accessors are also
#' provided.
#' @family rd access
#' @aliases rd
#' @param topic \code{character(1)}, help topic
#' @param package \code{character(1)}, package of help topic. Defaults to
#'   \code{NULL}
#' @param lib.loc \code{character(1)}, library location of package. Defaults to
#'   \code{NULL}
#'
#' @details The manner in which content is outputted depends on where it is
#'   being used. If used from a terminal the contents will be outputted via
#'   \code{file.show}. If used from a console the output will be printed to
#'   screens by sections unless this has been disabled via
#'   \code{options(rdoc.by_section = FALSE)}. In this case pressing any key will
#'   print out the next section. Exiting out early is possible with the 'q' key.
#'   Color support is checked via \code{crayon::has_color()}.
#'
#' @examples \dontrun{
#'
#' rd("rd")
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

#' rd ?
#' @inherit rd details
#' @importFrom utils ?
#' @export
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

#' Colourised R documentation
#'
#' rdoc replacements for \code{?} and \code{help}
#'
#' @details
#' Calling \code{rdoc()} will override \code{utils::`?`} with \code{rd_question}
#' and \code{utils::help} with \code{rd}
#' These replacements can be unset by calling \code{rdoc()} once more.
#' @examples \dontrun{
#'
#' rdoc::rdoc()
#' ?help
#' rdoc::rdoc()
#' ?help
#' }
#'
#' @export
rdoc <- function(){

  if ("rdoc" %in% searchpaths()){
    detach("rdoc")
    return(invisible(NULL))
  }

  e <- new.env()
  e$`?` <- rd_question
  attach(e, name = "rdoc", warn.conflicts = FALSE)
}

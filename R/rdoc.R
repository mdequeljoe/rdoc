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
    k <- tryCatch(eval(k), error = function(e) e)
    if (!length(k))
      return(k)
    help_path <- k[1L:length(k)]

    d <-
      Rdoc$new(topic, help_path, which, rd_opts())
    d$show()
  }
}

#' Colourised \R documentation
#'
#' Refer to colourised \R docs as terminal output. Provides a replacement for
#' \code{help}. A number of common (substantive) section accessors are also
#' provided.
#' @family rdoc access
#' @aliases rdoc
#' @param topic \code{character(1)}, help topic
#' @param package \code{character(1)}, package of help topic. Defaults to
#'   \code{NULL}
#' @param lib.loc \code{character(1)}, library location of package. Defaults to
#'   \code{NULL}
#'
#' @details The manner in which content is outputted depends on where it is
#'   being used. If used from a terminal the contents will be outputted via
#'   \code{file.show}. Otherwise the output will be printed by sections unless
#'   this has been disabled via \code{options(rdoc.by_section = FALSE)}. When
#'   printing by section pressing any key will print out the next section.
#'   Exiting out early is possible via 'q' keypress. Color support is checked
#'   via \code{crayon::has_color()}. The output styles can be modified via
#'   \code{rdoc_style()} and passing this to the option:
#'   \code{options(rdoc.style = rdoc_style())}
#'
#' @examples \dontrun{
#'
#' rdoc("rdoc")
#' rdoc_examples("min")
#' rdoc_usage(substr)
#'
#' }
#' @export
rdoc <- rd_()


#' @rdname rdoc
#' @export
rdoc_usage <- rd_("usage")

#' @rdname rdoc
#' @export
rdoc_arguments <- rd_("arguments")

#' @rdname rdoc
#' @export
rdoc_details <- rd_("details")

#' @rdname rdoc
#' @aliases rd_examples
#' @export
rdoc_examples <- rd_("examples")


#' Colourised \R documentation
#'
#' Refer directly to .Rd file contents as terminal output.
#'
#' @param path character(1), the path to an .Rd file
#'
#' @inherit rdoc details
#' @export
rdoc_rd <- function(path){
  stopifnot(file.exists(path), is_rd_file(path))
  topic <- tools::file_path_sans_ext(basename(path))
  d <- Rdoc$new(topic, path)
  d$show()
}

#' Colourised \R documentation
#'
#' Refer to colourised \R docs as terminal output. Provides a
#' replacement for \code{?}. Meant primarily for use as an
#' override in \code{use_rdoc()}
#' @inherit rdoc details
#' @inheritParams utils::`?`
#' @importFrom utils ?
#' @examples \dontrun{
#'
#' rdoc_question(lapply)
#'
#' }
#' @export
rdoc_question <- function(type, topic) {
  type <- substitute(type)
  topic <- substitute(topic)

  if (missing(topic)) {
    topic <- type
    k <- as.call(list(utils::`?`, topic))
  } else {
    k <- as.call(list(utils::`?`, type, topic))
  }

  k <- tryCatch(eval(k), error = function(e) e)
  if (!length(k))
    return(k)
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
#' @family use_rdoc shim
#' @aliases use_rdoc
#' @details
#' Calling \code{use_rdoc()} will override \code{utils::`?`} with \code{rdoc_question}
#' and \code{utils::help} with \code{rdoc}
#' These replacements can be unset by calling \code{rm_rdoc()}.
#' @examples \dontrun{
#'
#' rdoc::use_rdoc()
#' ?help
#' rdoc::rm_rdoc()
#' ?help
#' }
#'
#' @export
use_rdoc <- function(){

  if ("rdoc" %in% search())
    return(invisible(NULL))

  e <- new.env()
  e$`?` <- rdoc_question
  e$help <- rdoc
  base::attach(e, name = "rdoc", warn.conflicts = FALSE)
}

#' @rdname use_rdoc
#' @export
rm_rdoc <- function(){
  if (!"rdoc" %in% search())
    return(invisible(NULL))
  base::detach("rdoc")
}

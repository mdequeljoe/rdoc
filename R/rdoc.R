
rd_ <- function(which = NULL, method = "show") {
  function(topic,
           package = NULL,
           lib.loc = NULL){

    topic_ <- as.character(substitute(topic))
    topic <- tryCatch({
      if (is.function(topic)) topic_ else topic
    }, error = function(e) topic_)

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
    d[[method]]()
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
#'   \code{file.show}. Otherwise the output will be printed interactively by
#'   sections unless this has been disabled via \code{options(rdoc.by_section =
#'   FALSE)}. When printing by section pressing the <enter> keypress will show
#'   the next section. Any other keypress will exit the interaction without
#'   printing any remaining sections. Color support is checked via
#'   \code{crayon::has_color()}. Customising rdoc output is possible via
#'   `options`, see \code{\link{rdoc_options}} for more details.
#'
#' @examples \donttest{
#'
#' library(rdoc)
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
#' @export
rdoc_examples <- rd_("examples")


#' Colourised \R documentation
#'
#' Access the underlying rdoc text lines
#' @inheritParams rdoc
#' @return character
#' @examples
#' txt <- rdoc::rdoc_text('min')
#' @export
rdoc_text <- rd_(method = "rdoc_text")

#' Colourised \R documentation
#'
#' Refer directly to .Rd file contents as terminal output.
#'
#' @param path character(1), the path to an .Rd file
#'
#' @inherit rdoc details
#' @examples \donttest{
#' d <- system.file('extdata/rdoc_test.Rd', package = "rdoc")
#' rdoc::rdoc_rd(d)
#' }
#'
#' @export
rdoc_rd <- function(path){
  stopifnot(file.exists(path), is_rd_file(path))
  topic <- tools::file_path_sans_ext(basename(path))
  d <- Rdoc$new(topic, path, rd_opts())
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
#' @examples \donttest{
#'
#' rdoc::rdoc_question('lapply')
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

#' Base replacements
#'
#' rdoc replacements for \code{?} and \code{help}
#'
#' @family use_rdoc shim
#' @aliases use_rdoc
#' @details
#' Calling \code{use_rdoc()} will override \code{utils::`?`} with \code{rdoc_question}
#' and \code{utils::help} with \code{rdoc}
#' These replacements can be unset by calling \code{rm_rdoc()}.
#' @examples \donttest{
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


#' rdoc options
#'
#' @description
#'
#' Rdoc uses the following \code{options()} to set output:
#'
#' \itemize{ \item `rdoc.by_section`: logical, output interactively printed by
#' sections for console sessions. Sections will be printed with the <enter>
#' keypress. Any other keypress will exit the interaction without printing the
#' remaining sections. Default is TRUE.
#'
#' \item `rdoc.header`: logical, whether package header is included in the
#' output. Default is TRUE.
#'
#' \item `rdoc.text_formats`: see \code{\link{rdoc_text_formats}}
#'
#' \item `rdoc.style`: see \code{\link{rdoc_style}}
#'
#' \item `rdoc.item_bullet`: Symbol to use for itemized lists. Default is
#' \code{paste0(cli::symbol$circle, " ")} }
#' @docType package
#' @name rdoc_options
NULL


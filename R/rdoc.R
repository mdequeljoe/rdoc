rd_ <- function(which = NULL){
  function(topic,
           by_section = TRUE,
           options = rdoc_options(),
           package = NULL,
           lib.loc = NULL) {

    if (is.function(topic))
      topic <- as.character(substitute(topic))

    if (!missing(package)){
      p <- as.character(substitute(package))
      if (p %in% loadedNamespaces())
        package <- p
    }

    doc <- Rdoc$new(topic, by_section, options, package, lib.loc)
    doc$show(which)
  }
}

#' R docs
#'
#' Refer to colourised R docs in the console
#' @family rd access
#' @aliases rd
#' @param topic \code{character(1)}, help topic
#' @param by_section \code{logical(1)}, should docs be shown section by section or all
#'   at once? Defaults to TRUE.
#' @param package \code{character(1)}, package of help topic. Defaults to NULL.
#' @param lib.loc \code{character(1)}, library location of package. Defaults to null.
#'
#' @examples \dontrun{
#'
#' rd("rd", by_section = FALSE)
#' rd_example("min")
#'
#' }
#' @export
rd <- rd_()

#' @family rd access
#' @inherit rd
#' @export
rd_details <- rd_("details")

#' @inheritParams rd
#' @aliases rd_examples
#' @export
rd_example <- rd_("examples")

#' @export
rd_examples <- rd_example


#' R doc options
#'
#' Set output formatting styles
#' @param pkg_header package header information. Defaults to \code{cli::rule}
#' @param title topic title. Defaults to \code{crayon::bold}
#' @param section_titles section titles Defaults to \code{crayon::underline}
#' @param arg_params argument paramets. Defaults to \code{crayon::underline}
#' @param code_style code sections and inline code. Defaults to \code{prettycode::default_style()}
#' @param item_bullet item bullet points. Defaults to \code{cli::symbol$bullet}
#' @import crayon
#' @import prettycode
#' @import cli
#' @export
rdoc_options <- function(pkg_header = cli::rule,
                         title = crayon::bold,
                         section_titles = crayon::underline,
                         arg_params = crayon::underline,
                         code_style = prettycode::default_style(),
                         item_bullet = cli::symbol$bullet) {
  list(
    pkg_header = pkg_header,
    title = title,
    section_titles = section_titles,
    arg_params = arg_params,
    code_style = code_style,
    item_bullet = item_bullet
  )
}

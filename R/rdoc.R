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
#' @aliases rd
#' @param topic character(1), help topic
#' @param by_section logical(1), should docs be shown section by section or all
#'   at once? Defaults to TRUE.
#' @param package character(1), package of help topic. Defaults to NULL.
#' @param lib.loc character(1), library location of package. Defaults to null.
#'
#' @examples \dontrun{
#'
#' rd("min", by_section = FALSE)
#'
#' }
#' @export
rd <- rd_()

#' @export
rd_details <- rd_("details")

#' @export
rd_example <- rd_("examples")


#' R doc options
#'
#' Set output styles
#'
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

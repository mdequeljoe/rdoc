#' rdoc style options
#'
#' Set Rd output formatting styles to be supplied to \code{options(rdoc.style =
#'  rdoc_style())}
#' @param title topic title.
#' @param section_titles section titles
#' @param arguments argument parameters.
#' @param code_style code sections (e.g. examples) and inline code
#'   styles to be passed on to \code{prettycode::highlight}
#' @details all inputs should be functions or \code{NULL} with the exception of
#'   \code{code_style} which should be a list of functions.
#' @examples \donttest{
#' options(rdoc.style = rdoc::rdoc_style(arguments = crayon::italic))
#' }
#' @return A list
#' @export
rdoc_style <- function(title = crayon::bold,
                       section_titles = crayon::underline,
                       arguments = NULL,
                       code_style = prettycode::default_style()) {
  out <- list(
    title = title,
    section_titles = section_titles,
    arguments = arguments,
    code_style = code_style
  )
  stopifnot(is_valid_opt(out))
  style_names <- names(prettycode::default_style())
  valid_entry <- names(out$code_style) %in% style_names
  if (!all(valid_entry))
    stop("possible entries to code_style are: ",
         paste(style_names, collapse = ", "))
  out
}

#' Rdoc text formatting
#'
#' Set Rd text format defaults to be supplied to \code{options(rdoc.text_formats
#' = rdoc_text_formats())}
#' @param r_logo \R symbol
#' @param pkg \pkg{pkg} references
#' @param inline_code inline code, options to pass to the \code{style} argument
#'   of \code{prettycode::highlight} as a list of functions.
#' @param italic \emph{italic} font
#' @param bold \bold{bold} font
#' @param table tabular, options to pass to \code{cli::boxx} as a list.
#' @param email email formats
#' @param url url references
#' @param href href
#' @param link links
#' @details The input will generally be a function, unless otherwise indicated
#'   above. Functions must take and return a character vector. If set as
#'   \code{NULL}, the relevant text will be displayed using the defaults of
#'   \code{tools::Rd2txt} or the calling function it is passed to. Note that
#'   href and link formatting refer to the source reference which will otherwise
#'   be excluded by \code{tools::Rd2txt}.
#' @export
rdoc_text_formats <-
  function(r_logo = crayon::combine_styles("bold", "blue"),
           pkg = function(x)
             paste0("{", x, "}"),
           inline_code = prettycode::default_style(),
           italic = crayon::italic,
           bold = crayon::bold,
           table = list(float = "center",
                        border_style = "single",
                        align = "left"),
           email = crayon::underline,
           url = crayon::underline,
           href = crayon::underline,
           link = function(x) x) {
    out <- list(
      r_logo = r_logo,
      pkg = pkg,
      inline_code = inline_code,
      italic = italic,
      bold = bold,
      email = email,
      url = url,
      href = href,
      link = link
    )
    stopifnot(is_valid_opt(out))
    c(out, table = list(table))
  }

is_valid_opt <- function(l) {
  valid_ <- function(l) {
    lapply(l, function(d) {
      if (is.list(d))
        is_valid_opt(d)
      else
        is.function(d) || is.null(d)
    })
  }
  all(unlist(valid_(l)))
}

rd_opts <- function() {
  list(
    header = getOption("rdoc.header", TRUE),
    by_section = getOption("rdoc.by_section", TRUE),
    style = getOption("rdoc.style", rdoc_style()),
    text_formats = getOption("rdoc.text_formats", rdoc_text_formats()),
    item_bullet = getOption("rdoc.item_bullet", paste0(cli::symbol$circle, " ")),
    width = getOption("width")
  )
}


#' Rdoc style options
#'
#' Set Rd output formatting styles to be supplied to \code{options(rdoc.style =
#' rdoc_style())}
#' @param pkg_header package header information. Defaults to \code{cli::rule}
#' @param title topic title. Defaults to \code{crayon::bold}
#' @param section_titles section titles Defaults to \code{crayon::underline}
#' @param arg_params argument parameters. Defaults to \code{crayon::underline}
#' @param code_style code sections and inline code. Defaults to
#'   \code{prettycode::default_style()}
#' @param item_bullet item bullet points. Defaults to \code{cli::symbol$bullet}
#' @details all inputs with the exception of \code{item_bullet} should be functions.
#'
#' @return A list
#' @import crayon
#' @import prettycode
#' @import cli
#' @export
rdoc_style <- function(pkg_header = cli::rule,
                     title = crayon::bold,
                     section_titles = crayon::underline,
                     arg_params = crayon::underline,
                     code_style = prettycode::default_style(),
                     item_bullet = cli::symbol$bullet) {
  out <- list(
    pkg_header = pkg_header,
    title = title,
    section_titles = section_titles,
    arg_params = arg_params
  )
  out <- lapply(out, null_fn)
  stopifnot(all_fn(out))
  stopifnot(all_fn(code_style))
  style_names <- names(prettycode::default_style())
  valid_entry <- names(code_style) %in% style_names
  if (!all(valid_entry))
    stop("possible entries to code_style are: ",
         paste(style_names, collapse = ", "))
  stopifnot(is.character(item_bullet))
  c(out, list(code_style = code_style, item_bullet = item_bullet))
}

R_logo <- crayon::combine_styles("bold", "blue")

#' Rdoc text formatting
#'
#' Set Rd text format defaults to be supplied to \code{options(rdoc.text_formats =
#' rdoc_text_formats())}
#' @param r_logo \R symbol
#' @param pkg \pkg{pkg} formatting
#' @param email email
#' @param url = NULL
#' @param href = NULL
#' @details
#' All inputs must either be a function or NULL. If set as the latter, the relevant text
#' will be displayed using the defaults of \code{tools::Rd2txt}

rdoc_text_formats <- function(r_logo = R_logo,
                              pkg = function(x)
                                paste0("{", x, "}"),
                              italic = crayon::italic,
                              bold = crayon::bold,
                              squotes = NULL,
                              email = NULL,
                              url = NULL,
                              href = NULL) {
  out <- list(
    r_logo = R_logo,
    pkg = pkg,
    email = email,
    url = url,
    href = href
  )
  out <- lapply(out, null_fn)
  stopifnot(all_fn(out))
  out
}

all_fn <- function(x) all(vapply(x, is.function, logical(1)))
null_fn <- function(x) if (is.null(x)) function(x) x else x
rd_opts <- function(){
  list(
    header = getOption("rdoc.header", TRUE),
    by_section = getOption("rdoc.by_section", TRUE),
    style = getOption("rdoc.style", rdoc_style()),
    text_formats = getOption("rdoc.text_formats", rdoc_text_formats())
  )
}

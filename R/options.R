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
#' @details all inputs should be functions with the exception of
#'   \code{code_style} which should be a list of functions and
#'   \code{item_bullet} which should be a \code{character(1)}.
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
    arg_params = arg_params,
    code_style = code_style
  )
  stopifnot(is_valid_opt(out))
  style_names <- names(prettycode::default_style())
  valid_entry <- names(out$code_style) %in% style_names
  if (!all(valid_entry))
    stop("possible entries to code_style are: ",
         paste(style_names, collapse = ", "))
  stopifnot(is.character(item_bullet))
  c(out, item_bullet = item_bullet)
}

R_logo <- crayon::combine_styles("bold", "blue")

#' Rdoc text formatting
#'
#' Set Rd text format defaults to be supplied to \code{options(rdoc.text_formats
#' = rdoc_text_formats())}
#' @param r_logo \R symbol
#' @param pkg \pkg{pkg} references
#' @param inline_code inline code
#' @param italic italic font
#' @param bold bold font
#' @param squotes single quotes
#' @param email email
#' @param url url
#' @param href href
#' @details All inputs must either be a function (exceptionally as a list of
#' style functions for `inline_code`) or NULL. Functions must take and return a
#' vector: \code{character(1)} If set as the latter, the relevant text will be
#' displayed using the defaults of \code{tools::Rd2txt}

rdoc_text_formats <- function(r_logo = R_logo,
                              pkg = function(x)
                                paste0("{", x, "}"),
                              inline_code = prettycode::default_style(),
                              italic = crayon::italic,
                              bold = crayon::bold,
                              squotes = NULL,
                              email = NULL,
                              url = NULL,
                              href = NULL) {
  out <- list(
    r_logo = R_logo,
    pkg = pkg,
    inline_code = inline_code,
    italic = italic,
    bold = bold,
    squotes = squotes,
    email = email,
    url = url,
    href = href
  )
  stopifnot(is_valid_opt(out))
  out
}

is_valid_opt <- function(l){
  valid_ <- function(l){
    lapply(l, function(d){

      if (is.list(d))
        is_valid_opt(d)
      else
        is.function(d) || is.null(d)
    })
  }
  all(unlist(valid_(l)))
}

rd_opts <- function(){
  list(
    header = getOption("rdoc.header", TRUE),
    by_section = getOption("rdoc.by_section", TRUE),
    style = getOption("rdoc.style", rdoc_style()),
    text_formats = getOption("rdoc.text_formats", rdoc_text_formats())
  )
}

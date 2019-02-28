#' Pretty print R docs
#'
#' Refer to R docs in the console
#'
#'
#' @export
rd <- function(topic,
               by_section = TRUE,
               options = rdoc_options(),
               package = NULL,
               lib.loc = NULL) {
  rd_ <- Rdoc$new(topic, by_section, options, package, lib.loc)
  rd_$show()
}

rd_example <- function(topic, options = rdoc_options()) {
  rd <- Rdoc(topic, by_section, options)
  rd$show("examples")
}


#' @importFrom tools Rd2txt
#' @importFrom R6 R6Class
Rdoc <- R6Class(
  "Rdoc",
  public = list(
    topic = NULL,
    path = NULL,
    pkg = NULL,
    rd_file = NULL,
    opts = NULL,
    initialize = function(topic,
                          by_section,
                          options,
                          package,
                          lib.loc) {
      self$topic <- topic
      self$path <- help_path(self$topic) ### add in params..
      self$pkg <- basename(dirname(dirname(self$path)))
      self$opts <- options
      self$rd_file <- private$tmp_rd_(self$path)
      private$by_section = by_section
      private$rd_txt <- readLines(self$rd_file)
    },
    show = function(which = NULL){
      s <- private$list_sections(private$rd_txt)

      if (!is.null(which))
        return(private$out_(s[[which]]))
      if (!private$by_section || !interactive())
        return(private$out_(s))

      private$n_sections <- length(s)
      private$current_section <- 2L
      private$out_(s[1L:2L])

      while(private$current_section != private$n_sections){
        p <- readline("")
        if (substr(p, 1L, 1L) == "q")
          break
        private$current_section <- private$current_section + 1L
        private$out_(s[private$current_section])
      }
    }
  ),
  private = list(
    rd_txt = NULL,
    by_section = TRUE,
    n_sections = 0L,
    current_section = 0L,
    get_help_file = getFromNamespace(".getHelpFile", "utils"),
    topic_ = function(tp){
      if (is.character(tp))
        return(tp)
      deparse(substitute(tp))
    },
    tmp_rd_ = function(file){
      rd_file <- tempfile(fileext = ".rd")
      Rd2txt(
        private$get_help_file(file),
        out = rd_file,
        options = list(
          underline_titles = TRUE,
          width = getOption('width') - 3L,
          code_quote = TRUE,
          item_bullet = self$opts$item_bullet
        ))
      rd_file
    },
    list_sections = function(o) {
      headers <- which(grepl("^_\b", o))
      #on error?
      nm <- character(length(headers))
      sections <- lapply(seq_along(headers), function(i) {
        h <- headers[i]
        nm[i] <<- tolower(gsub("_\b|:", "", o[h]))
        o[h] <<- self$opts$section(o[h])
        if (i == length(headers))
          return(o[headers[i]:length(o)])
        o[h:(headers[i + 1] - 1L)]
      })
      names(sections) <- nm
      sections
    },
    out_ = function(s) cat(paste(private$append_(s), collapse = "\n")),
    append_ = function(l) Reduce(append, l)
  )
)



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
                         code = prettycode::highlight,
                         item_bullet = cli::symbol$bullet) {
  list(
    pkg_header = pkg_header,
    title = title,
    section_titles = section_titles,
    arg_params = arg_params,
    code = code,
    item_bullet = item_bullet
  )
}

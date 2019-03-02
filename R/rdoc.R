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
  doc <- Rdoc$new(topic, by_section, options, package, lib.loc)
  doc$show()
}

#' @export
rd_example <- function(topic, options = rdoc_options()) {
  doc <- Rdoc$new(topic, by_section = FALSE, options, package, lib.loc)
  doc$show("examples")
}

#' @importFrom tools Rd2txt
#' @importFrom R6 R6Class
Rdoc <- R6Class(
  "Rdoc",
  public = list(
    topic = NULL,
    path = NULL,
    pkg = NULL,
    opts = NULL,
    initialize = function(topic,
                          by_section,
                          options,
                          package,
                          lib.loc) {

      self$topic <- topic
      self$opts <- options
      private$by_section = by_section

      if (file.exists(topic) && grepl("\\.Rd?|\\.rd?", topic)){
        self$path <- normalizePath(topic)
        #check pkg?
      } else {
        self$path <- help_path(self$topic) ### add in params..
        if (!length(self$path))
          stop("topic: ", topic, " not found")
        self$pkg <- basename(dirname(dirname(self$path)))
        self$path <- private$get_help_file(self$path)
      }
      private$rd_to_text()
      invisible(self)
    },
    show = function(which = NULL){

      private$list_sections()
      private$format_sections(which)
      s <- private$rd_fmt

      if (!is.null(which) || !private$by_section || !interactive())
        return(private$out_(s))

      private$n_sections <- length(s)
      private$current_section <- 2L
      private$out_(s[1L:2L])

      while(private$current_section != private$n_sections){
        p <- readline("")
        if (tolower(substr(p, 1L, 1L)) == "q")
          break
        private$current_section <- private$current_section + 1L
        private$out_(s[private$current_section])
      }
    }
  ),
  private = list(
    rd_txt = NULL,
    rd_sections = NULL,
    rd_fmt = NULL,
    by_section = TRUE,
    n_sections = 0L,
    current_section = 0L,
    get_help_file = getFromNamespace(".getHelpFile", "utils"),
    topic_ = function(tp){
      if (is.character(tp))
        return(tp)
      deparse(substitute(tp))
    },
    rd_to_text = function(){
      tmp_ <- tempfile(fileext = ".txt")
      Rd2txt(
        self$path,
        out = tmp_,
        options = list(
          underline_titles = TRUE,
          width = getOption('width') - 3L,
          code_quote = TRUE,
          item_bullet = self$opts$item_bullet
        )
      )
      private$rd_txt <- readLines(tmp_)
      invisible(self)
    },
    list_sections = function() {
      o <- private$rd_txt
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
      private$rd_sections <- sections
      invisible(self)
    },
    out_ = function(s) cat(paste(private$append_(s), collapse = "\n")),
    append_ = function(l) Reduce(append, l)
  )
)

Rdoc$set("private", "format_sections", function(which = NULL){
  l <- private$rd_sections
  if (!is.null(which))
    l <- l[c(which)]
  fm <- lapply(seq_along(l), function(i){
    s <- l[[i]]
    if (names(l)[i] %in% c("examples", "example", "usage")){
      s[2:length(s)] <- highlight(s[2:length(s)], style = self$opts$code_style)
      return(s)
    }
    s
  })
  names(fm) <- names(l)
  private$rd_fmt <- fm
  invisible(self)
})




format_as_text <- function(l) {
  # for (i in 2:length(l)){
  #   if (!nzchar(l[i]) || !grepl("[[:alnum:]]", l[i]))
  #     next
  #
  # }
  l
}



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

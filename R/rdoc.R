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

#' @importFrom tools Rd2txt parse_Rd
#' @importFrom R6 R6Class
Rdoc <- R6Class(
  "Rdoc",
  public = list(
    topic = NULL,
    path = NULL,
    pkg = NULL,
    lib = NULL,
    opts = NULL,
    initialize = function(topic,
                          by_section = FALSE,
                          options = rdoc_options(),
                          package = NULL,
                          lib.loc = NULL) {

      self$topic <- topic
      self$opts <- options
      self$pkg <- package
      self$lib <- lib.loc
      private$by_section <- by_section
      private$find_rd_path()
      private$replace_text_formats()
      private$rd_to_text()
      invisible(self)
    },
    show = function(which = NULL){

      private$list_sections()
      private$format_code_sections()

      s <- private$rd_sections

      if (!is.null(which) || !private$by_section || !interactive())
        return(private$out_(s))

      n <- length(s)
      i <- 2L

      private$out_(s[1L:2L])

      while(i < n){
        p <- readline("")
        if (tolower(substr(p, 1L, 1L)) == "q")
          break
        i <- i + 1
        private$out_(s[i])
      }
    }
  ),
  private = list(
    rdo = NULL, #hold the Rd object
    rd_txt = NULL, #the output text
    rd_sections = NULL,
    rd_fmt = NULL,
    by_section = TRUE,
    get_help_file = getFromNamespace(".getHelpFile", "utils"),
    find_rd_path = function(package, lib.loc){
      if (file.exists(self$topic) && is_rd_file(self$topic)){
        self$path <- normalizePath(self$topic)
        private$rdo <- parse_Rd(self$path)
      } else {

        self$path <- help_path(self$topic, self$pkg, self$lib)
        if (!length(self$path))
          stop("topic: ", self$topic, " not found")

        if (length(self$path) > 1)
          private$select_path()

        if (is.null(self$pkg))
          self$pkg <- get_pkg(self$path)

        private$rdo <- private$get_help_file(self$path)
      }
      invisible(self)
    },
    #save 'output' rd text for further formatting
    rd_to_text = function(){
      tmp_ <- tempfile(fileext = ".txt")
      Rd2txt(
        private$rdo,
        out = tmp_,
        options = list(
          underline_titles = TRUE,
          width = getOption('width') - 3L,
          code_quote = TRUE, # maybe remove
          item_bullet = self$opts$item_bullet
        )
      )
      private$rd_txt <- readLines(tmp_)
      invisible(self)
    },
    list_sections = function() {

      o <- private$rd_txt
      headers <- which(grepl("^_\b", o))
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

Rdoc$set("private", "format_code_sections", function(){

  code_sections <- c("examples", "example", "usage")

  fm <- lapply(code_sections, function(d){
    if (is.null(s <- private$rd_sections[[d]]))
      return(NULL)

    #todo: partial highlighting - avoid text chunks (## ... ##)
    s[2:length(s)] <- tryCatch(
      highlight(s[2:length(s)], style = self$opts$code_style),
      error = function(e) s[2:length(s)],
      warning = function(w) s[2:length(s)]
    )

    private$rd_sections[[d]] <<- s
  })

  invisible(self)
})

Rdoc$set("private", "replace_text_formats", function(){
  private$rdo <- format_rdo(private$rdo)
  invisible(self)
})

Rdoc$set("private", "select_path", function() {
  if (interactive()) {
    id <- 1:length(self$path)
    p <- paste(id, get_pkg(self$path), sep = ": ")
    msg <-
      sprintf(
        "multiple paths found for topic: %s\n%s\n%s\n",
        self$topic,
        paste(p, collapse = "\n"),
        "enter number to select (or any key for first topic)"
      )
    cat(msg)
    selection <- readline("")
    s <- as.numeric(substr(selection, 1, 1))
    if (!s %in% id)
      s <- 1L
    self$path <- self$path[s]
  } else
    self$path <- self$path[1L]

  invisible(self)
})

get_pkg <- function(path)
  basename(dirname(dirname(path)))

is_rd_file <- function(path)
  grepl(".+\\.[R|r]d$", path)

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

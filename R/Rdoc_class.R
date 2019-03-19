#' @importFrom tools Rd2txt
#' @importFrom R6 R6Class
Rdoc <- R6Class(
  "Rdoc",
  public = list(
    topic = NULL,
    help_path = NULL,
    pkg = NULL,
    opts = NULL,
    initialize = function(topic,
                          help_path,
                          by_section = FALSE,
                          include_header = TRUE,
                          options = rdoc_options()) {

      self$topic <- topic
      self$help_path <- help_path
      self$opts <- options
      private$by_section <- by_section
      private$include_header <- include_header
      private$get_rdo()
      private$replace_text_formats()
      private$rd_to_text()
      private$reflow()
      invisible(self)
    },
    show = function(which = NULL){

      private$list_sections()
      private$format_code_sections()
      private$show_pkg_header()

      s <- private$rd_sections

      if (!is.null(which))
        s <- s[which[which %in% names(s)]]

      if (!private$by_section || !interactive())
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
    include_header = TRUE,
    get_help_file = getFromNamespace(".getHelpFile", "utils"),
    get_rdo = function(package, lib.loc){

      if (length(self$help_path) > 1)
        private$select_path()

      self$pkg <- get_pkg(self$help_path)

      private$rdo <- private$get_help_file(self$help_path)

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
          width = getOption('width'),
          code_quote = TRUE, # maybe remove
          item_bullet = self$opts$item_bullet
        )
      )
      private$rd_txt <- readLines(tmp_)
      invisible(self)
    },
    list_sections = function() {
      # maybe get headers from rdo not txt?
      o <- private$rd_txt
      headers <- which(grepl("^([[:punct:]]?)_\\b", o))
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
    reflow = function(){
      private$rd_txt <- reflow_lines(private$rd_txt, getOption('width'))
      invisible(self)
    },

    out_ = function(s) cat(private$append_(s), sep = "\n"),
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

Rdoc$set("private", "show_pkg_header", function() {
  if (!private$include_header)
    return(invisible(self))
  left_ <- if (is.null(self$pkg))
    self$topic
  else
    sprintf("%s {%s}", self$topic, self$pkg)
  cat_rule(left = left_, right = R_logo("Rdoc"))
  cat("\n")
  invisible(self)
})



get_pkg <- function(path)
  basename(dirname(dirname(path)))

is_rd_file <- function(path)
  grepl(".+\\.[R|r]d$", path)

reflow_lines <- function(x, w){

  i <- 1
  while (i < length(x)) {

    t <- !has_style(x[i]) ||
      !nzchar(x[i + 1]) ||
      !grepl("\\S", x[i + 1]) ||
      grepl("^([[:punct:]]?)_\\b", x[i + 1])

    if (t) {
      i <- i + 1
      next
    }

    olen <- nchar(strip_style(x[i]))
    nlen <- if (has_style(x[i + 1]))
      nchar(strip_style(x[i + 1]))
    else
      nchar(x[i + 1])

    open_space <- w - olen

    if (nlen <= open_space) {
      x[i] <- paste(x[i], rm_indent(x[i + 1]))
      x <- x[-(i + 1)]
    }
    i <- i + 1
  }
  x
}

rm_indent <- function(x) sub("^\\s+(\\S+)", "\\1", x)
line_spaces <- function(x)
  gregexpr("\\s", x)[[1L]]

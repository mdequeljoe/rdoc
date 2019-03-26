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
                          path,
                          options = rd_opts()) {
      self$topic <- topic
      self$path <- path
      self$opts <- options
      private$has_color <- crayon::has_color()
      private$in_term <- isatty(stdout())
      private$by_section <- !private$in_term && self$opts$by_section
      private$include_header <- self$opts$header
      private$get_rdo()
      private$replace_text_formats()
      private$rd_to_text()
      private$reflow()
      invisible(self)
    },
    show = function(which = NULL){

      private$list_sections()
      private$format_code_sections()
      s <- private$rd_sections
      s <- append(private$pkg_header(), s)

      if (private$in_term){
        less_ <- Sys.getenv("LESS")
        Sys.setenv(LESS = "-R")
        on.exit(Sys.setenv(LESS = less_))
        tf <- tempfile(fileext = ".Rtxt")
        conn <- file(tf, open = "w", encoding = "native.enc")
        s <- enc2utf8(private$append_(s))
        writeLines(s, con = conn, useBytes = TRUE)
        close(conn)
        file.show(tf)
        return(invisible(self))
      }

      if (!is.null(which)){
        s <- s[which[which %in% names(s)]]
        s <- append(private$pkg_header(), s)
      }

      if (!private$by_section || !interactive())
        return(private$out_(s))

      n <- length(s)
      i <- 3L
      private$out_(s[1L:i])
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
    has_color = NULL,
    in_term = NULL,
    rdo = NULL, #hold the Rd object
    rd_txt = NULL, #the output text
    rd_sections = NULL,
    rd_fmt = NULL,
    by_section = TRUE,
    include_header = TRUE,
    code_sections = c("examples", "example", "usage"),
    get_help_file = getFromNamespace(".getHelpFile", "utils"),
    get_rdo = function(){

      if (length(self$path) > 1)
        private$select_path()

      self$pkg <- get_pkg(self$path)
      private$rdo <- private$get_help_file(self$path)
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

      o <- private$rd_txt
      h <- id_headers(o)
      section_names <- as_title(o[h])
      if (private$has_color)
        o[h] <- self$opts$style$section_titles(section_names)
      section_ends <- c(h[-1] - 1, length(o))
      sections <- lapply(seq_along(h), function(i) {
        o[h[i]:section_ends[i]]
      })
      names(sections) <- tolower(section_names)
      private$rd_sections <- sections
      invisible(self)
    },
    reflow = function(){
      private$rd_txt <- reflow_lines(private$rd_txt, getOption('width'))
      invisible(self)
    },

    out_ = function(s, file = "") cat(private$append_(s), file = file, sep = "\n"),
    append_ = function(l) Reduce(append, l)
  )
)

Rdoc$set("private", "format_code_sections", function(){

  if (!private$has_color)
    return(invisible(self))

  cs <- private$code_sections
  fm <- lapply(cs, function(d){
    if (is.null(s <- private$rd_sections[[d]]))
      return(NULL)

    #todo: partial highlighting - avoid text chunks (## ... ##)
    s[2:length(s)] <- tryCatch(
      highlight(s[2:length(s)], style = self$opts$style$code_style),
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

Rdoc$set("private", "pkg_header", function() {
  if (!private$include_header)
    return(character(0))
  left_ <- if (is.null(self$pkg))
    self$topic
  else
    sprintf("%s {%s}", self$topic, self$pkg)
  c(cli::rule(left = left_, right = R_logo("Rdoc"))[], "")
})

get_pkg <- function(path)
  basename(dirname(dirname(path)))

is_rd_file <- function(path)
  grepl(".+\\.[R|r]d$", path)

id_headers <- function(rdtxt){
  which(grepl("^([[:punct:]]?)_\\b", rdtxt))
}

as_title <- function(h){
  gsub("_\b|:", "", h)
}

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

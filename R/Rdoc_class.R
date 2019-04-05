#' @importFrom tools Rd2txt
#' @importFrom R6 R6Class
Rdoc <- R6Class(
  "Rdoc",
  public = list(
    topic = NULL,
    path = NULL,
    pkg = NULL,
    style = NULL,
    text_formats = NULL,
    item_bullet = NULL,
    which = NULL,
    initialize = function(topic,
                          path,
                          which = NULL,
                          options = rd_opts()) {
      self$topic <- topic
      self$path <- path
      self$which <- which
      self$style <- set_styles(options$style)
      self$text_formats <- options$text_formats
      self$item_bullet <- options$item_bullet
      private$has_color <- crayon::has_color()
      private$in_term <- isatty(stdout())
      private$by_section <- !private$in_term &&
        options$by_section &&
        interactive()
      private$include_header <- options$header
      private$get_rdo()
      private$replace_text_formats()
      private$rd_to_text()
      invisible(self)
    },
    show = function(){

      private$list_sections()
      private$format_sections()
      private$reflow_sections()
      private$replace_tables()
      private$format_code_sections()

      s <- c(private$pkg_header(), private$rd_sections)

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

      if (!private$by_section)
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
      invisible(self)
    }
  ),
  private = list(
    has_color = NULL,
    in_term = NULL,
    rdo = NULL, #hold the Rd object
    tables = NULL, #formatted tables
    rd_txt = NULL, #the output text
    rd_sections = NULL,
    rd_fmt = NULL,
    by_section = TRUE,
    include_header = TRUE,
    code_sections = c("examples", "example", "usage"),
    get_help_file = getFromNamespace(".getHelpFile", "utils"),
    out_ = function(s, file = "") cat(private$append_(s), file = file, sep = "\n"),
    append_ = function(l) Reduce(append, l)
  )
)

Rdoc$set("private", "get_rdo", function(){
  if (length(self$path) > 1)
    private$select_path()
  self$pkg <- get_pkg(self$path)
  private$rdo <- private$get_help_file(self$path)
  invisible(NULL)
})

Rdoc$set("private", "rd_to_text", function(){
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
  invisible(NULL)
})


Rdoc$set("private", "list_sections", function(){
  o <- private$rd_txt
  h <- id_headers(o)
  o[h] <- as_title(o[h])
  nms <- tolower(o[h])

  section_ends <- c(h[-1] - 1, length(o))
  sections <- lapply(seq_along(h), function(i) {
    o[h[i]:section_ends[i]]
  })
  names(sections) <- nms

  if (isTRUE(self$which %in% nms))
    sections <- sections[self$which]

  private$rd_sections <- sections
  invisible(NULL)
})

Rdoc$set("private", "format_sections", function() {
  if (!private$has_color)
    return(invisible(NULL))

  o <- private$rd_sections
  i <- 1
  if (length(o) > 1) {
    o[[1]][1] <- self$style$title(o[[1]][1])
    i <- 2
  }

  o[i:length(o)] <- lapply(o[i:length(o)], function(d) {
    d[1] <- self$style$section_titles(d[1])
    d
  })

  if (length(o$arguments))
    o$arguments <- format_args(o$arguments, self$style$arguments)

  private$rd_sections <- o
  invisible(NULL)
})

Rdoc$set("private", "reflow_sections", function(){
  if (!private$has_color)
    return(invisible(NULL))
  txt <- !names(private$rd_sections) %in% private$code_sections
  if (length(txt))
    private$rd_sections[txt] <-
      lapply(private$rd_sections[txt], reflow_lines, exclude = "##>>RDOC_")
  invisible(NULL)
})

Rdoc$set("private", "replace_tables", function() {
  if (!length(private$tables))
    return(invisible(NULL))
  private$rd_sections <- lapply(private$rd_sections, function(d) {
    tb <- grepl("##>>RDOC_TABLE", d)
    d[tb] <- vapply(d[tb], function(x) {
      id <- gsub(".+_([0-9]+)$", "\\1", x)
      private$tables[[as.numeric(id)]]
    }, character(1))
    d
  })
  invisible(NULL)
})

Rdoc$set("private", "format_code_sections", function(){

  if (!private$has_color)
    return(invisible(NULL))

  fm <- lapply(private$code_sections, function(d){

    if (is.null(s <- private$rd_sections[[d]]))
      return(NULL)

    #todo: partial highlighting - avoid text chunks (## ... ##)
    rng <- 2:length(s)
    s[rng] <- tryCatch(
      prettycode::highlight(s[rng], style = self$style$code_style),
      error = function(e) s[rng],
      warning = function(w) s[rng]
    )
    private$rd_sections[[d]] <<- s
  })
  invisible(NULL)
})

Rdoc$set("private", "replace_text_formats", function(){
  if (private$has_color){
    x <- format_rdo(private$rdo, self$text_formats)
    private$rdo <- x$rdo
    private$tables <- x$tables
  }
  invisible(NULL)
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
    s <- substr(selection, 1, 1)
    if (!s %in% id)
      s <- 1L
    self$path <- self$path[s]
  } else
    self$path <- self$path[1L]

  invisible(NULL)
})

Rdoc$set("private", "pkg_header", function() {
  if (!private$include_header)
    return(character(0))
  left_ <- if (is.null(self$pkg))
    self$topic
  else
    sprintf("%s {%s}", self$topic, self$pkg)
  c(cli::rule(
    left = left_,
    right = self$text_formats$r_logo("rdoc"),
    width = getOption('width')
  )[],
  "")
})


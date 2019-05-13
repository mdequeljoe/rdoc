
Rdoc <- R6::R6Class(
  "Rdoc",
  public = list(
    topic = NULL,
    path = NULL,
    pkg = NULL,
    style = NULL,
    text_formats = NULL,
    item_bullet = NULL,
    which_section = NULL,
    rd_sections = NULL,
    width = NULL,
    initialize = function(topic,
                          path,
                          which_section = NULL,
                          opts = rd_opts()) {
      self$topic <- topic
      self$path <- path
      self$which_section <- which_section
      self$style <- set_styles(opts$style)
      self$text_formats <- opts$text_formats
      self$item_bullet <- opts$item_bullet
      self$width <- opts$width
      private$has_color <- crayon::has_color()
      private$in_term <- isatty(stdout())
      private$by_section <- !private$in_term &&
        opts$by_section &&
        interactive()
      private$include_header <- opts$header
      private$get_rdo()
      private$format_rdo()
      private$rd_to_text()
      invisible(self)
    },
    show = function(){

      self$set_rd_sections()

      if (private$in_term){
        private$show_file(self$rd_sections)
        return(invisible(NULL))
      }

      private$flow_by_section(self$rd_sections)
      invisible(NULL)
    }
  ),
  private = list(
    has_color = NULL,
    in_term = NULL,
    rdo = NULL,
    tables = NULL,
    rd_txt = NULL,
    rd_fmt = NULL,
    by_section = TRUE,
    include_header = TRUE,
    code_sections = c("examples", "example", "usage"),
    get_help_file = getFromNamespace(".getHelpFile", "utils")
  )
)

Rdoc$set("public", "set_rd_sections", function() {
  if (!is.null(self$rd_sections))
    return(invisible(self))
  private$list_sections()
  private$format_sections()
  private$reflow_sections()
  private$replace_tables()
  private$format_code_sections()
  private$set_pkg_header()
  invisible(self)
})

Rdoc$set("public", "rdoc_text", function() {
  self$set_rd_sections()
  append_list(self$rd_sections)
})

Rdoc$set("private", "show_file", function(s) {
  show_file(append_list(s))
  invisible(NULL)
})

Rdoc$set("private", "flow_by_section", function(s) {
  if (!private$by_section){
    send_out(s)
    return(invisible(NULL))
  }
  i <- 3L
  send_out(s[1L:i])
  while (i < length(s)) {
    l <- readline("")
    if (l != "")
      break
    i <- i + 1
    send_out(s[i])
  }
  invisible(NULL)
})

Rdoc$set("private", "get_rdo", function(){
  if (length(self$path) > 1)
    private$select_path()

  if (is_rd_file(self$path)){
    self$pkg <- self$path
    if (nchar(self$pkg) >= (self$width - 5L))
      self$pkg <- basename(self$pkg)
    private$rdo <- tools::parse_Rd(self$path)
    return(invisible(NULL))
  }

  self$pkg <- get_pkg(self$path)
  private$rdo <- private$get_help_file(self$path)
  invisible(NULL)
})

Rdoc$set("private", "rd_to_text", function(){
  tmp_ <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp_))
  tools::Rd2txt(
    private$rdo,
    out = tmp_,
    options = list(
      underline_titles = TRUE,
      width = self$width,
      code_quote = TRUE,
      itemBullet = self$item_bullet
    )
  )
  private$rd_txt <- readLines(tmp_)
  invisible(NULL)
})

Rdoc$set("private", "list_sections", function(){
  o <- set_rd_title(private$rd_txt)
  h <- id_headers(o)
  o[h] <- set_section_title(o[h])

  rng <- Map(`:`, h, c(h[-1L] - 1L, length(o)))
  sections <- lapply(rng, function(d) o[d] )
  names(sections) <- c("title", tolower(o[h[-1L]]))

  if (isTRUE(self$which_section %in% names(sections)))
    sections <- sections[self$which_section]

  self$rd_sections <- sections
  invisible(NULL)
})

#nocov start
Rdoc$set("private", "format_sections", function() {
  if (!private$has_color)
    return(invisible(NULL))

  o <- self$rd_sections
  if (!is.null(o$title))
    o$title[1] <- self$style$title(o$title[1])

  k <- !names(o) %in% "title"
  o[k] <- lapply(o[k], function(d) {
    d[1] <- self$style$section_titles(d[1])
    d
  })

  if (length(o$arguments))
    o$arguments <- format_args(o$arguments, self$style$arguments)

  self$rd_sections <- o
  invisible(NULL)
})

Rdoc$set("private", "reflow_sections", function(){
  if (!private$has_color)
    return(invisible(NULL))
  txt <- !names(self$rd_sections) %in% private$code_sections
  if (length(txt))
    self$rd_sections[txt] <-
      lapply(
        self$rd_sections[txt],
        reflow_lines,
        exclude = "##>>RDOC_|[0-9+]\\.")
  invisible(NULL)
})

Rdoc$set("private", "replace_tables", function() {
  if (!length(private$tables))
    return(invisible(NULL))
  self$rd_sections <- lapply(self$rd_sections, function(d) {
    tb <- grepl(table_id(), d)
    d[tb] <- vapply(d[tb], function(x) {
      private$tables[[trimws(x)]]
    }, character(1))
    d
  })
  invisible(NULL)
})

#todo: partial highlighting - avoid text chunks (## ... ##)
Rdoc$set("private", "format_code_sections", function(){

  if (!private$has_color)
    return(invisible(NULL))

  fm <- lapply(private$code_sections, function(d){

    if (is.null(s <- self$rd_sections[[d]]))
      return(NULL)

    rng <- 2:length(s)
    s[rng] <- tryCatch(
      prettycode::highlight(s[rng], style = self$style$code_style),
      error = function(e) s[rng],
      warning = function(w) s[rng]
    )
    self$rd_sections[[d]] <<- s
  })
  invisible(NULL)
})

Rdoc$set("private", "select_path", function() {
  self$path <- select_path(self$path, self$topic)
  invisible(NULL)
})
#nocov end

Rdoc$set("private", "format_rdo", function() {
  x <- format_rdo(private$rdo, self$text_formats)
  private$rdo <- x$rdo
  private$tables <- x$tables
  invisible(NULL)
})

Rdoc$set("private", "set_pkg_header", function() {
  if (!private$include_header)
    return(invisible(NULL))
  left_ <- if (is.null(self$pkg))
    self$topic
  else
    sprintf("%s {%s}", self$topic, self$pkg)
  h <- cli::rule(
    left = left_,
    right = self$text_formats$r_logo("rdoc"),
    width = self$width
  )[]
  self$rd_sections <- c(h, " ", self$rd_sections)
  return(invisible(NULL))
})

show_file <- function(s){
  less_ <- Sys.getenv("LESS")
  Sys.setenv(LESS = "-R")
  tf <- tempfile(fileext = ".Rtxt")
  on.exit({
    Sys.setenv(LESS = less_)
    unlink(tf)
  })
  conn <- file(tf, open = "w", encoding = "native.enc")
  s <- enc2utf8(s)
  writeLines(s, con = conn, useBytes = TRUE)
  close(conn)
  file.show(tf)
}

select_path <- function(path, topic){
  s <- 1L
  if (interactive()) {
    id <- 1:length(path)
    p <- paste(id, get_pkg(path), sep = ": ")
    msg <-
      sprintf(
        "multiple paths found for topic: %s\n%s\n%s\n",
        topic,
        paste(p, collapse = "\n"),
        "enter number to select (or any key for first topic)"
      )
    cat(msg)
    selection <- readline("")
    sl <- substr(selection, 1, 1)
    if (sl %in% id)
      s <- as.integer(sl)
  }
  path[s]
}



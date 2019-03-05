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
    opts = NULL,
    rd_tmp = NULL,
    initialize = function(topic,
                          by_section,
                          options,
                          package,
                          lib.loc) {

      self$topic <- topic
      self$opts <- options
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
    orig_txt = NULL,
    orig_txt_dp = NULL,
    rd_txt = NULL,
    rd_sections = NULL,
    rd_fmt = NULL,
    by_section = TRUE,
    get_help_file = getFromNamespace(".getHelpFile", "utils"),
    find_rd_path = function(){
      if (file.exists(self$topic) &&
          grepl("\\.Rd?|\\.rd?", self$topic)){
        self$path <- normalizePath(self$topic)
      } else {
        self$path <- help_path(self$topic) ### add in params..
        if (!length(self$path))
          stop("topic: ", self$topic, " not found")
        self$pkg <- basename(dirname(dirname(self$path)))
        self$path <- private$get_help_file(self$path)
      }

      private$rd_orig_text()
      invisible(self)
    },
    rd_orig_text = function(){
      if (inherits(self$path, "Rd")){

        private$orig_txt <- as.character(self$path)
        private$orig_txt_dp <- as.character(self$path, deparse = TRUE)
      } else {
        prd <- parse_Rd(self$path)
        private$orig_txt <- as.character(prd)
        private$orig_txt_dp <- as.character(prd, deparse = TRUE)
      }
      private$orig_txt <-
        replace_encoding(private$orig_txt, private$orig_txt_dp)

      invisible(self)
    },
    rd_to_text = function(){
      tmp_ <- tempfile(fileext = ".txt")
      Rd2txt(
        self$rd_tmp,
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

Rdoc$set("private", "format_code_sections", function(){

  code_sections <- c("examples", "example", "usage")

  fm <- lapply(code_sections, function(d){
    if (is.null(s <- private$rd_sections[[d]]))
      return(NULL)

    s[2:length(s)] <-
      highlight(s[2:length(s)], style = self$opts$code_style)

    private$rd_sections[[d]] <<- s
  })

  invisible(self)
})

Rdoc$set("private", "replace_text_formats", function(){

  #private$check_italic()
  #private$check_bold()
  # private$check_code()
  # private$check_squotes()
  self$rd_tmp <- tempfile(fileext = ".Rd")
  cat(private$orig_txt, "\n", sep = "", file = self$rd_tmp)

  invisible(self)
})

Rdoc$set("private", "check_italic", function(){

  m <- which(private$orig_txt == "\\emph")
  if (length(m))
    private$orig_txt <-
      replace_italic(private$orig_txt, m)

  invisible(self)
})

Rdoc$set("private", "check_bold", function(){

  m <- which(private$orig_txt == "\\bold")
  if (length(m))
    private$orig_txt <-
      replace_bold(private$orig_txt, m)

  invisible(self)
})

replace_sym <- function(sym, replace_open, replace_close){
  function(v, match_sym){

    rm_id <- integer(length(v))

    for (i in match_sym){

      v[i] <- replace_open
      rm_id[i + 1] <- 1L
      open <- 2L
      j <- i + 2
      found_close <- FALSE
      while(!found_close){

        if (open %% 2 == 0 && v[j] == "}"){
          v[j] <- replace_close
          found_close <- TRUE
        }
        if (v[j] %in% c("{", "}"))
          open <- open + 1

        j <- j + 1
      }
    }
    v[-which(rm_id == 1L)]
  }
}

replace_italic <- replace_sym("\\emph", "\033[3m", "\033[23m")
replace_bold <- replace_sym("\\bold", "\033[1m", "\033[22m")
replace_encoding <- function(k, d){
  elines <- which(k != d)
  if (!length(elines))
    return(k)
  elines <- elines[grepl("\\\\n|\\\\t|\\\\v|\\\\r|%", k[elines])]
  k[elines] <- d[elines]
  k
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
